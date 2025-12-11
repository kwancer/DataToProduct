# R/model_helpers.R --------------------------------------------------
# Helpers for soil moisture model + prediction pipeline

library(jsonlite)
library(dplyr)
library(lubridate)
library(slider)
library(tibble)
library(mgcv)

# --------------------------------------------------------------------
# SAFE NULL COALESCE
# --------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --------------------------------------------------------------------
# LOAD TRAINED BAM MODEL
# --------------------------------------------------------------------
bam_mod <- tryCatch(
  {
    m <- readRDS("soil_moisture_bam_model.rds")
    cat("Loaded BAM soil moisture model.\n")
    m
  },
  error = function(e) {
    warning("Failed to load BAM soil moisture model: ", conditionMessage(e))
    NULL
  }
)

# --------------------------------------------------------------------
# CONVERT WATERING MINUTES → MM OF WATER
# 1 L over 1 m² = 1 mm
# --------------------------------------------------------------------
watering_minutes_to_mm <- function(minutes, flow_lpm, area_m2) {
  # minutes: numeric vector (length 7 typically)
  # flow_lpm: L/min
  # area_m2: m²
  if (is.null(minutes) || length(minutes) == 0) {
    return(rep(0, 7))
  }
  minutes <- as.numeric(minutes)
  mm <- (flow_lpm * minutes) / area_m2
  mm
}

# --------------------------------------------------------------------
# ADD WATERING MM TO DAILY RAIN
# df_daily must have columns: date, rain_mm
# watering_mins must be length 7, mapped to last 7 days (ending today)
# --------------------------------------------------------------------
add_watering_to_daily <- function(df_daily,
                                  watering_mins,
                                  flow_lpm,
                                  area_m2,
                                  timezone = "Europe/London") {
  if (is.null(watering_mins)) {
    df_daily$watering_mm    <- 0
    df_daily$effective_rain <- df_daily$rain_mm
    return(df_daily)
  }
  
  # assume watering history corresponds to last 7 days:
  # Sys.Date()-6, ..., Sys.Date()
  today <- as.Date(format(Sys.time(), tz = timezone))
  dates_hist <- seq(today - 6, today, by = "1 day")
  
  w_mm <- watering_minutes_to_mm(watering_mins, flow_lpm, area_m2)
  
  watering_df <- tibble(
    date        = dates_hist,
    watering_mm = w_mm
  )
  
  df_daily %>%
    left_join(watering_df, by = "date") %>%
    mutate(
      watering_mm    = ifelse(is.na(watering_mm), 0, watering_mm),
      effective_rain = rain_mm + watering_mm
    )
}

# --------------------------------------------------------------------
# BUILD FEATURES FOR BAM MODEL (MATCH TRAINING, WITH rain_14d FALLBACK)
# df_daily must have: date, effective_rain, max_temp, humidity,
#                     wind_speed, sm_api
# --------------------------------------------------------------------
build_bam_features <- function(df_daily) {
  df <- df_daily %>%
    arrange(date) %>%
    mutate(
      # rolling windows based on effective_rain and max_temp
      rain_3d      = slide_dbl(effective_rain, sum,  .before = 2,  .complete = TRUE),
      rain_7d      = slide_dbl(effective_rain, sum,  .before = 6,  .complete = TRUE),
      rain_14d_raw = slide_dbl(effective_rain, sum,  .before = 13, .complete = TRUE),
      temp_3d      = slide_dbl(max_temp,      mean, .before = 2,  .complete = TRUE),
      doy          = yday(date)
    ) %>%
    # if we don't actually have 14 full days, fall back to rain_7d
    mutate(
      rain_14d = ifelse(is.na(rain_14d_raw), rain_7d, rain_14d_raw),
      sm_lag1  = dplyr::lag(sm_api, 1)
    ) %>%
    filter(
      !is.na(rain_3d),
      !is.na(rain_7d),
      !is.na(rain_14d),
      !is.na(temp_3d),
      !is.na(sm_lag1)
    )
  
  df
}

# --------------------------------------------------------------------
# LOOK UP POSTCODE → LAT/LON (postcodes.io) FOR MODEL USE
# --------------------------------------------------------------------
get_postcode_coords_model <- function(postcode) {
  pc_clean <- gsub(" ", "", postcode)
  url <- paste0("https://api.postcodes.io/postcodes/", pc_clean)
  
  geo <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (is.null(geo) || geo$status != 200) return(NULL)
  
  list(
    lat = geo$result$latitude,
    lon = geo$result$longitude
  )
}

# --------------------------------------------------------------------
# ENHANCED WEATHER FORECAST WITH EVAPORATION FACTORS
# --------------------------------------------------------------------

weather_forecast <- reactive({
  req(input$postcode)
  
  coords <- get_postcode_coords(input$postcode)
  if (is.null(coords)) return(NULL)
  
  # Enhanced API call with more parameters
  w_url <- paste0(
    "https://api.open-meteo.com/v1/forecast?",
    "latitude=", coords$lat,
    "&longitude=", coords$lon,
    "&daily=weathercode,temperature_2m_max,temperature_2m_min,",
    "precipitation_sum,precipitation_hours,",
    "windspeed_10m_max,winddirection_10m_dominant,",
    "shortwave_radiation_sum",  # Solar radiation - KEY for evaporation!
    "&timezone=auto"
  )
  
  w_json <- tryCatch(jsonlite::fromJSON(w_url), error = function(e) NULL)
  if (is.null(w_json) || is.null(w_json$daily)) return(NULL)
  
  days    <- w_json$daily$time
  tmin    <- w_json$daily$temperature_2m_min
  tmax    <- w_json$daily$temperature_2m_max
  code    <- w_json$daily$weathercode
  rain_mm <- w_json$daily$precipitation_sum
  precip_hours <- w_json$daily$precipitation_hours %||% rep(NA, length(days))
  wind_max <- w_json$daily$windspeed_10m_max %||% rep(NA, length(days))
  radiation <- w_json$daily$shortwave_radiation_sum %||% rep(NA, length(days))
  
  decoded <- lapply(code, weather_lookup)
  icons   <- sapply(decoded, function(x) x[1])
  cond    <- sapply(decoded, function(x) x[2])
  
  data.frame(
    day            = format(as.Date(days), "%a %d %b"),
    icon           = icons,
    condition      = cond,
    tmin           = tmin,
    tmax           = tmax,
    code           = code,
    rain_mm        = rain_mm,
    precip_hours   = precip_hours,  # Hours of precipitation
    wind_max_kmh   = wind_max * 3.6,  # Convert m/s to km/h
    radiation_mj    = radiation,      # Solar radiation in MJ/m²
    stringsAsFactors = FALSE
  )
})



# --------------------------------------------------------------------
# FETCH OPEN-METEO HOURLY DATA (PAST + FORECAST)
# --------------------------------------------------------------------
fetch_hourly_weather_sm <- function(lat,
                                    lon,
                                    past_days     = 21,
                                    forecast_days = 7,
                                    timezone      = "Europe/London") {
  url_weather <- paste0(
    "https://api.open-meteo.com/v1/forecast?",
    "latitude=", lat,
    "&longitude=", lon,
    "&hourly=",
    "temperature_2m,",
    "relative_humidity_2m,",
    "wind_speed_10m,",
    "rain,",
    "soil_moisture_9_to_27cm",
    "&past_days=", past_days,
    "&forecast_days=", forecast_days,
    "&timezone=", URLencode(timezone)
  )
  
  raw <- jsonlite::fromJSON(url_weather)
  
  df_hourly <- tibble(
    datetime = ymd_hm(raw$hourly$time),
    temp     = raw$hourly$temperature_2m,
    humidity = raw$hourly$relative_humidity_2m,
    wind     = raw$hourly$wind_speed_10m,
    rain     = raw$hourly$rain,
    sm_1cm   = raw$hourly$soil_moisture_9_to_27cm
  )
  
  df_hourly
}

# --------------------------------------------------------------------
# AGGREGATE HOURLY → DAILY AND DROP FUTURE DATES
# --------------------------------------------------------------------
hourly_to_daily_sm <- function(df_hourly, timezone = "Europe/London") {
  df_daily <- df_hourly %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(
      rain_mm    = sum(rain, na.rm = TRUE),
      max_temp   = max(temp, na.rm = TRUE),
      humidity   = max(humidity, na.rm = TRUE),
      wind_speed = max(wind, na.rm = TRUE),
      sm_api     = mean(sm_1cm, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    arrange(date)
  
  today <- as.Date(format(Sys.time(), tz = timezone))
  
  df_daily %>%
    filter(date <= today)
}

# --------------------------------------------------------------------
# MAIN ENTRY: PREDICT SOIL MOISTURE TODAY (MODEL + API COMPARISON)
# Returns tibble: date, sm_model, sm_api, diff
# --------------------------------------------------------------------
predict_soil_moisture_today <- function(postcode,
                                        watering_mins,
                                        flow_lpm,
                                        garden_area_m2,
                                        past_days     = 21,
                                        forecast_days = 7,
                                        timezone      = "Europe/London",
                                        verbose       = FALSE) {
  if (is.null(bam_mod)) {
    stop("BAM model not loaded.")
  }
  
  # Ensure enough history for 14-day rolling windows
  if (past_days < 21) past_days <- 21
  
  # 1. Geocode postcode
  coords <- get_postcode_coords_model(postcode)
  if (is.null(coords)) {
    stop("Postcode lookup failed.")
  }
  if (verbose) {
    cat("Postcode:", postcode, "→ Lat:", coords$lat,
        "Lon:", coords$lon, "\n")
  }
  
  # 2. Fetch hourly weather + soil moisture
  df_hourly <- fetch_hourly_weather_sm(
    lat           = coords$lat,
    lon           = coords$lon,
    past_days     = past_days,
    forecast_days = forecast_days,
    timezone      = timezone
  )
  
  if (verbose) {
    cat("Hourly data sample:\n")
    print(head(df_hourly, 10))
  }
  
  # 3. Aggregate to daily and drop future
  df_daily <- hourly_to_daily_sm(df_hourly, timezone = timezone)
  
  if (verbose) {
    cat("\nDaily summary (after dropping future rows):\n")
    print(df_daily)
  }
  
  # 4. Add watering (mm) into effective_rain
  df_daily2 <- add_watering_to_daily(
    df_daily      = df_daily,
    watering_mins = watering_mins,
    flow_lpm      = flow_lpm,
    area_m2       = garden_area_m2,
    timezone      = timezone
  )
  
  if (verbose) {
    cat("\nDaily + watering + effective rain:\n")
    print(df_daily2)
  }
  
  # 5. Build BAM features
  df_feat <- build_bam_features(df_daily2)
  if (nrow(df_feat) == 0) {
    stop("Not enough data to build BAM features (after rolling windows).")
  }
  
  # 6. Determine 'today' based on feature dates
  today     <- max(df_feat$date)
  yesterday <- today - 1  # kept in case you want it later
  
  # 7. Extract today's feature row
  newdata_today <- df_feat %>%
    filter(date == today) %>%
    select(
      rain_3d,
      rain_7d,
      rain_14d,
      temp_3d,
      humidity,
      wind_speed,
      sm_lag1,
      doy
    )
  
  if (nrow(newdata_today) != 1) {
    stop("Could not extract features for today.")
  }
  
  # 8. Predict with BAM
  sm_predicted <- as.numeric(
    predict(bam_mod, newdata = newdata_today, type = "response")
  )
  
  # 9. API soil moisture today (from df_daily2)
  sm_api_today <- df_daily2 %>%
    filter(date == today) %>%
    pull(sm_api)
  
  if (length(sm_api_today) == 0) sm_api_today <- NA_real_
  
  # 10. Return tibble with comparison
  tibble(
    date     = today,
    sm_model = sm_predicted,
    sm_api   = sm_api_today,
    diff     = sm_predicted - sm_api_today
  )
}

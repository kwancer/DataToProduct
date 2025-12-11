# HEre we use the trained BAM model to predict soil moisture for a given postcode
library(jsonlite)
library(dplyr)
library(lubridate)
library(slider)
library(mgcv)
library(httr)
library(tibble)


# LOAD TRAINED MODEL
bam_mod <- readRDS("soil_moisture_bam_model.rds")
cat("Loaded BAM model.\n")


#  DEFINE POSTCODE + GET LAT/LON (postcodes.io)
postcode <- "CO30AE"

url_geo <- paste0(
  "https://api.postcodes.io/postcodes/",
  gsub(" ", "", postcode)
)

geo <- fromJSON(url_geo)

if (geo$status != 200) {
  stop("Postcode lookup failed: ", geo$error)
}

lat <- geo$result$latitude
lon <- geo$result$longitude

cat("Postcode:", postcode, "→ Lat:", lat, " Lon:", lon, "\n\n")


# OPEN-METEO REQUEST FOR HOURLY DATA (PAST 7 DAYS + FORECAST)
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
  "&past_days=7",
  "&forecast_days=7",
  "&timezone=Europe/London"
)

cat("Requesting Open-Meteo:\n", url_weather, "\n\n")

raw <- fromJSON(url_weather)

# CONVERT HOURLY WITH TIBBLE
df_hourly <- tibble(
  datetime = ymd_hm(raw$hourly$time),
  temp     = raw$hourly$temperature_2m,
  humidity = raw$hourly$relative_humidity_2m,
  wind     = raw$hourly$wind_speed_10m,
  rain     = raw$hourly$rain,
  sm_1cm   = raw$hourly$soil_moisture_9_to_27cm
)

cat("Hourly data sample:\n")
print(head(df_hourly, 10))


# AGGREGATE HOURLY TO DAILY, THEN DROP FUTURE DATES
df_daily <- df_hourly %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    rain_mm = sum(rain, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    humidity = max(humidity, na.rm = TRUE),
    wind_speed = max(wind, na.rm = TRUE),
    sm_api = mean(sm_1cm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

cat("\nDaily summary (before dropping future rows):\n")
print(df_daily)

# Define "today" in Europe/London and keep only past + today
today <- as.Date(format(Sys.time(), tz = "Europe/London"))

df_daily <- df_daily %>%
  filter(date <= today)

cat("\nDaily summary (after dropping future rows):\n")
print(df_daily)


# 5. ADD WATERING OF 10 mm OVER LAST 7 DAYS (UP TO YESTERDAY)
yesterday <- today - 1

# Last 7 days ending yesterday: today-7+1 .. yesterday
dates_last7 <- seq(from = today - 7 + 1, to = yesterday, by = "1 day")

watering_df <- tibble(
  date = dates_last7,
  watering_mm = 10 / 7 # evenly distribute 10 mm over those 7 days
)


cat("\nWatering (mm) for last 7 days (up to yesterday):\n")
print(watering_df)

# Merge watering into df_daily, default 0 where no watering
df_daily <- df_daily %>%
  left_join(watering_df, by = "date") %>%
  mutate(
    watering_mm = ifelse(is.na(watering_mm), 0, watering_mm),
    effective_rain = rain_mm + watering_mm
  )

cat("\nDaily + watering + effective rain (no future dates):\n")
print(df_daily)


# FEATURE ENGINEERING (MATCHING BAM MODEL)
df_feat <- df_daily %>%
  mutate(
    rain_3d = slide_dbl(effective_rain, sum, .before = 2, .complete = TRUE),
    rain_7d = slide_dbl(effective_rain, sum, .before = 6, .complete = TRUE),
    temp_3d = slide_dbl(max_temp, mean, .before = 2, .complete = TRUE),
    doy     = yday(date)
  ) %>%
  filter(!is.na(rain_3d), !is.na(rain_7d), !is.na(temp_3d))

cat("\nFeature-engineered data:\n")
print(df_feat)


# NEWDATA FOR TODAY (sm_lag1 = YESTERDAY'S API SOIL MOISTURE)
today <- max(df_feat$date) # in case earliest windows drop first few days
yesterday <- today - 1

sm_lag1_today <- df_feat %>%
  filter(date == yesterday) %>%
  pull(sm_api)

if (length(sm_lag1_today) == 0 || is.na(sm_lag1_today)) {
  stop("Missing sm_lag1 for today (no sm_api for yesterday).")
}

newdata_today <- df_feat %>%
  filter(date == today) %>%
  mutate(sm_lag1 = sm_lag1_today) %>%
  select(rain_3d, rain_7d, temp_3d, humidity, wind_speed, sm_lag1, doy)

cat("\nNewdata for model prediction (today):\n")
print(newdata_today)


# MODEL PREDICTION FOR TODAY
sm_predicted <- as.numeric(predict(bam_mod, newdata = newdata_today))
cat("\nPredicted soil moisture TODAY (model): ", sm_predicted, "\n")


# API SOIL MOISTURE TODAY (FROM FILTERED df_daily)
sm_api_today <- df_daily %>%
  filter(date == today) %>%
  pull(sm_api)

cat("API soil moisture TODAY: ", sm_api_today, "\n")

# COMPARISON
comparison <- tibble(
  date     = today,
  sm_model = sm_predicted,
  sm_api   = sm_api_today,
  diff     = sm_predicted - sm_api_today
)

cat("\nMODEL vs API (TODAY):\n")
print(comparison)

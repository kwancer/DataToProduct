# In this file we merge daily weather data with soil moisture raster data (SMUK).

library(dplyr)
library(readr)
library(terra)
library(lubridate)
library(stringr)
library(tibble)

# LOAD WEATHER + FILTER TO SMUK DATE RANGE
weather <- read_csv("weather_uk_cleaned.csv")

weather <- weather %>%
  mutate(date = as.Date(date)) %>%
  filter(
    date >= as.Date("2016-04-22"),
    date <= as.Date("2023-12-31")
  )

cat("Weather rows after date filtering:", nrow(weather), "\n")

# LIST ALL SMUK RASTER FILES
sm_dir <- "soil_moisture/data/"

sm_files <- list.files(
  sm_dir,
  pattern = "^dt_smuk_.*\\.tif$",
  full.names = TRUE
)

cat("Found", length(sm_files), "SMUK raster files.\n")

# EXTRACT DATE FROM RASTER FILENAMES
extract_date <- function(path) {
  as.Date(str_extract(path, "\\d{4}-\\d{2}-\\d{2}"))
}

sm_lookup <- tibble(
  file    = sm_files,
  sm_date = extract_date(sm_files)
) %>% filter(!is.na(sm_date))


# JOIN WEATHER WITH RASTER LIST BY DATE
weather_joined <- weather %>%
  inner_join(sm_lookup, by = c("date" = "sm_date"))

cat("Joined rows:", nrow(weather_joined), "\n")

# The dataset with weather uses different coords (lat/lon) to the soil moisture rasters (EPSG:27700).
# We need to reproject the weather coords to EPSG:27700 before extraction.

# REPROJECT WEATHER COORDS → EPSG:27700 (AFTER JOINING)
# Use any raster to obtain CRS
r_test <- rast(weather_joined$file[1])
crs(r_test) <- "EPSG:27700"

weather_vect <- vect(
  weather_joined,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

weather_osgb <- project(weather_vect, "EPSG:27700")

coords_osgb <- geom(weather_osgb)[, c("x", "y")]

weather_joined$x <- coords_osgb[, 1]
weather_joined$y <- coords_osgb[, 2]

cat("Sample reprojected coords:\n")
print(head(weather_joined[, c("longitude", "latitude", "x", "y")]))


# SOIL MOISTURE EXTRACTION (DAY GROUPED)
soil_values <- rep(NA_real_, nrow(weather_joined))

unique_files <- unique(weather_joined$file)
cat("Extracting for", length(unique_files), "unique days...\n")

for (f in unique_files) {
  idx <- which(weather_joined$file == f)

  r <- rast(f)
  crs(r) <- "EPSG:27700"

  coords_day <- as.matrix(weather_joined[idx, c("x", "y")])

  # identify valid points
  cells <- cellFromXY(r, coords_day)
  good <- which(!is.na(cells))

  if (length(good) > 0) {
    vals <- terra::extract(r, coords_day[good, , drop = FALSE])

    # identify the soil moisture column
    value_col <- setdiff(names(vals), "ID")

    if (length(value_col) == 1) {
      soil_values[idx[good]] <- vals[[value_col]]
    } else {
      soil_values[idx[good]] <- NA
    }
  }

  cat(
    "Done:", basename(f),
    "   total:", length(idx),
    "   valid:", length(good), "\n"
  )
}

weather_joined$soil_moisture <- soil_values

#  SAVE FINAL MERGED CSV  (REMOVE NA SOIL MOISTURE ROWS)
final_df <- weather_joined %>%
  select(
    location,
    date,
    latitude, longitude,
    `min_temp °c`,
    `max_temp °c`,
    `rain mm`,
    `humidity %`,
    `cloud_cover %`,
    `wind_speed km/h`,
    wind_direction,
    wind_direction_numerical,
    soil_moisture
  ) %>%
  filter(!is.na(soil_moisture)) # remove rows where extraction failed

cat("Rows after removing NA soil-moisture:", nrow(final_df), "\n")

write_csv(final_df, "weather_soilmoisture_merged.csv")

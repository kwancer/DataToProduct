library(dplyr)
library(readr)
library(tidygeocoder)
library(stringr)
library(tidyr)

weather <- read_csv("weather.csv")
print(head(weather))

# GET UNIQUE CITY NAMES
cities <- weather %>%
  distinct(location) %>%
  filter(!is.na(location), location != "")

cat("Number of unique city names:", nrow(cities), "\n")

# GEOCODE CITY NAMES USING OPENSTREETMAP
cat("Geocoding cities... this may take a few minutes...\n")

geo <- cities %>%
  tidygeocoder::geocode(
    location,
    method = "osm",
    lat = latitude,
    long = longitude,
    limit = 1,
    quiet = FALSE
  )

print(head(geo))

# FILTER TO UK BOUNDING BOX
# UK approx bounding box:
#   lat: 49 to 61
#   lon: -8 to   2
geo_uk <- geo %>%
  filter(
    latitude > 49,
    latitude < 61,
    longitude > -8,
    longitude < 2
  )

cat("Cities inside UK bounding box:", nrow(geo_uk), "\n")

# JOIN LAT/LONG BACK TO THE WEATHER DATASET
weather_uk <- weather %>%
  inner_join(geo_uk, by = "location")

cat("Weather rows after filtering:", nrow(weather_uk), "\n")
print(head(weather_uk))

# SAVE CLEANED DATA
write_csv(geo_uk, "uk_city_coordinates.csv")
write_csv(weather_uk, "weather_uk_cleaned.csv")

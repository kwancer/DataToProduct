# R/ui_helpers.R -----------------------------------------------------

# Null-coalescing operator (like x %||% y in JS/other R pkgs)
# Returns y if x is NULL, otherwise x
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# --------------------------------------------------------------------
# Weather code → emoji + short text
# (matches Open-Meteo's weather codes)
# --------------------------------------------------------------------
weather_lookup <- function(code) {
  map <- list(
    "0"  = c("☀️", "Clear sky"),
    "1"  = c("🌤️", "Mainly clear"),
    "2"  = c("⛅",  "Partly cloudy"),
    "3"  = c("☁️", "Overcast"),
    
    "45" = c("🌫️", "Fog"),
    "48" = c("🌫️", "Depositing fog"),
    
    "51" = c("🌦️", "Light drizzle"),
    "53" = c("🌦️", "Moderate drizzle"),
    "55" = c("🌧️", "Dense drizzle"),
    
    "61" = c("🌧️", "Slight rain"),
    "63" = c("🌧️", "Moderate rain"),
    "65" = c("🌧️", "Heavy rain"),
    
    "71" = c("🌨️", "Slight snow"),
    "73" = c("🌨️", "Moderate snow"),
    "75" = c("❄️",  "Heavy snow"),
    
    "95" = c("⛈️", "Thunderstorm"),
    "96" = c("⛈️", "Thunderstorm + hail"),
    "99" = c("⛈️", "Thunderstorm + heavy hail")
  )
  
  key <- as.character(code)
  if (!is.null(map[[key]])) {
    map[[key]]
  } else {
    c("🌡️", "Unknown")
  }
}

# --------------------------------------------------------------------
# Weather code → background tint colour, given theme ("light"/"dark")
# This is used for the weather cards on the results screen.
# --------------------------------------------------------------------
weather_tint <- function(code, theme = "light") {
  light <- list(
    sun      = "#FFF4C2",
    cloud    = "#F3F5F7",
    rain     = "#E7F1FF",
    fog      = "#EFF3F5",
    thunder  = "#F4ECFF",
    snow     = "#F4FAFF"
  )
  
  dark <- list(
    sun      = "#2c2a00",
    cloud    = "#1e2326",
    rain     = "#182228",
    fog      = "#1c2123",
    thunder  = "#1f1a2d",
    snow     = "#1b242e"
  )
  
  code_num <- suppressWarnings(as.numeric(code))
  
  cat <- if (code_num %in% c(0, 1)) {
    "sun"
  } else if (code_num %in% c(2, 3)) {
    "cloud"
  } else if (code_num %in% c(45, 48)) {
    "fog"
  } else if (code_num %in% c(51, 53, 55, 61, 63, 65)) {
    "rain"
  } else if (code_num %in% c(71, 73, 75)) {
    "snow"
  } else if (code_num %in% c(95, 96, 99)) {
    "thunder"
  } else {
    "cloud"
  }
  
  palette <- if (identical(theme, "dark")) dark else light
  palette[[cat]]
}

# R/server_main.R ----------------------------------------------------
# Main server logic for the Water My Lawn app (clean skeleton)

server_main <- function(input, output, session) {
  # ------------------------------------------------------------------
  # SMALL UTIL
  # ------------------------------------------------------------------
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ------------------------------------------------------------------
  # THEME HANDLING
  # ------------------------------------------------------------------
  theme_trigger <- reactiveVal(0)
  
  observeEvent(input$theme, {
    theme_trigger(theme_trigger() + 1)
  })
  
  # ------------------------------------------------------------------
  # STEP NAVIGATION (1–6)
  # ------------------------------------------------------------------
  step  <- reactiveVal(1)
  total <- 6
  
  # Swipe navigation
  observeEvent(input$swipe_next, {
    if (step() < total) step(step() + 1)
  })
  observeEvent(input$swipe_prev, {
    if (step() > 1) step(step() - 1)
  })
  
  # Button navigation to specific step
  lapply(1:total, function(i) {
    observeEvent(input[[paste0("to", i)]], {
      step(i)
    })
  })
  
  # Auto move from step 5 → 6 after 2.5 seconds
  observeEvent(step(), {
    if (step() == 5) {
      later::later(function() {
        isolate({
          if (step() == 5) step(6)
        })
      }, 2.5)
    }
  })
  
  # ------------------------------------------------------------------
  # PROGRESS BAR
  # ------------------------------------------------------------------
  observeEvent(step(), {
    pct <- if (total > 1) (step() - 1) / (total - 1) * 100 else 100
    session$sendCustomMessage("progress", pct)
  })
  
  # ------------------------------------------------------------------
  # PRO TIPS FOR LOADING SCREEN
  # ------------------------------------------------------------------
  pro_tips <- c(
    "Water early in the morning to minimise evaporation.",
    "Avoid watering at night — it can encourage fungal growth.",
    "Deep, infrequent watering encourages stronger root growth."
    # (you can keep the rest of your list here)
  )
  
  loading_tip <- reactiveVal(NULL)
  
  observeEvent(step(), {
    if (step() == 5) {
      loading_tip(sample(pro_tips, 1))
    }
  })
  
  # ------------------------------------------------------------------
  # SOIL MOISTURE PREDICTION HOLDER
  # ------------------------------------------------------------------
  # Will store tibble: date, sm_model, sm_api, diff
  soil_pred <- reactiveVal(NULL)
  
  
  # ------------------------------------------------------------------
  # ENABLE SWIPE GESTURES
  # ------------------------------------------------------------------
  session$sendCustomMessage("enableSwipe", TRUE)
  
  # ------------------------------------------------------------------
  # WATERING HISTORY (last 7 days, minutes/day)
  # ------------------------------------------------------------------
  days <- reactive(seq(Sys.Date() - 6, Sys.Date(), by = "1 day"))
  
  wat <- reactive({
    sapply(1:7, function(i) {
      input[[paste0("w", i)]] %||% 0
    })
  })
  
  
  # Watering bars UI (step 4)
  output$historyBars <- renderUI({
    d <- days()
    w <- wat()
    maxMinutes <- 20
    
    bars <- lapply(1:7, function(i) {
      ratio <- w[i] / maxMinutes
      ratio <- max(0, min(1, ratio))
      height_pct <- ratio * 100
      day_str  <- format(d[i], "%a")
      date_str <- format(d[i], "%d/%m")
      
      tagList(
        div(
          class = "bar-wrap",
          div(
            class = "bar-bg",
            `data-i` = i,
            div(
              class = "bar-fill",
              style = paste0("height:", height_pct, "%;")
            )
          ),
          div(class = "day-label", paste(day_str, date_str)),
          div(class = "day-value", paste0(w[i], " min"))
        )
      )
    })
    
    tagList(
      div(class = "history-grid", bars),
      # trigger JS to attach drag handlers
      tags$script("Shiny.setInputValue('initDragTrigger', Math.random());")
    )
  })
  
  
  observeEvent(input$initDragTrigger, {
    session$sendCustomMessage("initDrag", 20)
  })
  
  # ------------------------------------------------------------------
  # POSTCODE → COORDS
  # ------------------------------------------------------------------
  get_postcode_coords <- function(pc) {
    pc_clean <- gsub(" ", "", pc)
    url <- paste0("https://api.postcodes.io/postcodes/", pc_clean)
    res <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    if (is.null(res) || res$status != 200) return(NULL)
    list(
      lat = res$result$latitude,
      lon = res$result$longitude
    )
  }
  
  # ------------------------------------------------------------------
  # MAP (step 1)
  # ------------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(-1.5, 54.5, zoom = 5)
  })
  
  observeEvent(input$locate, {
    pc <- input$postcode
    if (is.null(pc) || pc == "") return(NULL)
    
    coords <- get_postcode_coords(pc)
    if (!is.null(coords)) {
      leafletProxy("map") |>
        setView(lng = coords$lon, lat = coords$lat, zoom = 16)
    } else {
      showNotification("Postcode not found", type = "error")
    }
  })
  
  # ------------------------------------------------------------------
  # BASIC DERIVED QUANTITIES
  # ------------------------------------------------------------------
  garden_area <- reactive({
    if (input$am %||% "d" == "a") {
      input$area %||% 50
    } else {
      (input$w %||% 6) * (input$l %||% 10)
    }
  })
  
  # Live preview of area when using width × length
  output$area_calc <- renderText({
    # Only show when "Width × Length" is selected
    if (input$am %||% "a" != "d") return(NULL)
    
    w <- input$w
    l <- input$l
    
    if (is.null(w) || is.null(l) || is.na(w) || is.na(l)) return(NULL)
    
    paste("Estimated area:", round(w * l), "m²")
  })
  
  
  flow_rate <- reactive({
    input$flow %||% 10
  })
  
  # ------------------------------------------------------------------
  # WEATHER FORECAST (Open-Meteo)
  # ------------------------------------------------------------------
  weather_forecast <- reactive({
    req(input$postcode)
    
    coords <- get_postcode_coords(input$postcode)
    if (is.null(coords)) return(NULL)
    
    w_url <- paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=", coords$lat,
      "&longitude=", coords$lon,
      "&daily=weathercode,temperature_2m_max,temperature_2m_min,precipitation_sum",
      "&timezone=auto"
    )
    
    w_json <- tryCatch(jsonlite::fromJSON(w_url), error = function(e) NULL)
    if (is.null(w_json) || is.null(w_json$daily)) return(NULL)
    
    days    <- w_json$daily$time
    tmin    <- w_json$daily$temperature_2m_min
    tmax    <- w_json$daily$temperature_2m_max
    code    <- w_json$daily$weathercode
    rain_mm <- w_json$daily$precipitation_sum
    
    decoded <- lapply(code, weather_lookup)
    icons   <- sapply(decoded, function(x) x[1])
    cond    <- sapply(decoded, function(x) x[2])
    
    data.frame(
      day       = format(as.Date(days), "%a %d %b"),
      icon      = icons,
      condition = cond,
      tmin      = tmin,
      tmax      = tmax,
      code      = code,
      rain_mm   = rain_mm
    )
  })
  
  # ------------------------------------------------------------------
  # WHEN USER GOES FROM STEP 4 → 5: RUN SOIL MOISTURE PREDICTION
  # ------------------------------------------------------------------
  observeEvent(input$to5, {
    pc    <- input$postcode
    area  <- garden_area()
    flow  <- flow_rate()
    w_hist <- wat()
    
    if (is.null(pc) || pc == "") {
      showNotification("Please enter a postcode first.", type = "error")
      return(NULL)
    }
    
    res <- tryCatch(
      {
        predict_soil_moisture_today(
          postcode       = pc,
          watering_mins  = w_hist,
          flow_lpm       = flow,
          garden_area_m2 = area,
          timezone       = "Europe/London",
          verbose        = FALSE
        )
      },
      error = function(e) {
        warning("Prediction failed: ", conditionMessage(e))
        NULL
      }
    )
    
    soil_pred(res)
    step(5)
  })
  
  # ------------------------------------------------------------------
  # WATERING PLAN (uses soil_pred + forecast)
  # ------------------------------------------------------------------
  rec_plan <- reactive({
    w    <- wat() # watering history
    pred <- soil_pred() # prediction from my model + API
    wf   <- weather_forecast() # ENHANCED forecast from API call
    
    avg_hist <- mean(w)
    if (is.na(avg_hist)) avg_hist <- 0
    
    # ----- 1. BASE MINUTES FROM SOIL MOISTURE -----
    sm <- if (!is.null(pred) && !is.null(pred$sm_model)) pred$sm_model[1] else NA_real_
    
    # fallback if no prediction of soil moisture
    if (is.na(sm)) {
      if (avg_hist <= 0) avg_hist <- 8
      base_mins <- pmax(3, pmin(20, avg_hist * 0.9))
    } else {
      # Soil moisture based watering 
      if (sm > 0.40) {
        base_mins <- if (avg_hist <= 0) 3 else 0
      } else if (sm > 0.37) {
        base_mins <- if (avg_hist <= 0) 7 else 3
      } else if (sm > 0.34) {
        base_mins <- if (avg_hist <= 0) 10 else 5
      } else if (sm > 0.31) {
        base_mins <- if (avg_hist <= 0) 14 else 7
      } else if (sm > 0.28) {
        base_mins <- if (avg_hist <= 0) 18 else 10
      } else if (sm > 0.25) {
        base_mins <- if (avg_hist <= 0) 21 else 13
      } else {
        base_mins <- if (avg_hist <= 0) 25 else 15
      }
      
      base_mins <- pmin(25, pmax(0, base_mins))
    }
    
    # ----- 2. ENHANCED DAILY ADJUSTMENT WITH MULTIPLE WEATHER FACTORS -----
    days_out <- seq(Sys.Date() + 1, by = "1 day", length.out = 7)
    rec_vec  <- rep(round(base_mins), 7)
    
    if (!is.null(wf)) {
      # Align forecast rows to the 7 next days we care about
      wf2 <- wf |>
        dplyr::mutate(date = as.Date(day, format = "%a %d %b")) |>
        dplyr::right_join(
          tibble::tibble(date = days_out),
          by = "date"
        )
      
      # Get coordinates for potential ET calculation
      coords <- get_postcode_coords(input$postcode)
      lat <- if(!is.null(coords)) coords$lat else 51.5  # Default UK latitude
      
      # Differentiate the plan considering MULTIPLE factors
      for (i in seq_along(rec_vec)) {
        rain_i    <- wf2$rain_mm[i]
        tmax_i    <- wf2$tmax[i]
        tmin_i    <- wf2$tmin[i]
        wind_i    <- wf2$wind_max_kmh[i] %||% NA
        radiation_i <- wf2$radiation_mj[i] %||% NA
        code_i    <- wf2$code[i]
        
        if (is.na(rain_i) || is.na(tmax_i)) next
        
        # ----- 2A. RAIN ADJUSTMENT (water input) -----
        rain_factor <- case_when(
          rain_i >= 10 ~ 0.0,    # Heavy rain - no watering needed
          rain_i >= 8  ~ 0.2,
          rain_i >= 6  ~ 0.4,
          rain_i >= 5  ~ 0.5,
          rain_i >= 4  ~ 0.6,
          rain_i >= 3  ~ 0.7,
          rain_i >= 2  ~ 0.8,
          rain_i >= 1  ~ 0.9,
          TRUE         ~ 1.0     # No rain - full watering
        )
        
        # ----- 2B. TEMPERATURE ADJUSTMENT (evaporation) -----
        # Higher temps = more evaporation = more water needed
        temp_factor <- case_when(
          tmax_i >= 30 ~ 1.3,    # Very hot: 30% more water
          tmax_i >= 27 ~ 1.2,    # Hot: 20% more
          tmax_i >= 24 ~ 1.1,    # Warm: 10% more
          tmax_i >= 21 ~ 1.05,   # Mildly warm: 5% more
          tmax_i >= 15 ~ 1.0,    # Comfortable: no adjustment
          tmax_i >= 10 ~ 0.9,    # Cool: 10% less
          tmax_i >= 5  ~ 0.8,    # Cold: 20% less
          TRUE         ~ 0.7     # Very cold: 30% less
        )
        
        # ----- 2C. WIND ADJUSTMENT (evaporation) -----
        # High wind = more evaporation
        wind_factor <- if (!is.na(wind_i)) {
          case_when(
            wind_i >= 40 ~ 1.2,   # Very windy (>40 km/h): 20% more
            wind_i >= 30 ~ 1.15,  # Windy: 15% more
            wind_i >= 20 ~ 1.1,   # Breezy: 10% more
            wind_i >= 10 ~ 1.05,  # Light breeze: 5% more
            TRUE        ~ 1.0     # Calm: no adjustment
          )
        } else {
          1.0
        }
        
        # ----- 2D. SOLAR RADIATION ADJUSTMENT -----
        # More sun = more evaporation
        solar_factor <- if (!is.na(radiation_i)) {
          case_when(
            radiation_i >= 20 ~ 1.3,   # Very sunny: 30% more
            radiation_i >= 15 ~ 1.2,   # Sunny: 20% more
            radiation_i >= 10 ~ 1.1,   # Partly sunny: 10% more
            radiation_i >= 5  ~ 1.05,  # Cloudy with some sun: 5% more
            TRUE             ~ 1.0    # Overcast: no adjustment
          )
        } else {
          # Fallback: Use weather code as proxy for sunshine
          case_when(
            code_i == 0 ~ 1.2,   # Clear sky: 20% more
            code_i == 1 ~ 1.1,   # Mainly clear: 10% more
            code_i == 2 ~ 1.05,  # Partly cloudy: 5% more
            TRUE        ~ 1.0    # Overcast/rainy: no adjustment
          )
        }
        
        # ----- 2E. COMBINE ALL FACTORS -----
        # Rain REDUCES need, other factors INCREASE need
        # Formula: base × (rain_factor) × (evaporation_factors)
        combined_factor <- rain_factor * temp_factor * wind_factor * solar_factor
        
        # Safety bounds: Don't increase beyond 2x or drop below 0
        combined_factor <- pmax(0, pmin(2.0, combined_factor))
        
        # Update recommendation
        rec_vec[i] <- round(base_mins * combined_factor)
      }
    }
    
    # Final clamp to [0, 25] minutes
    rec_vec <- pmax(0, pmin(25, rec_vec))
    
    # Add explanation column for debugging/transparency
    data.frame(
      day  = format(days_out, "%a %d %b"),
      mins = rec_vec,
      stringsAsFactors = FALSE
    )
  })
  
  
  
  # ------------------------------------------------------------------
  # DOWNLOAD HANDLER
  # ------------------------------------------------------------------
  output$download_plan <- downloadHandler(
    filename = function() {
      paste0("watering-plan-", Sys.Date(), ".txt")
    },
    content = function(file) {
      plan <- rec_plan()
      wf   <- weather_forecast()
      
      lines <- c(
        "Your personalised watering plan",
        "",
        sprintf("Garden area: %s m^2", round(garden_area())),
        sprintf("Flow rate: %s L/min", flow_rate()),
        sprintf("Average watering (last 7 days): %s min/day",
                round(mean(wat()), 1)),
        "",
        "Daily watering recommendations:"
      )
      
      plan_lines <- paste0(
        "- ", plan$day, ": ", plan$mins, " min"
      )
      
      weather_lines <- if (is.null(wf)) {
        "Next 7 days weather: unavailable – please check again later."
      } else {
        c(
          "",
          "Next 7 days weather:",
          paste0(
            "- ", wf$day, " ", wf$icon, ": ",
            wf$condition, " (", wf$tmin, "–", wf$tmax, " °C, ",
            wf$rain_mm, " mm)"
          )
        )
      }
      
      writeLines(c(lines, plan_lines, weather_lines), file)
    }
  )
  
  # ------------------------------------------------------------------
  # STEP-SPECIFIC UI HELPERS
  # ------------------------------------------------------------------
  screen_step1 <- function() {
    tagList(
      div(class = "section-icon", style = "font-size:40px;", "📍"),
      div(class = "title", "Where is your lawn?"),
      textInput("postcode", NULL, placeholder = "e.g. SW7 2AZ"),
      actionButton("locate", "Find my garden", class = "next-btn"),
      br(),
      leafletOutput("map", height = 300),
      div(
        class = "nav-row",
        span(),
        actionButton("to2", "Next", class = "next-btn")
      )
    )
  }
  
  screen_step2 <- function() {
    tagList(
      div(class = "section-icon", style = "font-size:40px;", "📏"),
      div(class = "title", "Garden size"),
      
      radioButtons(
        "am", "Measurement method",
        choices = c("Area (m²)" = "a", "Width × Length" = "d"),
        inline  = TRUE
      ),
      
      # Option 1: user directly enters total area
      conditionalPanel(
        "input.am == 'a'",
        numericInput("area", "Area (m²)", value = 50, min = 5)
      ),
      
      # Option 2: user enters width and length, we compute area
      conditionalPanel(
        "input.am == 'd'",
        div(
          class = "dimension-box",
          div(
            class = "dimension-row",
            numericInput("w", "Width (m)", value = 6, min = 1),
            numericInput("l", "Length (m)", value = 10, min = 1)
          ),
          div(
            class = "subtitle",
            textOutput("area_calc")   # live preview, e.g. "Estimated area: 60 m²"
          )
        )
      ),
      
      div(
        class = "nav-row",
        actionButton("to1", "Back", class = "back-btn"),
        actionButton("to3", "Next", class = "next-btn")
      )
    )
  }
  
  
  screen_step3 <- function() {
    # WATERING METHOD -----------------------------------------------
    tagList(
      div(class = "section-icon", style="font-size:40px;", "💧"),
      div(class = "title", "How do you water your lawn?"),
      div(
        class = "subtitle",
        "Choose your usual watering method and estimate its flow rate."
      ),
      
      selectInput(
        "method", "Method",
        choices = c("💦 Hose", "🚿 Sprinkler", "🪣 Watering can", "⚙️ Custom")
      ),
      
      numericInput("flow", "Flow rate (L/min)", 10, min = 1, max = 40),
      
      div(
        class = "tip-box",
        strong("How to measure flow rate"), br(),
        "1. Place your hose or sprinkler into a 10L bucket.", br(),
        "2. Time how many seconds it takes to fill.", br(),
        "3. Flow (L/min) = 10 ÷ (seconds ÷ 60)."
      ),
      
      div(
        class = "nav-row",
        actionButton("to2", "Back", class = "back-btn"),
        actionButton("to4", "Next", class = "next-btn")
      )
    )
  }
  
  screen_step4 <- function() {
    tagList(
      div(class = "section-icon", style = "font-size:40px;", "📊"),
      div(class = "title", "Your watering history"),
      uiOutput("historyBars"),
      div(
        class = "nav-row",
        actionButton("to3", "Back", class = "back-btn"),
        actionButton("to5", "Next", class = "next-btn")
      )
    )
  }
  
  screen_step5 <- function() {
    tagList(
      div(class = "section-icon", style = "font-size:40px;", "🌱"),
      div(class = "title", "Calculating your plan…"),
      div(
        class = "subtitle",
        "Analysing your lawn size, watering method, history and local weather…"
      ),
      div(class = "loading-circle"),
      div(
        class = "tip-box",
        strong("Pro tip • "),
        loading_tip()
      ),
      div(
        class = "nav-row",
        actionButton("to4", "Back", class = "back-btn"),
        span()
      )
    )
  }
  
  screen_step6 <- function() {
    plan <- rec_plan()
    wf   <- weather_forecast()
    pred <- soil_pred()
    current_theme <- input$theme %||% "light"
    
    # --- soil moisture card (keeps your original idea) ---
    soil_card <- div(
      style = paste(
        "background:var(--card);",
        "border-radius:20px;",
        "padding:20px;",
        "border:1px solid var(--border);",
        "box-shadow:var(--shadow);"
      ),
      h4("Soil moisture today", style = "margin-top:0; color:var(--green-dark);"),
      if (is.null(pred) || is.null(pred$sm_model)) {
        p("Soil moisture estimate not available.")
      } else {
        sm   <- pred$sm_model[1]
        api  <- if (!is.null(pred$sm_api)) pred$sm_api[1] else NA_real_
        diff <- if (!is.null(pred$diff))   pred$diff[1]   else NA_real_
        
        tagList(
          p("Model estimate: ", strong(sprintf("%.3f", sm))),
          if (!is.na(api)) {
            tagList(
              p("API estimate: ", strong(sprintf("%.3f", api))),
              p("Difference: ", sprintf("%.3f", diff))
            )
          } else {
            p("API estimate: not available")
          }
        )
      }
    )
    
    # --- weather block (simplified) ---
    wf_block <- if (is.null(wf)) {
      div("Weather unavailable — try again later.")
    } else {
      div(
        style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(140px,1fr)); gap:14px;",
        lapply(1:nrow(wf), function(i) {
          tint <- weather_tint(wf$code[i], theme = current_theme)
          div(
            style = paste0(
              "padding:16px;border-radius:18px;border:1px solid var(--border);",
              "text-align:center;box-shadow:var(--shadow);background:", tint, ";"
            ),
            div(style = "font-size:32px;", wf$icon[i]),
            div(style = "font-weight:600; margin-top:6px;", wf$day[i]),
            div(style = "font-size:14px; margin-top:4px;", wf$condition[i]),
            div(style = "margin-top:8px; font-size:13px;",
                paste0(wf$tmin[i], "°C — ", wf$tmax[i], "°C, ", wf$rain_mm[i], " mm"))
          )
        })
      )
    }
    
    tagList(
      div(class = "section-icon", style = "font-size:40px;", "🌿"),
      div(class = "title", "Your personalised watering plan"),
      
      # summary cards
      div(
        style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(260px,1fr)); gap:20px; margin-bottom:30px;",
        
        div(
          style = "background:var(--card); border-radius:20px; padding:20px; border:1px solid var(--border); box-shadow:var(--shadow);",
          h4("Garden Summary", style = "margin-top:0; color:var(--green-dark);"),
          p("Area: ", strong(round(garden_area()), " m²")),
          p("Flow rate: ", strong(flow_rate(), " L/min")),
          p("Avg watering (last 7 days): ",
            strong(round(mean(wat()), 1), " min/day"))
        ),
        
        soil_card,
        
        div(
          style = "background:var(--card); border-radius:20px; padding:20px; border:1px solid var(--border); box-shadow:var(--shadow);",
          h4("Quick Advice", style = "margin-top:0; color:var(--green-dark);"),
          p("• Water early morning to reduce evaporation."),
          p("• Keep soil consistently moist—not soaked."),
          p("• Skip or reduce watering on days with heavy rain.")
        )
      ),
      
      # daily recommendations
      h3("Daily watering recommendations",
         style = "color:var(--green-dark); margin-bottom:16px;"),
      
      div(
        style = "display:flex; flex-direction:column; gap:12px; margin-bottom:30px;",
        lapply(1:nrow(plan), function(i) {
          div(
            style = paste(
              "background:var(--card); padding:16px; border-radius:18px;",
              "border:1px solid var(--border); display:flex; align-items:center;",
              "justify-content:space-between; box-shadow:var(--shadow);"
            ),
            span(plan$day[i]),
            span(strong(paste0(plan$mins[i], " min")))
          )
        })
      ),
      
      # weather
      h3("Next 7 days weather",
         style = "color:var(--green-dark); margin-bottom:16px;"),
      wf_block,
      
      br(),
      
      div(
        class = "nav-row",
        actionButton("to4", "Back", class = "back-btn"),
        downloadButton("download_plan", "Share my plan", class = "next-btn")
      )
    )
  }
  
  # ------------------------------------------------------------------
  # MAIN SCREEN RENDERING (switch on step)
  # ------------------------------------------------------------------
  output$screen <- renderUI({
    theme_trigger()   # re-run when theme changes
    s <- step()
    
    switch(
      as.character(s),
      "1" = screen_step1(),
      "2" = screen_step2(),
      "3" = screen_step3(),
      "4" = screen_step4(),
      "5" = screen_step5(),
      "6" = screen_step6()
    )
  })
}

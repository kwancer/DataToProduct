# R/ui_main.R --------------------------------------------------------
# Main UI layout for the Water My Lawn app

ui_main <- fluidPage(
  tags$head(
    # External CSS + JS
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    # Hammer.js for swipe gestures
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/hammer.js/2.0.8/hammer.min.js"),
    
    # Custom JS logic (swipe, progress bar, drag bars, theme toggle, etc.)
    tags$script(src = "app.js")
  ),
  
  # NAVBAR WITH LOGO + THEME TOGGLE ---------------------------------
  div(
    class = "top-nav",
    div(
      style = "display:flex; align-items:center; gap:10px;",
      img(src = "logo.png", style = "height:90px;")
    ),
    div(
      class = "theme-toggle",
      span("\u2600\ufe0f"),  # sun
      tags$label(
        class = "switch",
        tags$input(type = "checkbox", id = "themeSwitch"),
        tags$span(class = "slider")
      ),
      span("\U0001F319")    # moon
    )
  ),
  
  # MAIN CARD + PROGRESS BAR ----------------------------------------
  div(id = "card", class = "onboard", uiOutput("screen")),
  
  div(
    class = "progress-holder",
    div(id = "pbar")
  )
)

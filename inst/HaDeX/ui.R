source("data-work.R")
source("custom-elements.R")

for (file in list.files("ui", full.names = TRUE)) source(file, local = TRUE)

options(shiny.useragg = TRUE)

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$script(type = "text/javascript", src = "ga.js"),
    # tags$script(src = "navbar-menu.js"),
    tags$script(type = "text/javascript", src = "detect-element-resize.js")
  ),
  div(
    id = "fullsize-website",
    navbarPage(
      title = "HaDeX",
      tab_start(),
      tab_input(),
      navbarMenu(
        title = "Deuterium uptake",
        tab_woods(),
        tab_volcano(),
        # navbarMenu(
        # title = "Butterfly Plots",
          tab_butterfly(),
          tab_butterfly_diff(),
        # ),
        # navbarMenu(
        # title = "Chiclet Plots",
          tab_chiclet(),
          tab_chiclet_diff(),
        # ),
        # navbarMenu(
        # title = "Uptake curves",
          tab_uptake(),
          tab_uptake_diff()
        # )
      ),
      navbarMenu(
        title = "Time-based data",
        tab_replicates(),
        tab_manhattan(),
        tab_quality
      ),
      navbarMenu(
        title = "Sequence data",
        tab_sequence,
        tab_coverage
      ),
      tab_summary(),
      tab_report(),
      
      theme = "HaDeX_theme.css",
      header = img(id = "HaDeX-logo", src = "logo.png")
    )
  ),
  div(
    id = "mobile-website",
    navbarPage(
      title = "HaDeX",
      # tab_start(mobile = TRUE),
      tab_start(),
      theme = "HaDeX_theme.css",
      header = img(id = "HaDeX-logo", src = "logo.png")
    )
  )
)

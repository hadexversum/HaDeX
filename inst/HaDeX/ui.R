source("data-work.R")
for (file in list.files("ui", full.names = TRUE)) source(file, local = TRUE)

options(spinner.color="#715D91")


ui <- tagList(useShinyjs(),
              tags$head(includeScript("ga.js"),
                        tags$link(rel="stylesheet",
                                  href="mobile_version.css",
                                  media="screen and (max-width: 600px)"),
                        tags$script(type="text/javascript",
                                    src="detect-element-resize.js")),
                navbarPage(
                  "HaDeX",
                  tab_start,
                  tab_input,
                  navbarMenu(
                    "Plots",
                    tab_woods,
                    tab_butterfly,
                    tab_volcano,
                    tab_chiclet,
                    tab_uptake
                  ),
                  navbarMenu(
                    "Time-based data",
                    tab_replicates,
                    tab_quality
                  ),
                  navbarMenu(
                    "Sequence data",
                    tab_sequence,
                    tab_coverage
                  ),
                  tab_summary,
                  tab_report,
                  theme = "HaDeX_theme.css",
                  header = img(src = "logo.png", class = "logo")
              ),
                
                ##################################
                ########## MOBILE ###############
                ##################################
                
              tags$div(
                class = "mobile-site-backbone",
                tags$div(
                  class = "logo-panel",
                  img(src = "mock_logo.png", class = "logo")
                ),
                tags$div(
                  class = "mobile-information",
                  h3("Welcome to HaDeX website!"),
                  h4("For better user experience please use device with wider screen (at least 900px)."),
                  img(src='funding_icons.png', height = 100)
                )
              ),
              tags$script(type="text/javascript",
                          src="resize-logo-panel.js")
)

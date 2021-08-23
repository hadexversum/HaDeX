source("data-work.R")
library(bslib)
for (file in list.files("ui", full.names = TRUE)) source(file, local = TRUE)

options(spinner.color="#715D91")

theme <- bs_theme(fg = "#001c34", 
                  primary = "#0E774E", 
                  secondary = "#6B91B3", 
                  success = "#0E774E", 
                  base_font = font_google("Lato"), 
                  code_font = font_google("Fira Code"),
                  `enable-gradients` = TRUE, 
                  bootswatch = "sandstone", 
                  bg = "#FFFFFF") %>%
  bs_add_variables("navbar-bg" = "#6B91B3",
                   "navbar-light" = "#EDF7D2")

ui <- tagList(useShinyjs(),
              tags$head(includeScript("ga.js"),
                        tags$link(rel="stylesheet",
                                  href="HaDeX_theme.css"), # TODO: import this file to theme
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
                    tab_butterfly_diff,
                    tab_volcano,
                    tab_chiclet,
                    tab_chiclet_diff,
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
                  theme = theme,
                  header = img(src = "logo.png", class = "logo")
              )
)

source("data-work.R")
source("custom-elements.R")
library(bslib)
for (file in list.files("ui", full.names = TRUE)) source(file, local = TRUE)

theme <- bs_theme(bootswatch = "sandstone", 
                  
                  fg = "#25283D", 
                  bg = "#FFFEFD",
                  primary = "#776274", 
                  secondary = "#485696", 
                  success = "#188B55", 
                  
                  base_font = font_google("Lato"), 
                  code_font = font_google("Fira Code"), 
                  font_scale = 1.285714,
                  
                  `enable-gradients` = TRUE) %>%
  bs_add_variables("navbar-bg" = "#CCC2D6",
                   "navbar-light" = "#EDF7D2")
options(shiny.useragg = TRUE)

ui <- tagList(useShinyjs(),
              tags$head(includeScript("ga.js"),
                        tags$link(rel="stylesheet",
                                  href="HaDeX_theme.css"), # TODO: import this file to theme
                        tags$script(type="text/javascript",
                                    src="detect-element-resize.js")),
                navbarPage(
                  "HaDeX",
                  tab_start(),
                  tab_input(),
                  navbarMenu(
                    "Plots",
                    tab_woods(),
                    tab_butterfly(),
                    tab_butterfly_diff(),
                    tab_volcano(),
                    tab_chiclet(),
                    tab_chiclet_diff(),
                    tab_uptake()
                  ),
                  navbarMenu(
                    "Time-based data",
                    tab_replicates(),
                    tab_quality
                  ),
                  navbarMenu(
                    "Sequence data",
                    tab_sequence,
                    tab_coverage
                  ),
                  tab_summary(),
                  tab_report(),
                  
                  theme = theme,
                  header = img(
                    id = "HaDeX-logo",
                    src = "logo.png"
                  )
              )
)

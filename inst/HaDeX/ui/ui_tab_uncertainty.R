tab_uncertainty <- function() HaDeX_plotTab(
  
  title = "Uncertainty",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    uncertainty_plot_param(),
    uncertainty_time_param()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    uncertainty_plot_panel(),
    uncertainty_debug()
  )
  
)


uncertainty_plot_param <- function() HaDeX_plotSettingsSection(
  
  title = "States",
  
  selectInput_h(inputId = "un_state",
                label = "Select state: ",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160")
)

uncertainty_time_param <- function() HaDeX_plotSettingsSection(
  
  title = "Timepoints",
  
  fluidPage(
    fluidRow(
      column(
        width = 6,
        checkboxGroupInput_h(inputId = "un_timepoints",
                             label = "Show time points: ",
                             choices = c(0.167, 1, 5, 25, 120, 1440),
                             selected = c(0.167, 1, 5, 25, 120, 1440))
      ),
      column(
        width = 6,
        checkboxInput_h(inputId = "un_aggregated", 
                        label = "Show aggregated data?",
                        value = TRUE),
        
        checkboxInput_h(inputId = "un_separate_times",
                        label = "Show timepoints separately?",
                        value = FALSE)
      )
    )
  )
  
)


uncertainty_plot_panel <- function() tabsetPanel(
  
  tabPanel("Uncertainty plot",
           plotOutput_h("uncertaintyPlot", width = "80%", height = "800px", hover = hoverOpts("uncertaintyPlot_hover", delay = 10, delayType = "debounce")),
           downloadButton("uncertaintyPlot_download_button",
                          "Save chart (.svg)")
  ),
  tabPanel("Data",
           DT::dataTableOutput("uncertaintyPlot_data")
  )
  
  
)

uncertainty_debug <- function() uiOutput("uncertaintyPlot_debug")
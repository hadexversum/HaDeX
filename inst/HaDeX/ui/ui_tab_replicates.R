tab_replicates <- function() HaDeX_plotTab(
  
  title = "Replicates",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    replicates_plot_param()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    replicates_histogram_panel(),
    replicates_histogram_all_panel(),
    replicates_histogram_times_panel()
  )
  
)

replicates_plot_param <- function() HaDeX_plotSettingsSection(
  
  title = "Select parameters:",
  
  selectInput_h(inputId = "rep_state",
                label = "Select state: ",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160"),
  
  selectInput_h(inputId = "rep_time",
                label = "Select time point: ",
                choices = c(0, 0.001, 0.167, 1, 5, 25, 120, 1440),
                selected = 1)
)



replicates_histogram_panel <- function() tabsetPanel(
  
  tabPanel("Plot",
           div(style = "position:relative",
               plotOutput_h("replicatesHistogram", hover = hoverOpts("replicatesHistogram_hover", delay = 10, delayType = "debounce")),
               uiOutput("replicatesHistogram_debug")),
           downloadButton("replicatesHistogram_download_button",
                          "Save chart (.svg)")),
  tabPanel("Data",
           DT::dataTableOutput("replicatesHistogram_data"))
)

replicates_histogram_all_panel <- function() tabsetPanel(
  
  tabPanel("Plot",
           div(style = "position:relative",
               plotOutput_h("allReplicatesHistogram", hover = hoverOpts("allReplicatesHistogram_hover", delay = 10, delayType = "debounce")),
               uiOutput("allReplicatesHistogram_debug")
           ),
           downloadButton("allReplicatesHistogram_download_button",
                          "Save chart (.svg)")),
  
  tabPanel("Data",
           DT::dataTableOutput("allReplicatesHistogram_data"))
)

replicates_histogram_times_panel <- function() tabsetPanel(
  
  tabPanel("Plot", 
           div(style = "position:relative",
               plotOutput_h("timesReplicatesHistogram", hover = hoverOpts("timesReplicatesHistogram_hover", delay = 10, delayType = "debounce")),
               uiOutput("timesReplicatesHistogram_debug")),
           downloadButton("timesReplicatesHistogram_download_button",
                          "Save chart (.svg)")),
  
  tabPanel("Data",
           DT::dataTableOutput("timesReplicatesHistogram_data"))
  
)
 
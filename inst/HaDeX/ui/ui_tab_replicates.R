tab_replicates <- function() HaDeX_plotTab(
  
  title = "Replicates",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    replicates_plot_param(),
    replicates_sequence(),
    replicates_labels_adjustement()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    replicates_plot_and_charge_panel(),
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
                selected = 1),
  
  checkboxInput_h(inputId = "rep_show_charge",
                  label = "Show charge values?",
                  value = TRUE)
)


replicates_sequence <- function() HaDeX_plotSettingsSection(
  
  title = "Sequence",
  
  dataTableOutput_h("rep_sequence")
)

replicates_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-replicates-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    
    id = "HaDeX-replicates-labels-adjusting-panel",
    
    fluidRow(
      column(width = 10,
             textInput(inputId = "rep_plot_title",
                       label = "Plot title:",
                       value = ""),
             textInput(inputId = "rep_plot_x_label",
                       label = "Plot x label:",
                       value = "Measured mass [Da]"),
             textInput(inputId = "rep_plot_y_label",
                       label = "Plot y label:",
                       value = "")),
      column(width = 2,
             numericInput_h(inputId = "rep_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "rep_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "rep_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label.")
  )
)


replicates_plot_and_charge_panel <- function() tabsetPanel(
  
  tabPanel("Plot",
           # fluidRow(
             # column(width = 6,
                    plotOutput_h("replicatesPlot", hover = hoverOpts("replicatesPlot_hover", delay = 10, delayType = "debounce")),
                    uiOutput("replicatesPlot_debug"),
                    downloadButton("replicatesPlot_download_button",
                                   "Save chart (.svg)")
           # ),
             # column(width = 6,
                    # plotOutput_h("replicatesChargePlot", hover = hoverOpts("replicatesChargePlot_hover", delay = 10, delayType = "debounce")),
                    # uiOutput("replicatesChargePlot_debug"),
                    # downloadButton("replicatesChargePlot_download_button",
                                   # "Save chart (.svg)"))
           # )
  ),
  
  tabPanel("Data",
           DT::dataTableOutput("replicatesPlot_data"),
           DT::dataTableOutput("replicatesChargePlot_data")
  )
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
 
tab_measurements <- function() HaDeX_plotTab(
  
  title = "Measurements",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    measures_plot_param(),
    measures_sequence(),
    measures_labels_adjustement()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    measures_plot_and_charge_panel()
  )
  
)

measures_plot_param <- function() HaDeX_plotSettingsSection(
  
  title = "Select parameters:",
  
  selectInput_h(inputId = "measures_state",
                label = "Select state: ",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160"),
  
  selectInput_h(inputId = "measures_time",
                label = "Select time point: ",
                choices = c(0, 0.001, 0.167, 1, 5, 25, 120, 1440),
                selected = 1),
  
  checkboxInput_h(inputId = "measures_show_charge",
                  label = "Show charge values?",
                  value = TRUE)
)

measures_sequence <- function() HaDeX_plotSettingsSection(
  
  title = "Sequence",
  
  dataTableOutput_h("measures_sequence")
)

measures_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-measures-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    
    id = "HaDeX-measures-labels-adjusting-panel",
    
    fluidRow(
      column(width = 10,
             textInput(inputId = "measures_plot_title",
                       label = "Plot title:",
                       value = ""),
             textInput(inputId = "measures_plot_x_label",
                       label = "Plot x label:",
                       value = "Measured mass [Da]"),
             textInput(inputId = "measures_plot_y_label",
                       label = "Plot y label:",
                       value = "")),
      column(width = 2,
             numericInput_h(inputId = "measures_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "measures_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "measures_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label.")
  )
)


measures_plot_and_charge_panel <- function() tabsetPanel(
  
  tabPanel("Plot",
           # fluidRow(
           # column(width = 6,
           plotOutput_h("measuresPlot", hover = hoverOpts("measuresPlot_hover", delay = 10, delayType = "debounce")),
           uiOutput("measuresPlot_debug"),
           downloadButton("measuresPlot_download_button",
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
           DT::dataTableOutput("measuresPlot_data")
  )
)
tab_measurements <- function() HaDeX_plotTab(
  
  title = "Measurements",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    measures_mass_uptake_plot_param(),
    measures_plot_param(),
    measures_sequence(),
    mass_uptake_visualization(),
    mass_uptake_zoom(),
    measures_labels_adjustement()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    measures_plot_and_charge_panel(),
    measures_mass_uptake_panel()
  )
  
)

measures_mass_uptake_plot_param <- function() HaDeX_plotSettingsSection(
  
  title = "Select parameters:",
  
  selectInput_h(inputId = "measures_state",
                label = "Select state: ",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160"),
  
  checkboxInput_h(inputId = "measures_show_charge",
                  label = "Show replicate values?",
                  value = FALSE)
)

measures_plot_param <- function() HaDeX_plotSettingsSection(
  
  title = "Parameters for measurements plot:",
  
  selectInput_h(inputId = "measures_time",
                label = "Select time point: ",
                choices = c(0, 0.001, 0.167, 1, 5, 25, 120, 1440),
                selected = 1)
  
)

measures_sequence <- function() HaDeX_plotSettingsSection(
  
  title = "Sequence",
  
  dataTableOutput_h("measures_sequence")
)

mass_uptake_visualization <- function() HaDeX_plotSettingsSection(
  
  title = "Visualization for mass uptake curve",
  
  checkboxInput_h(inputId = "mass_uptake_log_x",
                  label = "Logaritmic x scale",
                  value = TRUE)
)

mass_uptake_zoom <- function() HaDeX_plotSettingsSection(
  
  title = "Zoom",
  
  sliderInput(inputId = 'mass_uptake_plot_y_range',
              label = 'Choose y range for the plot:',
              min = -5,
              max = 50,
              value = c(-1, 50),
              step = 1)
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
                       label = "Measurements plot title:",
                       value = ""),
             textInput(inputId = "measures_plot_x_label",
                       label = "Measurements plot x label:",
                       value = "Measured mass [Da]"),
             textInput(inputId = "measures_plot_y_label",
                       label = "Measurements plot y label:",
                       value = ""),
             textInput(inputId = "mass_uptake_plot_title",
                       label = "Mass uptake plot title:",
                       value = "Mass uptake plot"),
             textInput(inputId = "mass_uptake_plot_x_label",
                       label = "Mass uptake plot x label:",
                       value = "Time point [min]"),
             textInput(inputId = "mass_uptake_plot_y_label",
                       label = "Mass uptake plot y label:",
                       value = "Mass [Da]")
             ),
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
                            min = 5),
             numericInput_h(inputId = "mass_uptake_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "mass_uptake_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "mass_uptake_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5)
             )
    ),
    p("The axis ticks have the same size as the axis label.")
  )
)


measures_plot_and_charge_panel <- function() tabsetPanel(
  
  tabPanel("Plot",
           plotOutput_h("measuresPlot", hover = hoverOpts("measuresPlot_hover", delay = 10, delayType = "debounce")),
           uiOutput("measuresPlot_debug"),
           downloadButton("measuresPlot_download_button",
                          "Save chart (.svg)")
  ),
  
  tabPanel("Data",
           DT::dataTableOutput("measuresPlot_data")
  )
)

measures_mass_uptake_panel <- function() tabsetPanel(
  
  tabPanel("Plot",
           plotOutput_h("massUptakePlot"),
           downloadButton("massUptakePlot_downlad_button",
                          "Save chart (.svg)")),
  
  tabPanel("Data", 
           DT::dataTableOutput("massUptakePlot_data"))

)
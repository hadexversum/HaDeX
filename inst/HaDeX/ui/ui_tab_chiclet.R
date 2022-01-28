tab_chiclet <- function() HaDeX_plotTab(
  title = "Chiclet plot",
  settingsPanel = HaDeX_plotSettingsPanel(
    chiclet_general_settings(),
    chiclet_state(),
    chiclet_timepoints(),
    chiclet_visualization(),
    chiclet_zoom(),
    chiclet_labels_adjustement()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    chiclet_plot_panel()
  )
)
  
chiclet_general_settings <- function() HaDeX_plotSettingsSection(
  checkboxInput_h(inputId = "chic_theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = "chic_fractional",
                  label = "Fractional values",
                  value = FALSE)
)

chiclet_state <- function() HaDeX_plotSettingsSection(
  title = "State",
  selectInput_h(inputId = "chic_state",
                label = "Choose state:",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160")
)                        

chiclet_timepoints <- function() HaDeX_plotSettingsSection(
  title = "Timepoints",
  fluidRow(
    column(width = 6,
           checkboxGroupInput_h(inputId = "chic_timepoints",
                                label = "Show time points: ",
                                choices = c(0.167, 1, 5, 25, 120, 1440),
                                selected = c(0.167, 1, 5, 25, 120, 1440))
    ),
    column(width = 6,
           div(id = "chic_time_0_part",
               selectInput_h(inputId = "chic_time_0",
                             label = "TIME IN",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 0.001)
           ),
           div(id = "chic_time_100_part",
               selectInput_h(inputId = "chic_time_100",
                             label = "TIME OUT",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 1440)
           )
    )
  )
)

chiclet_visualization <- function() HaDeX_plotSettingsSection(
  title = "Visualization",
  checkboxInput_h(inputId = "chic_show_uncertainty",
                  label = "Show uncertainty",
                  value = TRUE)
)

chiclet_zoom <- function() HaDeX_plotSettingsSection(
  title = "Zoom",
  sliderInput(inputId = "chic_x_range",
              label = "Choose x range for chiclet plot:",
              min = 1,
              max = 41,
              value = c(1, 41),
              step = 1)
)

chiclet_labels_adjustement <- function() HaDeX_plotSettingsSection(
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-chiclet-labels-adjusting-panel"
  ),
  HaDeX_collapsablePanel(
    id = "HaDeX-chiclet-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "chiclet_plot_title",
                       label = "Chiclet plot title:",
                       value = ""),
             textInput(inputId = "chiclet_plot_x_label",
                       label = "Chiclet plot axis x label:",
                       value = "Peptide ID"),
             textInput(inputId = "chiclet_plot_y_label",
                       label = "Chiclet plot axis y label:",
                       value = "Exposure [min]")),
      column(width = 2,
             numericInput_h(inputId = "chiclet_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "chiclet_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "chiclet_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

chiclet_plot_panel <- function() tabsetPanel(
  tabPanel("Chiclet plot",
           plotOutput_h("chicletPlot", hover = hoverOpts("chicletPlot_hover", delay = 10, delayType = "debounce")),
           uiOutput("chicletPlot_debug"),
           downloadButton("chicletPlot_download_button",
                          "Save chart (.svg)")
  ),
  tabPanel("Data",
           DT::dataTableOutput("chicletPlot_data"),
           p("Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value.")
  )
)
tab_chiclet_diff <- function() HaDeX_plotTab(
  title = "Chiclet differential plot",
  settingsPanel = HaDeX_plotSettingsPanel(
    chiclet_diff_general_settings(),
    chiclet_diff_state(),
    chiclet_diff_timepoints(),
    chiclet_diff_visualization(),
    chiclet_diff_zoom(),
    chiclet_diff_labels_adjustement()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    chiclet_diff_plot_panel()
  )
)

chiclet_diff_general_settings <- function() HaDeX_plotSettingsSection(
  checkboxInput_h(inputId = "chic_diff_theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = "chic_diff_fractional",
                  label = "Fractional values",
                  value = FALSE)
)

chiclet_diff_state <- function() HaDeX_plotSettingsSection(
  title = "States",
  p("Differential plot presents the uptake difference [Da] between State 1 and State 2."),
  splitLayout(selectInput_h(inputId = "chic_diff_state_1",
                            label = "State 1",
                            choices = c("CD160", "CD160_HVEM")),
              selectInput_h(inputId = "chic_diff_state_2",
                            label = "State 2",
                            choices = c("CD160_HVEM", "CD160"))
  )
)                        

chiclet_diff_timepoints <- function() HaDeX_plotSettingsSection(
  title = "Timepoints",
  fluidRow(
    column(width = 6,
           checkboxGroupInput_h(inputId = "chic_diff_timepoints",
                                label = "Show time points: ",
                                choices = c(0.167, 1, 5, 25, 120, 1440),
                                selected = c(0.167, 1, 5, 25, 120, 1440))
    ),
    column(width = 6,
           div(id = "chicdiff_time_0_part",
               selectInput_h(inputId = "chic_diff_time_0",
                             label = "TIME IN",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 0.001)
           ),
           div(id = "chicdiff_time_100_part",
               selectInput_h(inputId = "chic_diff_time_100",
                             label = "TIME OUT",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 1440)
           )
    )
  )
)

chiclet_diff_visualization <- function() HaDeX_plotSettingsSection(
  title = "Visualization",
  checkboxInput_h(inputId = "chic_diff_show_uncertainty",
                  label = "Show uncertainty",
                  value = TRUE)
)

chiclet_diff_zoom <- function() HaDeX_plotSettingsSection(
  title = "Zoom",
  sliderInput(inputId = "chic_diff_x_range",
              label = "Choose x range for chiclet plot:",
              min = 1,
              max = 41,
              value = c(1, 41),
              step = 1)
)

chiclet_diff_labels_adjustement <- function() HaDeX_plotSettingsSection(
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-chiclet-diff-labels-adjusting-panel"
  ),
  HaDeX_collapsablePanel(
    id = "HaDeX-chiclet-diff-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "chicletDifferential_plot_title",
                       label = "Chiclet plot title:",
                       value = ""),
             textInput(inputId = "chicletDifferential_plot_x_label",
                       label = "Chiclet plot axis x label:",
                       value = "Peptide ID"),
             textInput(inputId = "chicletDifferential_plot_y_label",
                       label = "Chiclet plot axis y label:",
                       value = "Exposure [min]")),
      column(width = 2,
             numericInput_h(inputId = "chicletDifferential_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "chicletDifferential_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "chicletDifferential_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

chiclet_diff_plot_panel <- function() tabsetPanel(
  tabPanel("Chiclet differential plot",
           plotOutput_h("chicletDifferentialPlot", hover = hoverOpts("chicletDifferentialPlot_hover", delay = 10, delayType = "debounce")),
           uiOutput("chicletDifferentialPlot_debug"),
           downloadButton("chicletDifferentialPlot_download_button",
                          "Save chart (.svg)")
  ),
  tabPanel("Data",
           DT::dataTableOutput("chicletDifferentialPlot_data"),
           p(
             "The table presents data from the chosen x plot range.",
             "Abbreviations from the table: Diff DU - differential deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )
  )
)
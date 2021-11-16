tab_butterfly <- function() HaDeX_plotTab(
  title = "Butterfly plot",
  settingsPanel = HaDeX_plotSettingsPanel(
    butterfly_general_settings(),
    butterfly_state(),
    butterfly_timepoints(),
    butterfly_visualization(),
    butterfly_zoom(),
    butterfly_labels_adjustement()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    butterfly_plot_panel(),
    butterfly_debug()
  )
)

butterfly_general_settings <- function() HaDeX_plotSettingsSection(
  checkboxInput_h(inputId = "butt_theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = "butt_fractional",
                  label = "Fractional values",
                  value = FALSE)
)

butterfly_state <- function() HaDeX_plotSettingsSection(
  title = "State",
  selectInput_h(inputId = "butt_state",
                label = "Choose state:",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160"),
)

butterfly_timepoints <- function() HaDeX_plotSettingsSection(
  title = "Timepoints",
  fluidRow(
    column(width = 6,
           checkboxGroupInput_h(inputId = "butt_timepoints",
                                label = "Show time points: ",
                                choices = c(0.167, 1, 5, 25, 120, 1440),
                                selected = c(0.167, 1, 5, 25, 120, 1440))
    ),
    column(width = 6,
           div(id = "butt_time_0_part",
               selectInput_h(inputId = "butt_time_0",
                             label = "TIME IN",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 0.001)
           ),
           div(id = "butt_time_100_part",
               selectInput_h(inputId = "butt_time_100",
                             label = "TIME OUT",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 1440)
           )
    )
  )
)

butterfly_visualization <- function() HaDeX_plotSettingsSection(
  title = "Visualization",
  selectInput_h(inputId = "butt_uncertainty",
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon")
)

butterfly_zoom <- function() HaDeX_plotSettingsSection(
  title = "Zoom",
  sliderInput(inputId = "butt_x_range",
              label = "Choose x range for butterfly plot:",
              min = 1,
              max = 41,
              value = c(1, 41),
              step = 1),
  sliderInput(inputId = "butt_y_range",
              label = "Choose y range for butterfly plot:",
              min = 0,
              max = 100,
              value = c(0, 100),
              step = 1)
)

butterfly_labels_adjustement <- function() HaDeX_plotSettingsSection(
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-butterfly-labels-adjusting-panel"
  ),
  HaDeX_collapsablePanel(
    id = "HaDeX-butterfly-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "butterfly_plot_title",
                       label = "Butterfly plot title:",
                       value = ""),
             textInput(inputId = "butterfly_plot_x_label",
                       label = "Butterfly plot axis x label:",
                       value = "Peptide ID"),
             textInput(inputId = "butterfly_plot_y_label",
                       label = "Butterfly plot axis y label:",
                       value = "Deuterium uptake [Da]")),
      column(width = 2,
             numericInput_h(inputId = "butterfly_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "butterfly_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "butterfly_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)


butterfly_plot_panel <- function() tabsetPanel(
  tabPanel("Butterfly plot",
           plotOutput_h("butterflyPlot", hover = hoverOpts("butterflyPlot_hover", delay = 10, delayType = "debounce")),
           downloadButton("butterflyPlot_download_button",
                          "Save chart (.svg)")),
  tabPanel("Data",
           DT::dataTableOutput("butterflyPlot_data"),
           p(
             "The empty values (e.q. `Frac DU`) means there was not sufficient data for this peptide.",
             "Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )
  )
)

butterfly_debug <- function() uiOutput("butterflyPlot_debug")

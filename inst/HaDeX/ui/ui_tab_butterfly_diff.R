tab_butterfly_diff <- function() HaDeX_plotTab(
  
  title = "Butterfly differential plot",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    butterfly_diff_general_settings(),
    butterfly_diff_state(),
    butterfly_diff_timepoints(),
    butterfly_diff_test(),
    butterfly_diff_visualization(),
    butterfly_diff_zoom(),
    butterfly_diff_labels_adjustement()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    butterfly_diff_plot_panel(),
    butterfly_diff_debug()
  )
)

butterfly_diff_general_settings <- function() HaDeX_plotSettingsSection(
  
  checkboxInput_h(inputId = "butt_diff_theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  
  checkboxInput_h(inputId = "butt_diff_fractional",
                  label = "Fractional values",
                  value = FALSE)
)

butterfly_diff_state <- function() HaDeX_plotSettingsSection(
  
  title = "States",
  
  p("Differential plot presents the uptake difference between State 1 and State 2."),
  splitLayout(selectInput_h(inputId = "butt_diff_state_1",
                            label = "State 1",
                            choices = c("CD160", "CD160_HVEM")),
              selectInput_h(inputId = "butt_diff_state_2",
                            label = "State 2",
                            choices = c("CD160_HVEM", "CD160"))
  )
)

butterfly_diff_timepoints <- function() HaDeX_plotSettingsSection(
  
  title = "Timepoints",
  
  fluidRow(
    column(width = 6,
           checkboxGroupInput_h(inputId = "butt_diff_timepoints",
                                label = "Show time points: ",
                                choices = c(0.167, 1, 5, 25, 120, 1440),
                                selected = c(0.167, 1, 5, 25, 120, 1440)
           )
    ),
    column(width = 6,
           div(id = "butt_diff_time_0_part",
               selectInput_h(inputId = "butt_diff_time_0",
                             label = "TIME IN",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 0.001)
           ),
           div(id = "butt_diff_time_100_part",
               selectInput_h(inputId = "butt_diff_time_100",
                             label = "TIME OUT",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 1440)
           )
    )
  )
)

butterfly_diff_test <- function() HaDeX_plotSettingsSection(
  
  title = "Test",
  
  fluidPage(
    fluidRow(
      column(
        width = 6, 
        checkboxInput_h(inputId = "butt_diff_show_houde",
                        label = "Houde test",
                        value = FALSE),
        checkboxInput_h(inputId = "butt_diff_show_tstud", 
                        label = "t-Student test", 
                        value = FALSE)
      ),
      column(
        width = 6, 
        selectInput_h(inputId = "butt_diff_confidence_level",
                      label = "Select confidence level:",
                      choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                      selected = 0.98)
      )
    )
  )

)

butterfly_diff_visualization <- function() HaDeX_plotSettingsSection(
  
  title = "Visualization",
  
  selectInput_h(inputId = "butt_diff_uncertainty",
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon"),
 
)

butterfly_diff_zoom <- function() HaDeX_plotSettingsSection(
  
  title = "Zoom",
  
  sliderInput(inputId = "butt_diff_x_range",
              label = "Choose x range for butterfly plot:",
              min = 1,
              max = 41,
              value = c(1, 41),
              step = 1),
  sliderInput(inputId = "butt_diff_y_range",
              label = "Choose y range for butterfly plot:",
              min = -2,
              max = 2,
              value = c(-2, 2),
              step = 1)
)

butterfly_diff_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-butterfly-diff-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    id = "HaDeX-butterfly-diff-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "butterflyDifferential_plot_title",
                       label = "Butterfly plot title:",
                       value = ""),
             textInput(inputId = "butterflyDifferential_plot_x_label",
                       label = "Butterfly plot axis x label:",
                       value = "Peptide ID"),
             textInput(inputId = "butterflyDifferential_plot_y_label",
                       label = "Butterfly plot axis y label:",
                       value = "Deuterium uptake difference [Da])")),
      column(width = 2,
             numericInput_h(inputId = "butterflyDifferential_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "butterflyDifferential_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "butterflyDifferential_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

butterfly_diff_plot_panel <- function() tabsetPanel(
  
  tabPanel("Butterfly differential plot",
           plotOutput_h("butterflyDifferentialPlot", hover = hoverOpts("butterflyDifferentialPlot_hover", delay = 10, delayType = "debounce")),
           p("The empty values (e.q. `Frac DU`) mean there was not sufficient data for this peptide."),
           downloadButton("butterflyDifferentialPlot_download_button",
                          "Save chart (.svg)")
  ),
  
  tabPanel("Data",
           DT::dataTableOutput("butterflyDifferentialPlot_data"),
           p(
             "The table presents data from the chosen x plot range.",
             "The empty values (e.q. `Frac Diff DU`) mean there was not sufficient data for this peptide. There is a possibility that the measurement result is available for only one state of the peptide.",
             "Abbreviations from the table: Diff DU - differential deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )
  )
)

butterfly_diff_debug <- function() uiOutput("butterflyDifferentialPlot_debug")

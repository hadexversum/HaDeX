tab_volcano <- function() HaDeX_plotTab(
  
  title = "Volcano plot",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    volcano_state(),
    volcano_timepoints(),
    volcano_test(),
    volcano_zoom(),
    volcano_labels_adjustement()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    volcano_plot_panel(),
    volcano_debug()
  )
)

volcano_state <- function() HaDeX_plotSettingsSection(
  
  title = "States",
  
  p("Volcano plot presents the uptake difference between State 1 and State 2."),
  splitLayout(selectInput_h(inputId = "vol_state_1",
                            label = "State 1",
                            choices = c("CD160", "CD160_HVEM")),
              selectInput_h(inputId = "vol_state_2",
                            label = "State 2",
                            choices = c("CD160_HVEM", "CD160"))
  ),
  h5("Displayed values: "),
  # checkboxInput_h(inputId = "vol_theoretical",
  #                 label = "Theoretical calculations",
  #                 value = F),
  checkboxInput_h(inputId = "vol_fractional",
                label = "Fractional values",
                value = F),
  div(id = "vol_control_part",
      splitLayout(
        selectInput_h(inputId = "vol_time_0",
                      label = "Deut 0%", 
                      choices = c("0", "1", "5", "25", "1440")),
        selectInput_h(inputId = "vol_time_100", 
                      label = "Deut 100%", 
                      choices = c("0", "1", "5", "25", "1440"))
        )
      )
)

volcano_timepoints <- function() HaDeX_plotSettingsSection(
  
  title = "Timepoints",
  
  fluidRow(
    column(width = 6,
           checkboxGroupInput_h(inputId = "vol_timepoints",
                                label = "Show time points: ",
                                choices = c(0.167, 1, 5, 25, 120, 1440),
                                selected = c(0.167, 1, 5, 25, 120, 1440)),
           checkboxInput_h(inputId = "vol_color_times",
                           label = "distinquish by color",
                           value = TRUE)
    ),
    column(width = 6,
           selectInput_h(inputId = "vol_confidence_level",
                         label = "Confidence level:",
                         choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                         selected = 0.98),
           selectInput_h(inputId = "vol_interval",
                         label = "Show confidence limit for: ",
                         choices = c("All time points", "Selected time points"),
                         selected = "All time points")
           
    )
  ),
  sliderInput(inputId = "vol_sequence_range",
              label = "Select subregion of the protein sequence: ",
              min = 1, 
              max = 300, 
              value = c(1, 300))
)

volcano_test <- function() HaDeX_plotSettingsSection(
  
  title = "Test",
  
  # selectInput_h(inputId = "vol_test_type",
  #               label = "Select test type:",
  #               choices = c("Houde test for selected time points" = 1, "Houde test all time points" = 2), #, "semi-parametric test" = 3),
  #               selected = 1),
  selectInput_h(inputId = "vol_p_adjustment_method",
                label = "Choose method of adjustment:",
                choices = c("none", "BH", "bonferroni"),
                selected = "none")
)


volcano_zoom <- function() HaDeX_plotSettingsSection(
  
  title = "Zoom",
  
  sliderInput(inputId = "vol_x_range",
              label = "Choose x range for volcano plot:",
              min = -1,
              max = 1,
              value = c(-1, 1),
              step = 0.1),
  sliderInput(inputId = "vol_y_range",
              label = "Choose y range for volcano plot:",
              min = 0,
              max = 15,
              value = c(0, 15),
              step = 1)
)

volcano_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-volcano-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    id = "HaDeX-volcano-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "volcano_plot_title",
                       label = "Volcano plot title:",
                       value = ""),
             textInput(inputId = "volcano_plot_x_label",
                       label = "Volcano plot axis x label:",
                       value = "Mass difference [Da]"),
             textInput(inputId = "volcano_plot_y_label",
                       label = "Volcano plot axis y label:",
                       value = "-log(P value)")),
      column(width = 2,
             numericInput_h(inputId = "volcano_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "volcano_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "volcano_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)
  
volcano_plot_panel <- function() tabsetPanel(
  
  tabPanel("Volcano plot",
           plotOutput_h("volcanoPlot", width = "80%", height = "800px", hover = hoverOpts("volcanoPlot_hover", delay = 10, delayType = "debounce")),
           p(textOutput("vol_thresholds")),
           p("For more information see about hybrid testing see: Hageman, T. S. & Weis, D. D. Reliable Identification of Significant Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019)."),
           downloadButton("volcanoPlot_download_button",
                          "Save chart (.svg)")
  ),
  tabPanel("Data",
           DT::dataTableOutput("volcanoPlot_data")
  )
)

volcano_debug <- function() uiOutput("volcanoPlot_debug")

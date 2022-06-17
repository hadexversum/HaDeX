tab_manhattan <- function() HaDeX_plotTab(
  
  title = "Manhattan plot",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    manhattan_state(),
    manhattan_test(),
    manhattan_times(),
    manhattan_labels_adjustement()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    manhattan_plot_panel(),
    manhattan_debug()
  )
)


manhattan_state <- function() HaDeX_plotSettingsSection(
  
  title = "States",
  
  p("Manhattan plot presents the t-Student test results for difference between two biological states."),
  
  splitLayout(
    
    selectInput_h(inputId = "man_state_1",
                  label = "State 1",
                  choices = c("CD160", "CD160_HVEM")),
              
    selectInput_h(inputId = "man_state_2",
                  label = "State 2",
                  choices = c("CD160_HVEM", "CD160"))
  ),
  
  checkboxInput_h(inputId = "man_show_position",
                  label = "Show peptide length and position in the sequence? ",
                  value = TRUE)
  
)

##

manhattan_test <- function() HaDeX_plotSettingsSection(
  
  title = "t-Student Test",
  
  selectInput_h(inputId = "man_confidence_level",
                label = "Confidence level:",
                choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                selected = 0.98),
  
  selectInput_h(inputId = "man_p_adjustment_method",
                label = "Choose method of adjustment:",
                choices = c("none", "BH", "bonferroni"),
                selected = "none")
)


##

manhattan_times <- function() HaDeX_plotSettingsSection(
  
  title = "Time points",
  
  splitLayout(
    
    checkboxGroupInput_h(inputId = "man_times",
                         label = "Show time points: ",
                         choices = c(0.167, 1, 5, 120, 1440),
                         selected = c(0.167, 1, 5, 120, 1440)),
    
    checkboxInput_h(inputId = "man_separate_times",
                    label = "Show times separately? ",
                    value = FALSE)
    
  )
  
)

manhattan_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-manhattan-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    id = "HaDeX-manhattan-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "man_plot_title",
                       label = "Manhattan plot title:",
                       value = "Deuterium uptake difference"),
             textInput(inputId = "man_plot_x_label",
                       label = "Manhattan plot axis x label:",
                       value = "Peptide position"),
             textInput(inputId = "man_plot_y_label",
                       label = "Manhattan plot axis y label:",
                       value = "log(P value)")),
      column(width = 2,
             numericInput_h(inputId = "man_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "man_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "man_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

##########
## MAIN ##
##########

manhattan_plot_panel <- function() tabsetPanel(
  
  tabPanel("Manhattan plot",
           plotOutput_h("manhattanPlot", width = "80%", height = "800px", hover = hoverOpts("manhattanPlot_hover", delay = 10, delayType = "debounce")),
           downloadButton("manhattanPlot_download_button",
                          "Save chart (.svg)")),
  
  tabPanel("Data",
           DT::dataTableOutput("manhattanPlot_data"))
)

manhattan_debug <- function() uiOutput("manhattanPlot_debug")
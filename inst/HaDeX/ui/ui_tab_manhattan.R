tab_manhattan <- function() HaDeX_plotTab(
  
  title = "Manhattan plot",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    manhattan_state(),
    manhattan_test(),
    manhattan_times()
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
  )
  
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


manhattan_plot_panel <- function() tabsetPanel(
  
  tabPanel("Manhattan plot",
           plotOutput_h("manhattanPlot", width = "80%", height = "800px", hover = hoverOpts("manhattanPlot_hover", delay = 10, delayType = "debounce")),
           downloadButton("manhattanPlot_download_button",
                          "Save chart (.svg)")),
  
  tabPanel("Data",
           DT::dataTableOutput("manhattanPlot_data"))
)

manhattan_debug <- function() uiOutput("manhattanPlot_debug")
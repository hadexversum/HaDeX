tab_woods <- tabPanel(
  title = "Comparison and Woods plot",
  fillRow(
    class = "HaDeX-plot-tab-content",
    flex = c(3, 7),
    wellPanel(
      class = "HaDeX-tab-content-element HaDeX-plot-settings-panel",
      fillCol(
        flex = NA,
        h4("Select parameters for the plot"),
        checkboxInput_h(inputId = "theory",
                        label = "Theoretical calculations",
                        value = FALSE),
        checkboxInput_h(inputId = "comp_fractional", 
                        label = "Fractional values",
                        value = FALSE),
        
        h5("Comparison plot parameters"),
        p("Choose time parameters:"),
        splitLayout(
          div(id = "time_0_part",
              selectInput_h(inputId = "time_0",
                            label = "IN",
                            choices = c("0", "1", "5", "25", "1440"))
          ),
          selectInput_h(inputId = "time_t",
                        label = "CHOSEN",
                        choices = c("0", "1", "5", "25", "1440")),
          div(id = "time_100_part",
              selectInput_h(inputId = "time_100",
                            label = "OUT",
                            choices = c("0", "1", "5", "25", "1440"))
          )
        ),
        p("Choose states for comparison:"),
        fluidRow(
          column(6,
                 checkboxGroupInput_h(inputId = "compare_states",
                                      label = "",
                                      choices = c("CD160", "CD160VEM"),
                                      selected = c("CD160", "CD160VEM")),
                 class = "states-to-compare-column"),
          column(6,
                 helper(tags$button("Adjust colors",
                                    class = "collapse-button",
                                    `data-toggle`="collapse",
                                    `data-target`="#colorss"),
                        content = "adjust_colors", type = "markdown", buttonLabel = "Okay",
                        easyClose = TRUE, colour = "#F8F1FF"),
                 tags$div(
                   class = "hideable",
                   id = "colorss",
                   uiOutput("states_colors")
                 ),
                 class = "states-colors-column"
          )
          
        ),
        h5("Woods plot parameters"),
        p("Differential plot presents the uptake difference between State 1 and State 2."),
        splitLayout(
          selectInput_h(inputId = "diff_state_1",
                        label = "State 1",
                        choices = c("CD160", "CD160VEM")),
          selectInput_h(inputId = "diff_state_2",
                        label = "State 2",
                        choices = c("CD160", "CD160VEM"))
        ),
        splitLayout(
          selectInput_h(inputId = "confidence_level",
                        label = "Confidence limit 1:",
                        choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                        selected = 0.98),
          selectInput_h(inputId = "confidence_level_2",
                        label = "Confidence limit 2:",
                        choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                        selected = 0.99)
        ),
        h5("Zoom"),
        sliderInput(inputId = 'comp_plot_y_range',
                    label = 'Choose y range for comparison plot:',
                    min = -200,
                    max = 200,
                    value = c(0, 120),
                    step = 10,
                    width = "100%"),
        sliderInput(inputId = 'woods_plot_y_range',
                    label = 'Choose y range for Woods plot:',
                    min = -200,
                    max = 200,
                    value = c(-50, 50),
                    step = 10),
        sliderInput(inputId = 'plot_x_range',
                    label = 'Choose x range for both plots:',
                    min = 0,
                    max = 300,
                    value = c(0, 300),
                    # ticks = seq(0, 300, 1) # this breaks shiny 1.5
        ),
        
        tags$button("Adjust labels",
                    class = "collapse-button",
                    `data-toggle`="collapse",
                    `data-target`="#labs"),
        tags$div(
          class = "hideable",
          id = "labs",
          fluidRow(
            column(width = 10,
                   textInput(inputId = "comparison_plot_title",
                             label = "Comparison plot title:",
                             value = ""),
                   textInput(inputId = "comparison_plot_x_label",
                             label = "Comparison plot axis x label:",
                             value = "Position in sequence"),
                   textInput(inputId = "comparison_plot_y_label",
                             label = "Comparison plot axis y label:",
                             value = "")
            ),
            column(width = 2,
                   numericInput_h(inputId = "comparison_plot_title_size",
                                  label = "Size:",
                                  value = 15,
                                  min = 5),
                   numericInput_h(inputId = "comparison_plot_x_label_size",
                                  label = "Size:",
                                  value = 15,
                                  min = 5),
                   numericInput_h(inputId = "comparison_plot_y_label_size",
                                  label = "Size:",
                                  value = 15,
                                  min = 5)
                   
            )
          )
          ,
          fluidRow(
            column(width = 10,
                   textInput(inputId = "woods_plot_title",
                             label = "Woods plot title:",
                             value = ""),
                   textInput(inputId = "woods_plot_x_label",
                             label = "Woods plot axis x label:",
                             value = "Position in sequence"),
                   textInput(inputId = "woods_plot_y_label",
                             label = "Woods plot axis y label:",
                             value = "")),
            column(width = 2,
                   numericInput_h(inputId = "woods_plot_title_size",
                                  label = "Size:",
                                  value = 15,
                                  min = 5),
                   numericInput_h(inputId = "woods_plot_x_label_size",
                                  label = "Size:",
                                  value = 15,
                                  min = 5),
                   numericInput_h(inputId = "woods_plot_y_label_size",
                                  label = "Size:",
                                  value = 15,
                                  min = 5)
                   
            )
          ),
          p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
        )
      )
    ),
    div(
      class = "HaDeX-tab-content-element HaDeX-plot-column-panel",
      fillCol(
        flex = NA,
        tabsetPanel(
          tabPanel("Comparison plot",
                   br(),
                   plotOutput_h("comparisonPlot", hover = hoverOpts("comparisonPlot_hover", delay = 10, delayType = "debounce")),
                   downloadButton("comparisonPlot_download_button",
                                  "Save chart (.svg)")
          ),
          tabPanel("Data",
                   br(),
                   DT::dataTableOutput("comparisonPlot_data"),
                   br(),
                   h4("The empty values (e.q. `Frac DU`) mean there was not sufficient data for this peptide."),
                   h4("Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value.")
          )
          
        ),
        uiOutput("comparisonPlot_debug"),
        br(),
        
        ##### WOODS PLOT ##### 
        
        tabsetPanel(
          tabPanel("Woods plot",
                   br(),
                   div(style = "position:relative",
                       plotOutput_h("differentialPlot", hover = hoverOpts("differentialPlot_hover", delay = 10, delayType = "debounce")),
                       uiOutput("differentialPlot_debug")),
                   downloadButton("differentialPlot_download_button",
                                  "Save chart (.svg)")),
          tabPanel("Data",
                   br(),
                   DT::dataTableOutput("differentialPlot_data"),
                   br(),
                   h4("The empty values (e.q. `Frac Diff DU`) mean there was not sufficient data for this peptide. There is a possibility that the measurement result is available for only one state of the peptide."),
                   h4("Abbreviations from the table: Diff DU - differential deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value.")
          )
        )
      )
    )
  )
)
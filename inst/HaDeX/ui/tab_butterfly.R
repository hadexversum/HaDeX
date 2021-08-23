tab_butterfly <- tabPanel("Butterfly plot",
                    br(),
                    sidebarPanel(
                      class = "scrollable",
                      
                      ##### SETTINGS ##### 
                      
                      h3("Select parameters for the plot."),
                      fluidRow(
                        column(width = 6,
                               checkboxInput_h(inputId = "butt_theory",
                                               label = "Theoretical calculations",
                                               value = FALSE),
                               checkboxInput_h(inputId = "butt_fractional",
                                               label = "Fractional values",
                                               value = FALSE)
                        ),
                        column(width = 6,
                               selectInput_h(inputId = "butt_state",
                                             label = "Choose state:",
                                             choices = c("CD160", "CD160_HVEM"),
                                             selected = "CD160"),
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               checkboxGroupInput_h(inputId = "butt_timepoints",
                                                    label = "Show time points: ",
                                                    choices = c(0.167, 1, 5, 25, 120, 1440),
                                                    selected = c(0.167, 1, 5, 25, 120, 1440)),
                               h4("Visualization:"),
                               selectInput_h(inputId = "butt_uncertainty",
                                             label = "Show uncertainty as:",
                                             choices = c("ribbon", "bars", "bars + line"),
                                             selected = "ribbon")),
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
                      ),
                      h4("Zoom:"),
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
                                  step = 1),
                      tags$button("Adjust labels",
                                  class = "collapse-button",
                                  `data-toggle`="collapse",
                                  `data-target`="#butt_labs"),
                      tags$div(
                        class = "hideable",
                        id = "butt_labs",
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
                        h4("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
                      )
                    ),
                    mainPanel(
                      class = "scrollable",
                      tabsetPanel(
                        
                        ##### BUTTERFLY PLOT ##### 
                        
                        tabPanel("Butterfly plot",
                                 br(),
                                 plotOutput_h("butterflyPlot", hover = hoverOpts("butterflyPlot_hover", delay = 10, delayType = "debounce")),
                                 downloadButton("butterflyPlot_download_button",
                                                "Save chart (.svg)")),
                        tabPanel("Data",
                                 br(),
                                 DT::dataTableOutput("butterflyPlot_data"),
                                 br(),
                                 h4("The empty values (e.q. `Frac DU`) means there was not sufficient data for this peptide."),
                                 h4("Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value.")
                        )
                      ),
                      uiOutput("butterflyPlot_debug"),
                      br()
                    )
)
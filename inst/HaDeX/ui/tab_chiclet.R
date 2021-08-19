tab_chiclet <- tabPanel("Chiclet plot",
         br(),
         tabsetPanel(
           tabPanel("Chiclet plot",
                    br(),
                    sidebarPanel(
                      class = "scrollable", 
                      
                      ##### SETTINGS #####
                      h3("Select parameters for the plot."),
                      fluidRow(
                        column(width = 6,
                               checkboxInput_h(inputId = "chic_theory",
                                               label = "Theoretical calculations",
                                               value = FALSE),
                               checkboxInput_h(inputId = "chic_fractional",
                                               label = "Fractional values",
                                               value = FALSE)
                        ),
                        column(width = 6,
                               selectInput_h(inputId = "chic_state",
                                             label = "Choose state:",
                                             choices = c("CD160", "CD160_HVEM"),
                                             selected = "CD160"),
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               checkboxGroupInput_h(inputId = "chic_timepoints",
                                                    label = "Show time points: ",
                                                    choices = c(0.167, 1, 5, 25, 120, 1440),
                                                    selected = c(0.167, 1, 5, 25, 120, 1440)),
                               h4("Visualization:"),
                               checkboxInput_h(inputId = "chic_show_uncertainty",
                                               label = "Show uncertainty",
                                               value = TRUE)),
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
                      ),
                      h4("Zoom:"),
                      sliderInput(inputId = "chic_x_range",
                                  label = "Choose x range for chiclet plot:",
                                  min = 1,
                                  max = 41,
                                  value = c(1, 41),
                                  step = 1),
                      tags$button("Adjust labels",
                                  class = "collapse-button",
                                  `data-toggle`="collapse",
                                  `data-target`="#chic_labs"),
                      tags$div(
                        class = "hideable",
                        id = "chic_labs",
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
                        h4("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
                      )
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Chiclet plot",
                                 br(),
                                 plotOutput_h("chicletPlot", hover = hoverOpts("chicletPlot_hover", delay = 10, delayType = "debounce")),
                                 uiOutput("chicletPlot_debug"),
                                 downloadButton("chicletPlot_download_button",
                                                "Save chart (.svg)")
                        ),
                        tabPanel("Data",
                                 br(),
                                 DT::dataTableOutput("chicletPlot_data"),
                                 h4("Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."),
                                 br()
                        )
                      )
                    )
           ),
           tabPanel("Chiclet differential plot",
                    br(),
                    sidebarPanel(
                      class = "scrollable", 
                      
                      ##### SETTINGS #####
                      h3("Select parameters for the plot."),
                      fluidRow(
                        column(width = 6,
                               checkboxInput_h(inputId = "chic_diff_theory",
                                               label = "Theoretical calculations",
                                               value = FALSE),
                               checkboxInput_h(inputId = "chic_diff_fractional",
                                               label = "Fractional values",
                                               value = FALSE)
                        )
                      ),
                      h5("Differential plot presents the uptake difference [Da] between State 1 and State 2."),
                      splitLayout(selectInput_h(inputId = "chic_diff_state_1",
                                                label = "State 1",
                                                choices = c("CD160", "CD160_HVEM")),
                                  selectInput_h(inputId = "chic_diff_state_2",
                                                label = "State 2",
                                                choices = c("CD160_HVEM", "CD160"))
                      ),
                      fluidRow(
                        column(width = 6,
                               checkboxGroupInput_h(inputId = "chic_diff_timepoints",
                                                    label = "Show time points: ",
                                                    choices = c(0.167, 1, 5, 25, 120, 1440),
                                                    selected = c(0.167, 1, 5, 25, 120, 1440)),
                               h4("Visualization:"),
                               checkboxInput_h(inputId = "chic_diff_show_uncertainty",
                                               label = "Show uncertainty",
                                               value = TRUE)
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
                      ),
                      h4("Zoom:"),
                      sliderInput(inputId = "chic_diff_x_range",
                                  label = "Choose x range for chiclet plot:",
                                  min = 1,
                                  max = 41,
                                  value = c(1, 41),
                                  step = 1),
                      tags$button("Adjust labels",
                                  class = "collapse-button",
                                  `data-toggle`="collapse",
                                  `data-target`="#chic_diff_labs"),
                      tags$div(
                        class = "hideable",
                        id = "chic_diff_labs",
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
                        h4("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
                      ),
                      br()
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Chiclet plot",
                                 br(),
                                 plotOutput_h("chicletDifferentialPlot", hover = hoverOpts("chicletDifferentialPlot_hover", delay = 10, delayType = "debounce")),
                                 uiOutput("chicletDifferentialPlot_debug"),
                                 downloadButton("chicletDifferentialPlot_download_button",
                                                "Save chart (.svg)")
                        ),
                        tabPanel("Data",
                                 br(),
                                 DT::dataTableOutput("chicletDifferentialPlot_data"),
                                 br(),
                                 h4("The table presents data from the chosen x plot range."),
                                 h4("Abbreviations from the table: Diff DU - differential deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."),
                                 br()
                        )
                      )
                      
                    )
           )
         )
)
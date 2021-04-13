source("data-work.R")

options(spinner.color="#715D91")

ui <- fluidPage(theme = "HaDeX_theme.css",
#                 #titlePanel("HaDeX"), #: analysis of data from hydrogen deuterium exchange-mass spectrometry"),
                title = "HaDeX",
                useShinyjs(),
                tags$head(includeScript("ga.js"),
                          tags$link(rel="stylesheet",
                                    href="mobile_version.css",
                                    media="screen and (max-width: 600px)"),
                          tags$script(type="text/javascript",
                                      src="detect-element-resize.js")),

                tags$div(
                  class = "site-backbone",
                  tags$div(
                    class = "logo-panel",
                    img(src = "mock_logo.png", class = "logo")
                  ),
                  tabsetPanel(type = "pills",
                              tabPanel("Start",
                                       h3("Welcome to HaDeX!"),
                                       h4("Thank you for using our tool."),
                                       #h4("For any additional information, please see the documnetation available online.", a(href = "https://hadexversum.github.io/HaDeX/", "Check it out!")),
                                       h4("Questions/feature requests/commercial applications: hadex@ibb.waw.pl"),
                                       includeMarkdown("readmes/about.md"),
                                       img(class='funding-icons',
                                           src='funding_icons.png'),
                                       br()
                              ),
                              tabPanel("Input data",
                                       br(),
                                       h4("Upload your file. Otherwise, you will see the example data."),
                                       h4("Currently HaDeX is limited to `cluster` files from DynamX 3.0 or 2.0 and `tables` file from  HDeXaminer"),
                                       h4("Accepted file extensions: .csv, .xls, .xlsx. "),
                                       h4("The supplied file should contain at least two repetitions of the measurement for the uncertainty to be calculated."),
                                       h4("If the supplied file contains modified peptides, maximal exchange control cannot be applied."),
                                       #
                                       fluidRow(
                                         column(3,
                                                fileInput(
                                                  inputId = "data_file",
                                                  label = "Choose file:",
                                                  multiple = FALSE,
                                                  accept = c(".csv", ".xlsx", ".xls"),
                                                  placeholder = "No file selected")),
                                         column(4,
                                                h5("File status:"),
                                                tags$div(
                                                  class = "file-status-message",
                                                  withSpinner(textOutput("data_file_info"))
                                                ))
                                       ),
                                       #
                                       h4("Please be aware that loading data (including example file) may take a while. Be patient."),
                                       h4("For the program to behave correctly, please make sure supplied file fulfills following requirements:"),
                                       tags$button("Show requirements",
                                                   class = "collapse-button",
                                                   style = "width: unset",
                                                   `data-toggle`="collapse",
                                                   `data-target`="#reqs"),
                                       tags$div(
                                         id = "reqs",
                                         style = "width: min-content",
                                         class = "hideable",
                                         tableOutput("file_req")),
                                       h3("Settings"),
                                       h4("Values chosen here are propagated into all of the tables for coherent results."),
                                       fluidRow(
                                         column(4,
                                                selectInput_h(inputId = "chosen_protein",
                                                              label = "Choose protein: ",
                                                              choices = c("db_CD160"),
                                                              width = "100%"),
                                                selectInput_h(inputId = "chosen_control",
                                                              label = "Choose maximal exchange control for chosen protein: ",
                                                              choices = c("db_CD160 | CD160 | 1440"),
                                                              width = "100%"),
                                                numericInput_h(inputId = "deut_part",
                                                               label = "Choose D20 concentration [%]: ",
                                                               value = 100,
                                                               min = 0, max = 100, step = 1,
                                                               width = "100%"),
                                                numericInput_h(inputId = "sequence_start_shift",
                                                               label = "Move sequence start:",
                                                               value = 1, step = 1,
                                                               width = "100%"),
                                                numericInput_h(inputId = "sequence_length",
                                                               label = "Correct sequence length:",
                                                               value = 300, step = 1,
                                                               width = "100%"),
                                                textOutput("sequence_length_exp_info")
                                         ),
                                         column(2,
                                                br()),
                                         column(4,
                                                hidden(div(id = "examiner_settings",
                                                           h3("File from HDeXaminer detected!"),
                                                           h4("Some of the information from the data file requires your confirmation."),
                                                           h4("For the additional information on how the data from HDeXaminer is processed, check the requirements above. Keep in mind that the MHP value is generated based on the peptide sequence and therefore, may differ from actual value in case of the modifications."),
                                                           numericInput_h(inputId = "examiner_fd_timepoint",
                                                                          label = "FD timepoint [min]:",
                                                                          value = 1440,
                                                                          min = 0,
                                                                          width = "100%"),
                                                           textInput_h(inputId = "exam_protein_name",
                                                                       label = "Protein name:",
                                                                       width = "100%"),
                                                           textInput_h(inputId = "exam_state_name",
                                                                       label = "States names:",
                                                                       width = "100%"),
                                                           checkboxGroupInput_h(inputId = "exam_confidence",
                                                                                label = "Accepted confidence values:",
                                                                                choices = c("High", "Medium", "Low"),
                                                                                selected = c("Medium", "High")),
                                                           actionButton(inputId = "exam_apply_changes",
                                                                        label = "Apply changes to continue"),
                                                           br(),
                                                           h5("The calculated values of MPH might slightly differ based on data used and its precision."),
                                                           a(href="http://www.matrixscience.com/help/aa_help.html", "Used amino mass data"),
                                                           br(),
                                                           br(),
                                                           DT::dataTableOutput("checking_exam_data"),
                                                           br(),
                                                           br(),
                                                           br()
                                                ))
                                         )),
                                       br(),
                                       br(),
                                       br()
                              ),
                              ##
                              tabPanel("Woods plot",
                                       br(),
                                       sidebarPanel(
                                         class = "scrollable",
                                         h3("Select parameters for the plot."),
                                         fluidRow(checkboxInput_h(inputId = "theory",
                                                                  label = "Theoretical calculations",
                                                                  value = FALSE)),
                                         radioButtons_h(inputId = "calc_type",
                                                        label = "Choose values type:",
                                                        choices = c("fractional", "absolute"),
                                                        selected = "fractional"),
                                         h4("Comparison plot parameters:"),
                                         h5("Choose time parameters:"),
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
                                         ##
                                         fluidRow(
                                           column(6,
                                                  h5("Choose states for comparison:"),
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
                                         ##
                                         h4("Woods plot parameters:"),
                                         splitLayout(
                                           selectInput_h(inputId = "state_first",
                                                         label = "State 1",
                                                         choices = c("CD160", "CD160VEM")),
                                           selectInput_h(inputId = "state_second",
                                                         label = "State 2",
                                                         choices = c("CD160", "CD160VEM"))
                                         ),
                                         splitLayout(
                                           selectInput_h(inputId = "confidence_limit",
                                                         label = "Confidence limit 1:",
                                                         choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                                         selected = 0.98),
                                           selectInput_h(inputId = "confidence_limit_2",
                                                         label = "Confidence limit 2:",
                                                         choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                                         selected = 0.99)
                                         ),
                                         ##
                                         h4("Zoom:"),
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
                                         ##
                                         tags$button("Adjust labels",
                                                     class = "collapse-button",
                                                     `data-toggle`="collapse",
                                                     `data-target`="#labs"),
                                         tags$div(
                                           class = "hideable",
                                           id = "labs",
                                           textInput(inputId = "comparison_plot_title",
                                                     label = "Comparison plot title:",
                                                     value = ""),
                                           textInput(inputId = "comparison_plot_x_label",
                                                     label = "Comparison plot axis x label:",
                                                     value = "Position in sequence"),
                                           textInput(inputId = "comparison_plot_y_label",
                                                     label = "Comparison plot axis y label:",
                                                     value = ""),
                                           textInput(inputId = "woods_plot_title",
                                                     label = "Woods plot title:",
                                                     value = ""),
                                           textInput(inputId = "woods_plot_x_label",
                                                     label = "Woods plot axis x label:",
                                                     value = "Position in sequence"),
                                           textInput(inputId = "woods_plot_y_label",
                                                     label = "Woods plot axis y label:",
                                                     value = "")
                                         )
                                       ),
                                       mainPanel(
                                         class = "scrollable",
                                         tabsetPanel(
                                           tabPanel("Comparison plot",
                                                    br(),
                                                    plotOutput_h("comparisonPlot", hover = hoverOpts("comparisonPlot_hover", delay = 10, delayType = "debounce")),
                                                    downloadButton("comparisonPlot_download_button",
                                                                   "Save chart (.svg)")),
                                           tabPanel("Data",
                                                    br(),
                                                    DT::dataTableOutput("comparisonPlot_data"),
                                                    br(),
                                                    h4("The empty values (e.q. `Frac Exch`) means there was not sufficient data for this peptide."))),
                                         uiOutput("comparisonPlot_debug"),
                                         br(),
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
                                                    h4("The empty values (e.q. `Diff Frac Exch`) means there was not sufficient data for this peptide.")))


                                       )
                              ),
                              ##
                              tabPanel("Butterfly plot",
                                       br(),
                                       tabsetPanel(
                                              tabPanel("Butterfly Plot",
                                                       br(),
                                                       sidebarPanel(
                                                         class = "scrollable",
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
                                                                              selected = c(0.167, 1, 5, 25, 120, 1440))),
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
                                                           textInput(inputId = "butterfly_plot_title",
                                                                     label = "Butterfly plot title:",
                                                                     value = ""),
                                                           textInput(inputId = "butterfly_plot_x_label",
                                                                     label = "Butterfly plot axis x label:",
                                                                     value = "Peptide ID"),
                                                           textInput(inputId = "butterfly_plot_y_label",
                                                                     label = "Butterfly plot axis y label:",
                                                                     value = "Deuterim uptake [Da])")
                                                         )
                                                       ),
                                                       mainPanel(
                                                         class = "scrollable",
                                                         tabsetPanel(
                                                           tabPanel("Butterfly plot",
                                                                    br(),
                                                                    plotOutput_h("butterflyPlot", hover = hoverOpts("butterflyPlot_hover", delay = 10, delayType = "debounce")),
                                                                    downloadButton("butterflyPlot_download_button",
                                                                                   "Save chart (.svg)")),
                                                           tabPanel("Data",
                                                                    br(),
                                                                    DT::dataTableOutput("butterflyPlot_data"),
                                                                    br(),
                                                                    h4("The empty values (e.q. `Frac Exch`) means there was not sufficient data for this peptide."))),
                                                         uiOutput("butterflyPlot_debug"),
                                                         br()
                                                       )),
                                              tabPanel("Butterfly Differential Plot",
                                                       br(),
                                                       sidebarPanel(
                                                         class = "scrollable",
                                                         h3("Select parameters for the plot."),
                                                         fluidRow(
                                                           column(width = 6,
                                                                  checkboxInput_h(inputId = "butt_diff_theory",
                                                                                  label = "Theoretical calculations",
                                                                                  value = FALSE),
                                                                  checkboxInput_h(inputId = "butt_diff_fractional",
                                                                                  label = "Fractional values",
                                                                                  value = FALSE)
                                                           )
                                                         ),
                                                         h5("Differential plot presents the difference between values in State 1 and State 2."),
                                                         splitLayout(selectInput_h(inputId = "butt_diff_state_first",
                                                                                   label = "State 1",
                                                                                   choices = c("CD160", "CD160_HVEM")),
                                                                     selectInput_h(inputId = "butt_diff_state_second",
                                                                                   label = "State 2",
                                                                                   choices = c("CD160_HVEM", "CD160"))
                                                         ),
                                                         fluidRow(
                                                           column(width = 6,
                                                                  checkboxGroupInput_h(inputId = "butt_diff_timepoints",
                                                                                       label = "Show time points: ",
                                                                                       choices = c(0.167, 1, 5, 25, 120, 1440),
                                                                                       selected = c(0.167, 1, 5, 25, 120, 1440))
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
                                                         ),
                                                         h4("Zoom:"),
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
                                                                     step = 1),
                                                         tags$button("Adjust labels",
                                                                     class = "collapse-button",
                                                                     `data-toggle`="collapse",
                                                                     `data-target`="#butt_diff_labs"),
                                                         tags$div(
                                                           class = "hideable",
                                                           id = "butt_diff_labs",
                                                           textInput(inputId = "butterflyDifferential_plot_title",
                                                                     label = "Butterfly plot title:",
                                                                     value = ""),
                                                           textInput(inputId = "butterflyDifferential_plot_x_label",
                                                                     label = "Butterfly plot axis x label:",
                                                                     value = "Peptide ID"),
                                                           textInput(inputId = "butterflyDifferential_plot_y_label",
                                                                     label = "Butterfly plot axis y label:",
                                                                     value = "Deuterium uptake difference [Da])")
                                                         )
                                                       ),
                                                       mainPanel(
                                                         class = "scrollable",
                                                         tabsetPanel(
                                                           tabPanel("Butterfly differential plot",
                                                                    br(),
                                                                    plotOutput_h("butterflyDifferentialPlot", hover = hoverOpts("butterflyDifferentialPlot_hover", delay = 10, delayType = "debounce")),
                                                                    downloadButton("butterflyDifferentialPlot_download_button",
                                                                                   "Save chart (.svg)")
                                                           ),
                                                           tabPanel("Data",
                                                                    br(),
                                                                    DT::dataTableOutput("butterflyDifferentialPlot_data"),
                                                                    br()
                                                             
                                                           )
                                                         )
                                                       )
                                              )
                                              
                                          ),
                                              
                                       ),
                              ##
                              tabPanel("Volcano plot",
                                       br(),
                                       sidebarPanel(
                                         class = "scrollable",
                                         h3("Select parameters for the plot."),
                                         h5("Volcano plot presents the difference between values in State 1 and State 2."),
                                         splitLayout(selectInput_h(inputId = "vol_state_first",
                                                                   label = "State 1",
                                                                   choices = c("CD160", "CD160_HVEM")),
                                                     selectInput_h(inputId = "vol_state_second",
                                                                   label = "State 2",
                                                                   choices = c("CD160_HVEM", "CD160"))
                                                     ),
                                         fluidRow(
                                           column(width = 6,
                                                  selectInput_h(inputId = "vol_confidence_limit",
                                                                label = "Confidence limit:",
                                                                choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                                                selected = 0.98),
                                                  checkboxInput_h(inputId = "vol_p_adjustment",
                                                                  label = "Adjust p value?",
                                                                  value = FALSE)
                                                  )
                                         ),
                                         splitLayout(checkboxGroupInput_h(inputId = "vol_timepoints",
                                                                          label = "Show time points: ",
                                                                          choices = c(0.167, 1, 5, 25, 120, 1440),
                                                                          selected = c(0.167, 1, 5, 25, 120, 1440)),
                                                     selectInput_h(inputId = "vol_interval",
                                                                   label = "Show confidence limit for: ",
                                                                   choices = c("All time points" = 99999, "0.167 min" = 0.167, "1 min" = 1, "5 min" = 5, "25 min" = 25, "120 min" = 120, "1440 min" = 1440),
                                                                   selected = 99999)),
                                         h4("Zoom:"),
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
                                                     step = 1),
                                         tags$button("Adjust labels",
                                                     class = "collapse-button",
                                                     `data-toggle`="collapse",
                                                     `data-target`="#vol_labs"),
                                         tags$div(
                                           class = "hideable",
                                           id = "vol_labs",
                                           textInput(inputId = "volcano_plot_title",
                                                     label = "Volcano plot title:",
                                                     value = ""),
                                           textInput(inputId = "volcano_plot_x_label",
                                                     label = "Volcano plot axis x label:",
                                                     value = "Mass difference [Da]"),
                                           textInput(inputId = "volcano_plot_y_label",
                                                     label = "Volcano plot axis y label:",
                                                     value = "-log(P value)")
                                         )
                                       ),
                                       mainPanel(
                                         class = "scrollable",
                                         tabsetPanel(
                                           tabPanel("Volcano plot",
                                                    br(),
                                                    plotOutput_h("volcanoPlot", width = "80%", height = "800px", hover = hoverOpts("volcanoPlott_hover", delay = 10, delayType = "debounce")),
                                                    downloadButton("volcanoPlot_download_button",
                                                                   "Save chart (.svg)")
                                           ),
                                           tabPanel("Data",
                                                    br(),
                                                    DT::dataTableOutput("volcanoPlot_data"),
                                                    br()
                                           )
                                         )
                                       )
                                      ),
                              ##
                              tabPanel("Coverage",
                                       br(),
                                       sidebarPanel(
                                         class = "scrollable",
                                         radioButtons(
                                           inputId = 'chosen_state',
                                           label = 'Choose state:',
                                           choices = c('CD160', 'CD160_HVEM'),
                                           selected = 'CD160'
                                         ),
                                         sliderInput(
                                           inputId = 'plot_range',
                                           label = 'Choose range:',
                                           min = 1,
                                           max = 300,
                                           value = c(1, 300),
                                           # ticks = seq(1, 300, 1) # this breaks shiny 1.5
                                         )
                                       ),
                                       mainPanel(
                                         class = "scrollable",
                                         tabsetPanel(
                                           tabPanel("Peptide Coverage",
                                                    br(),
                                                    withSpinner(plotOutput("stateOverlap", hover = hoverOpts("stateOverlap_hover", delay = 10, delayType = "debounce"))),
                                                    uiOutput("stateOverlap_debug"),
                                                    downloadButton("stateOverlap_download_button",
                                                                   "Save chart (.svg)")),

                                           tabPanel("Data",
                                                    br(),
                                                    DT::dataTableOutput("stateOverlap_data"))
                                         ),
                                         br(),
                                         tabsetPanel(
                                           tabPanel("Position Frequency",
                                                    br(),
                                                    div(style = "position:relative;",
                                                        withSpinner(plotOutput("stateOverlapDist", hover = hoverOpts("stateOverlapDist_hover", delay = 10, delayType = "debounce"))),
                                                        uiOutput("stateOverlapDist_debug")),
                                                    downloadButton("stateOverlapDist_download_button",
                                                                   "Save chart (.svg)")
                                           ),
                                           tabPanel("Data",
                                                    br(),
                                                    DT::dataTableOutput("stateOverlapDist_data"))
                                         )

                                       )
                              ),
                              ##
                              tabPanel("Sequence data",
                                       h3('Protein name'),
                                       h4(textOutput("protein_name"), class  = "monospaced"),
                                       h3('Reconstructed sequence'),
                                       htmlOutput("sequenceName", container = tags[["span"]], class  = "monospaced"),
                                       br(),
                                       wellPanel(
                                         sidebarLayout(
                                           sidebarPanel(
                                             fluidRow(
                                               column(6,
                                                      tableOutput("protein_stats"),
                                                      br(),
                                                      checkboxGroupInput(
                                                        inputId = "hydro_prop",
                                                        label = "Hydro-",
                                                        choices = c(
                                                          "Hydrophilic" = "philic",
                                                          "Hydrophobic" = "phobic"),
                                                        selected = c("philic", "phobic"))
                                               ),
                                               column(6,
                                                      br()
                                               )
                                             )
                                           ),
                                           mainPanel(withSpinner(plotOutput("aminoDist", hover = hoverOpts("aminoDist_hover", delay = 10, delayType = "debounce"))),
                                                     uiOutput("aminoDist_debug"),
                                                     downloadButton("aminoDist_download_button",
                                                                    "Save chart (.svg)"),
                                                     p("Source: Kyte, J., and Doolittle, R.F. (1982). A simple method for displaying the hydropathic character of a protein. J Mol Biol 157, 105–132."))
                                         )
                                       )
                              ),
                              ##
                              tabPanel("Uptake curves",
                                       br(),
                                       sidebarPanel(
                                         class = "scrollable",
                                         h3("Select parameters for the plot."),
                                         checkboxInput_h(inputId = "kin_theory",
                                                         label = "Theoretical calculations",
                                                         value = FALSE),
                                         radioButtons_h(inputId = "kin_calc_type",
                                                        label = "Choose values type:",
                                                        choices = c("fractional", "absolute"),
                                                        selected = "fractional"),
                                         div(id = "kin_time_part",
                                             h5("Choose time parameters:"),
                                             splitLayout(
                                               div(id = "kin_time_0_part",
                                                   selectInput_h(inputId = "kin_time_0",
                                                                 label = "IN",
                                                                 choices = c("0", "1", "5", "25", "1440"))
                                               ),
                                               div(id = "kin_time_100_part",
                                                   selectInput_h(inputId = "kin_time_100",
                                                                 label = "OUT",
                                                                 choices = c("0", "1", "5", "25", "1440")))
                                             )),
                                         h5("Choose peptide:"),
                                         dataTableOutput_h("peptide_list_data"), ## !!
                                         actionButton(inputId = "reset_peptide_list",
                                                      label = "Reset chosen peptides"),
                                         br(),
                                         br(),
                                         br(),
                                         sliderInput(inputId = 'kin_plot_y_range',
                                                     label = 'Choose y range for the plot:',
                                                     min = -50,
                                                     max = 200,
                                                     value = c(-10, 100),
                                                     step = 10),
                                         tags$button("Adjust labels",
                                                     class = "collapse-button",
                                                     `data-toggle`="collapse",
                                                     `data-target`="#kin-labs"),
                                         tags$div(
                                           class = "hideable",
                                           id = "kin-labs",
                                           textInput(inputId = "kin_plot_title",
                                                     label = "Uptake curve title:",
                                                     value = "Uptake curve for chosen peptides"),
                                           textInput(inputId = "kin_plot_x_label",
                                                     label = "Uptake curve axis x label:",
                                                     value = "Time point [min]"),
                                           textInput(inputId = "kin_plot_y_label",
                                                     label = "Uptake curve axis y label:",
                                                     value = "Deuteration")
                                         )
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel(
                                             "Uptake curve",
                                             br(),
                                             plotOutput_h("kinetic_plot_chosen_peptides", hover = hoverOpts("kinetic_plot_chosen_peptides_hover", delay = 10, delayType = "debounce"), height = 600),
                                             uiOutput("kinetic_plot_chosen_peptides_debug"),
                                             downloadButton("kineticPlot_download_button",
                                                            "Save chart (.svg)")),
                                           tabPanel("Data",
                                                    br(),
                                                    DT::dataTableOutput("kin_plot_data"))
                                         )
                                       )
                              ) ,
                              #
                              tabPanel("Quality control",
                                       br(),
                                       sidebarPanel(
                                         h3("Select parameters for the plot."),
                                         h5("The plot can be rendered only for fractional experimental values."),
                                         selectInput_h(inputId = "qc_time_0",
                                                       label = "Choose in time: ",
                                                       choices = c("0", "1", "5", "25", "1440")),
                                         selectInput_h(inputId = "qc_time_t",
                                                       label = "Choose time: ",
                                                       choices = c("0", "1", "5", "25", "1440")),
                                         selectInput_h(inputId = "qc_state_first",
                                                       label = "State 1",
                                                       choices = c("CD160", "CD160VEM")),
                                         selectInput_h(inputId = "qc_state_second",
                                                       label = "State 2",
                                                       choices = c("CD160", "CD160VEM"))
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Quality control plot",
                                                    br(),
                                                    plotOutput_h("quality_control_plot", hover = hoverOpts("quality_control_plot_hover", delay = 10, delayType = "debounce"), height = 600),
                                                    uiOutput("quality_control_plot_debug"),
                                                    downloadButton("quality_control_plot_download_button",
                                                                   "Save chart (.svg)"),
                                                    br(),
                                                    h4("This function plots the change in the uncertainty of deuteration levels as a function of incubation time. The uncertainty is averaged over all peptides available at a given time point in a selected state. This chart has a double function: firstly, it allows checking if the measurement uncertainty is decreasing over time (which is the expected behavior). Secondly, it helps to plan the appropriate incubation length for the tested protein (whether we obtain the desired data reliability values).")
                                           ),
                                           tabPanel("Data",
                                                    br(),
                                                    DT::dataTableOutput("quality_control_plot_data"))
                                         )
                                       )
                              ),
                              tabPanel("Summary",
                                       br(),
                                       fluidRow(
                                         withSpinner(DT::dataTableOutput("summary_table"))
                                       ),
                                       includeMarkdown("./readmes/summary.md")
                              ),
                              tabPanel("Report",
                                       br(),
                                       sidebarPanel(width = 8,
                                                    h4("Choose items for report:"),
                                                    fluidRow(
                                                      column(6,
                                                             checkboxInput(inputId = "export_overlap_dist",
                                                                           label = "Position Frequency",
                                                                           value = TRUE),
                                                             checkboxInput(inputId = "export_overlap_graph",
                                                                           label = "Peptide Coverage",
                                                                           value = TRUE),
                                                             checkboxInput(inputId = "export_comparison_plot",
                                                                           label = "Comparison Plot",
                                                                           value = TRUE),
                                                             checkboxInput(inputId = "export_theo_comparison_plot",
                                                                           label = "Theoretical Comparison Plot",
                                                                           value = TRUE),
                                                             checkboxInput(inputId = "export_woods_plot",
                                                                           label = "Woods Plot",
                                                                           value = TRUE),
                                                             checkboxInput(inputId = "export_theo_woods_plot",
                                                                           label = "Theoretical Woods Plot",
                                                                           value = TRUE),
                                                             checkboxInput(inputId = "export_kin_plot",
                                                                           label = "Uptake Curve",
                                                                           value = FALSE),
                                                             checkboxInput(inputId = "export_theo_kin_plot",
                                                                           label = "Theoretical Uptake Curve",
                                                                           value = FALSE),
                                                             checkboxInput(inputId = "export_quality_control_plot",
                                                                           label = "Quality Control Plot",
                                                                           value = FALSE),
                                                             checkboxInput(inputId = "export_butterfly_plot",
                                                                           label = "Butterfly Plot",
                                                                           value = FALSE),
                                                             checkboxInput(inputId = "export_butterfly_differential_plot",
                                                                           label = "Butterfly Differential Plot",
                                                                           value = FALSE),
                                                             checkboxInput(inputId = "export_volcano_plot",
                                                                           label = "Volcano Plot",
                                                                           value = FALSE)),
                                                      column(6,
                                                             checkboxInput(inputId = "export_overlap_dist_data",
                                                                           label = "Position Frequency Data"),
                                                             checkboxInput(inputId = "export_overlap_graph_data",
                                                                           label = "Peptide Coverage Data"),
                                                             checkboxInput(inputId = "export_comparison_plot_data",
                                                                           label = "Comparison Plot Data"),
                                                             checkboxInput(inputId = "export_theo_comparison_plot_data",
                                                                           label = "Theoretical Comparison Plot Data"),
                                                             checkboxInput(inputId = "export_woods_plot_data",
                                                                           label = "Woods Plot Data"),
                                                             checkboxInput(inputId = "export_theo_woods_plot_data",
                                                                           label = "Theoretical Woods Plot Data"),
                                                             checkboxInput(input = "export_kin_plot_data",
                                                                           label = "Uptake Curve Data"),
                                                             checkboxInput(inputId = "export_theo_kin_plot_data",
                                                                           label = "Theoretical Uptake Curve Data"),
                                                             checkboxInput(inputId = "export_quality_control_plot_data",
                                                                           label = "Quality Control Plot Data"),
                                                             checkboxInput(inputId = "export_butterfly_plot_data",
                                                                           label = "Butterfly Plot Data"),
                                                             checkboxInput(inputId = "export_butterfly_differential_plot_data",
                                                                           label = "Butterfly Differential Plot Data"),
                                                             checkboxInput(inputId = "export_volcano_plot_data",
                                                                           label = "Volcano Plot Data")
                                                             )),
                                                    br(),
                                                    h5("Elements chosen for report have the same parameters as chosen in panel e.g. axis range and title. Adjust parameters for plots as needed in the report."),
                                                    br(),
                                                    downloadButton(outputId = "export_action",
                                                                   label = "  Create report!",
                                                                   icon = icon("fas fa-download"))
                                       )
                              )
                  )
                ),
                tags$div(
                  class = "mobile-site-backbone",
                  tags$div(
                    class = "logo-panel",
                    img(src = "mock_logo.png", class = "logo")
                  ),
                  tags$div(
                    class = "mobile-information",
                    h3("Welcome to HaDeX website!"),
                    h4("For better user experience please use device with wider screen (at least 900px)."),
                    img(src='funding_icons.png', height = 100)
                  )
                ),
                tags$script(type="text/javascript",
                            src="resize-logo-panel.js")
)

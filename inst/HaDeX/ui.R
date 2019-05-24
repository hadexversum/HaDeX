source("data-work.R")

#########################################

ui <- fluidPage(theme = "HaDeX_theme.css",
                #titlePanel("HaDeX"), #: analysis of data from hydrogen deuterium exchange-mass spectrometry"),
                title = "HaDeX",
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
                             h3("Welcome!"),
                             h4("Upload your file. Otherwise you will see example data."),
                             #
                             fluidRow(
                               column(3, 
                                      fileInput(
                                        inputId = "data_file",
                                        label = "Choose file:",
                                        multiple = FALSE,
                                        accept = c(".csv"),
                                        placeholder = "No .csv file selected")),
                               column(4, 
                                      h5("File status:"),
                                      textOutput("data_file_info"))
                             ),
                             #
                             h4("Currently HaDeX supports files with only one protein."),
                             h4("In order for program to behave correctly, please make sure supplied file fulfills following requirements:"),
                             tags$button("Show requirements", 
                                         class = "collapse-button", 
                                         style = "width: unset",
                                         `data-toggle`="collapse", 
                                         `data-target`="#reqs"),
                             tags$div(
                               id = "reqs",
                               style = "width: min-content",
                               class = "hideable",
                             tableOutput("file_req")
                             ),
                             includeMarkdown("readmes/about.md"),
                             img(class='funding-icons',
                                 src='funding_icons.png'),
                             br()
                    ),
                    tabPanel("Woods plot",
                             br(),
                             sidebarPanel(
                               class = "scrollable",
                               h3("Select parameters for the plot."),
                               checkboxInput(inputId = "theory",
                                             label = "Theoretical calculations",
                                             value = FALSE),
                               radioButtons(inputId = "calc_type",
                                            label = "Choose values type:",
                                            choices = c("relative", "absolute"),
                                            selected = "relative"),
                               h4("Comparison plot parameters:"),
                               h5("Choose time paramters:"),
                               splitLayout(
                                 selectInput(inputId = "in_time",
                                             label = "IN",
                                             choices = c("0", "1", "5", "25", "1440")),
                                 selectInput(inputId = "chosen_time",
                                             label = "CHOSEN",
                                             choices = c("0", "1", "5", "25", "1440")),
                                 selectInput(inputId = "out_time",
                                             label = "OUT",
                                             choices = c("0", "1", "5", "25", "1440"))
                               ),
                               ##
                               fluidRow(
                                 column(6, 
                                        h5("Choose states for comparison:"),
                                        checkboxGroupInput(inputId = "compare_states",
                                                           label = "",
                                                           choices = c("CD160", "CD160_HVEM"),
                                                           selected = c("CD160", "CD160_HVEM")),
                                        class = "states-to-compare-column"),
                                 column(6,
                                        tags$button("Adjust colors",
                                                    class = "collapse-button",
                                                    `data-toggle`="collapse",
                                                    `data-target`="#colorss"),
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
                                 selectInput(inputId = "state_first",
                                             label = "State 1",
                                             choices = c("CD160", "CD160_HVEM")),
                                 selectInput(inputId = "state_second",
                                             label = "State 2", 
                                             choices = c("CD160", "CD160_HVEM"))
                               ),
                               splitLayout(
                                 selectInput(inputId = "confidence_limit",
                                             label = "Confidence limit 1:",
                                             choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                             selected = 0.98),
                                 selectInput(inputId = "confidence_limit_2",
                                             label = "Confidence limit 2:",
                                             choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                             selected = 0.99)
                               ),
                               ##
                               h4("Adjust plot:"),
                               sliderInput(inputId = 'comp_plot_y_range',
                                           label = 'Choose y range for comparison plot:',
                                           min = -2,
                                           max = 2,
                                           value = c(0, 1.2),
                                           step = 0.1,
                                           width = "100%"),
                               sliderInput(inputId = 'woods_plot_y_range',
                                           label = 'Choose y range for Woods plot:',
                                           min = -2,
                                           max = 2,
                                           value = c(-.5, .5),
                                           step = 0.1),
                               sliderInput(inputId = 'plot_x_range',
                                           label = 'Choose x range for both plots:',
                                           min = 0,
                                           max = 300,
                                           value = c(0, 300),
                                           ticks = seq(0, 300, 1)),
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
                                           label = "Comprison plot axis x label:",
                                           value = "Position in sequence"),
                                 textInput(inputId = "comparison_plot_y_label",
                                           label = "Comprison plot axis y label:",
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
                                          plotOutput("comparisonPlot")), # height = "800px")),
                                 tabPanel("Data",
                                          br(),
                                          DT::dataTableOutput("comparisonPlot_data"))),
                               br(),
                               tabsetPanel(
                                 tabPanel("Woods plot",
                                          br(),
                                          plotOutput("differentialPlot")), # height = "800px")),
                                 tabPanel("Data",
                                          br(),
                                          DT::dataTableOutput("differentialPlot_data")))
                               
                             )
                    ),
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
                                 ticks = seq(1, 300, 1)
                               )
                             ),
                             mainPanel(
                               class = "scrollable",
                               tabsetPanel(
                                 tabPanel("Peptide Coverage",
                                          br(),
                                          plotOutput("stateOverlap")), # height = "600px")),
                                 tabPanel("Data",
                                          br(),
                                          DT::dataTableOutput("stateOverlap_data"))
                               ),
                               br(),
                               tabsetPanel(
                                 tabPanel("Position Frequency",
                                          br(),
                                          plotOutput("stateOverlapDist")),
                                 tabPanel("Data",
                                          br(),
                                          DT::dataTableOutput("stateOverlapDist_data"))
                               )
                               
                             )
                    ),
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
                                            numericInput(inputId = "sequence_length",
                                                         label = "Correct sequence length:",
                                                         value = 300, 
                                                         step = 1),
                                            h5("If C-term is not covered, enter correct value."),
                                            textOutput("sequence_length_exp_info")
                                     )
                                   )
                                 ),
                                 mainPanel(plotOutput('aminoDist')))
                             )
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
                                                                 value = TRUE)),
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
                                                                 label = "Theoretical Woods Plot Data"))),
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



#tags$div(class = "logo-container",
#         tags$img(source = "www/mock_logo.png"))
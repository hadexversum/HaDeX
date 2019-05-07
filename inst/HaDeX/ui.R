source("data-work.R")

#########################################

ui <- fluidPage(theme = "HaDeX_theme.css",
                titlePanel("HaDeX: analysis of data from hydrogen deuterium exchange-mass spectrometry"),
                
                tabsetPanel(type = "pills",
                  tabPanel("Start",
                           h3("Welcome!"),
                           h4("Upload your file. Otherwise you will see example data."),
                           fileInput(
                             inputId = "data_file",
                             label = "Choose file:",
                             multiple = FALSE,
                             accept = c(".csv"),
                             placeholder = "No .csv file selected"),
                           h4("In order for program to behave correctly, please make sure supplied file fulfills following requirements:"),
                           tableOutput("file_req")
                  ),
                  tabPanel("Sequence data",
                           h3('Protein name'),
                           h4(textOutput("protein_name"), class  = "monospaced"),
                           h3('Reconstructed sequence'),
                           htmlOutput("sequenceName", container = tags$span, class  = "monospaced"),
                           br(),
                           wellPanel(
                             sidebarLayout(
                               sidebarPanel(
                                 tableOutput("protein_stats"),
                                 br(),
                                 checkboxGroupInput(
                                   inputId = "hydro_prop",
                                   label = "Hydro-",
                                   choices = c(
                                     "Hydrophilic" = "philic",
                                     "Hydrophobic" = "phobic"
                                   ),
                                   selected = c("philic", "phobic")
                                 )
                               ),
                               mainPanel(plotOutput('aminoDist'))
                             )
                           )
                  ),
                  tabPanel("Coverage", 
                           br(),
                           sidebarPanel(
                             radioButtons(
                               inputId = 'chosen_state',
                               label = 'Choose state:',
                               choices = c('BETA', 'BETA_gamma', 'BETA_gamma_alpha'),
                               selected = 'BETA'
                             ),
                             sliderInput(
                               inputId = 'plot_range',
                               label = 'Choose range:',
                               min = 0,
                               max = 300,
                               value = c(0, 300),
                               ticks = seq(0, 300, 1)
                             )
                           ),
                           mainPanel(
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
                  tabPanel("Woods plot",
                           br(),
                           sidebarPanel(
                             h3("Select parameters for the plot."),
                             checkboxInput(inputId = "theory",
                                           label = "Theoretical calculations",
                                           value = FALSE),
                             radioButtons(inputId = "calc_type",
                                          label = "Choose values type:",
                                          choices = c("relative", "absolute"),
                                          selected = "relative"),
                             h4("Comparison plot parameters:"),
                             selectInput(inputId = "chosen_time",
                                         label = "Time point CHOSEN",
                                         choices = c("0", "1", "5", "25", "1440"),
                                         width = "40%"),
                             selectInput(inputId = "in_time",
                                         label = "Time point IN",
                                         choices = c("0", "1", "5", "25", "1440"),
                                         width = "40%"),
                             selectInput(inputId = "out_time",
                                         label = "Time point OUT",
                                         choices = c("0", "1", "5", "25", "1440"),
                                         width = "40%"),
                             checkboxGroupInput(inputId = "compare_states",
                                                label = "Choose states for comparison:",
                                                choices = c("CD160", "CD160_HVEM"),
                                                selected = c("CD160", "CD160_HVEM")),
                             h4("Woods plot parameters:"),
                             selectInput(inputId = "state_first",
                                         label = "State 1",
                                         choices = c("CD160", "CD160_HVEM"),
                                         width = "50%"),
                             selectInput(inputId = "state_second",
                                         label = "State 2", 
                                         choices = c("CD160", "CD160_HVEM"),
                                         width = "50%"),
                             selectInput(inputId = "confidence_limit",
                                         label = "Choose confidence limit:",
                                         choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                         selected = 0.98,
                                         width = "60%"),
                             selectInput(inputId = "confidence_limit_2",
                                         label = "Choose second confidence limit:",
                                         choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                         selected = 0.99,
                                         width = "60%"),
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
                                         ticks = seq(0, 300, 1))),
                           mainPanel(
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
                                        downloadButton(outputId = "export_action",
                                                       label = "  Create report!",
                                                       icon = icon("fas fa-download"))
                           )
                  ),
                  tabPanel("About",
                           includeMarkdown("readmes/about.md")
                  )
                )
)

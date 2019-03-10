source("data-work.R")

#########################################

ui <- fluidPage(theme = "theme.css",
  
  tags[["head"]](
    tags[["style"]](HTML("
                         
                         span { 
                         display: block;
                         max-width: 100%;
                         word-wrap: break-word;
                         }
                         
                         "))
    ),
  
  
  titlePanel("Protein data analysis"),
  
  tabsetPanel(
    tabPanel("Start",
             br(),
             h3("Welcome!"),
             h4("Please, upload your file. Otherwise you will see example data."),
             fileInput(
               inputId = "data_file",
               label = "Choose file:",
               multiple = FALSE,
               accept = c(".csv"),
               placeholder = "No .csv file selected"),
             h4("In order for program to behave correctly, please make sure supplied file fulfills following requirements:"),
             tableOutput("file_req")
    ),
    tabPanel("General data",
             br(),
             h3('Protein name'),
             h4(textOutput("protein_name")),
             h3('Reconstructed sequence'),
             htmlOutput("sequenceName"),
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
    tabPanel("Overlapping", 
             br(),
             sidebarPanel(
               radioButtons(
                 inputId = 'chosen_state',
                 label = 'Choose state :',
                 choices = c('BETA', 'BETA_gamma', 'BETA_gamma_alpha'),
                 selected = 'BETA'
               ),
               sliderInput(
                 inputId = 'plot_range',
                 label = 'Choose range :',
                 min = 0,
                 max = 300,
                 value = c(0, 300),
                 ticks = seq(0, 300, 1)
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Overlap Distribution",
                          br(),
                          plotOutput("stateOverlapDist")),
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput("stateOverlapDist_data"))
               ),
               br(),
               tabsetPanel(
                 tabPanel("Overlap Graphically",
                          br(),
                          plotOutput("stateOverlap")),
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput("stateOverlap_data"))
               )
             )
    ),
    tabPanel("Woods\'s plot",
             br(),
             h4("Double plot after small modifications  - for comparison"),
             br(),
             sidebarPanel(
               h3("Please, select parameters for the plot."),
               checkboxInput(inputId = "theory",
                             label = "Theoretical calculations",
                             value = FALSE
               ),
               h4("Time points parameters: "),
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
               h4("State paramters"),
               selectInput(inputId = "state_first",
                           label = "State 1",
                           choices = c("ALPHA", "BETA"),
                           width = "50%"),
               selectInput(inputId = "state_second",
                           label = "State 2", 
                           choices = c("ALPHA", "BETA"),
                           width = "50%")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Comparison plot", 
                          br(),
                          plotOutput("comparisonPlot")),
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput("comparisonPlot_data"))
               ),
               br(),
               tabsetPanel(
                 tabPanel("Wood\'s plot",
                          br(),
                          plotOutput("differentialPlot")),
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput("differentialPlot_data"))
               )
               
             )
    ),
    tabPanel("Cosik",
             br(),
             h4("Double plot the way Krzysiu plots it - for comparison"),
             br(),
             sidebarPanel(
               h3("Parameters are the same, see:"),
               tableOutput("plotParametersKrzys")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Comparison plot",
                          br(),
                          plotOutput("comparisonPlotKrzys")),
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput("comparisonPlotKrzys_data"))
               ),
               tabsetPanel(
                 tabPanel("Wood\'s plot",
                          br(),
                          plotOutput("differentailPlotKrzys")),
                 tabPanel("Data",
                          br(),
                          DT::dataTableOutput("differentailPlotKrzys_data"))
               )
             )
             
    ),
    tabPanel("Report",
             br(),
             sidebarPanel(h4("Please, choose items for raport."),
                          checkboxInput(inputId = "export_overlap_dist",
                                        label = "Overlap Distribution",
                                        value = TRUE),
                          checkboxInput(inputId = "export_overlap_dist_data",
                                        label = "Overlap Distribution Data"),
                          checkboxInput(inputId = "export_overlap_graph",
                                        label = "Overlap Graphically",
                                        value = TRUE),
                          checkboxInput(inputId = "export_overlap_graph_data",
                                        label = "Overlap Graphically Data"),
                          checkboxInput(inputId = "export_comparison_plot",
                                        label = "Comparison Plot",
                                        value = TRUE),
                          checkboxInput(inputId = "export_comparison_plot_data",
                                        label = "Comparison Plot Data"),
                          checkboxInput(inputId = "export_theo_comparison_plot",
                                        label = "Theoretical Comparison Plot",
                                        value = TRUE),
                          checkboxInput(inputId = "export_theo_comparison_plot_data",
                                        label = "Theoretical Comparison Plot Data"),
                          checkboxInput(inputId = "export_woods_plot",
                                        label = "Woods Plot",
                                        value = TRUE),
                          checkboxInput(inputId = "export_woods_plot_data",
                                        label = "Woods Plot Data"),
                          checkboxInput(inputId = "export_theo_woods_plot",
                                        label = "Theoretical Woods Plot",
                                        value = TRUE),
                          checkboxInput(inputId = "export_theo_woods_plot_data",
                                        label = "Theoretical Woods Plot Data"),
                          # br(),
                          # actionButton(inputId = "preview_action",
                          #              label = "  Show preview!",
                          #              icon = icon("fas fa-eye")),
                          br(),
                          br(),
                          downloadButton(outputId = "export_action",
                                         label = "  Create report!",
                                         icon = icon("fas fa-download"))
                          )
             )
  )
)

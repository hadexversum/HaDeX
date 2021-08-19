tab_replicates <- tabPanel("Replicates",
         br(),
         sidebarPanel(
           class = "scrollable",
           
           ##### SETTINGS #####
           h3("Select parameters for the plots"),
           splitLayout(
             selectInput_h(inputId = "rep_time",
                           label = "Select time point: ",
                           choices = c(0, 0.001, 0.167, 1, 5, 25, 120, 1440),
                           selected = 1),
             selectInput_h(inputId = "rep_state",
                           label = "Select state: ",
                           choices = c("CD160", "CD160_HVEM"),
                           selected = "CD160")
           ),
           br(),
           dataTableOutput_h("rep_sequence"),
           br(),
           
           br(),
           tags$button("Adjust labels",
                       class = "collapse-button",
                       `data-toggle`="collapse",
                       `data-target`="#rep-labs"),
           tags$div(
             class = "hideable",
             id = "rep-labs",
             fluidRow(
               column(width = 10,
                      textInput(inputId = "rep_plot_title",
                                label = "Plot title:",
                                value = ""),
                      textInput(inputId = "rep_plot_x_label",
                                label = "Plot x label:",
                                value = "Measured mass [Da]"),
                      textInput(inputId = "rep_plot_y_label",
                                label = "Plot y label:",
                                value = "")),
               column(width = 2,
                      numericInput_h(inputId = "rep_plot_title_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5),
                      numericInput_h(inputId = "rep_plot_x_label_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5),
                      numericInput_h(inputId = "rep_plot_y_label_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5))
             ),
             h4("The axis ticks have the same size as the axis label.")
           )
         ),
         mainPanel(
           class = "scrollable" ,
           tabsetPanel(
             tabPanel("Plot",
                      fluidRow(
                        column(width = 6,
                               br(),
                               plotOutput_h("replicatesPlot", hover = hoverOpts("replicatesPlot_hover", delay = 10, delayType = "debounce")),
                               uiOutput("replicatesPlot_debug"),
                               downloadButton("replicatesPlot_download_button",
                                              "Save chart (.svg)")),
                        column(width = 6,
                               br(),
                               plotOutput_h("replicatesChargePlot", hover = hoverOpts("replicatesChargePlot_hover", delay = 10, delayType = "debounce")),
                               uiOutput("replicatesChargePlot_debug"),
                               downloadButton("replicatesChargePlot_download_button",
                                              "Save chart (.svg)"),
                               br())
                      ) ,
                      br(),
                      br()
             ) ,
             tabPanel("Data",
                      br(),
                      DT::dataTableOutput("replicatesPlot_data"),
                      br(),
                      DT::dataTableOutput("replicatesChargePlot_data"),
                      br()
                      
             )
           ),
           tabsetPanel(
             tabPanel("Plot",
                      br(),
                      div(style = "position:relative",
                          plotOutput_h("replicatesHistogram", hover = hoverOpts("replicatesHistogram_hover", delay = 10, delayType = "debounce")),
                          uiOutput("replicatesHistogram_debug")),
                      downloadButton("replicatesHistogram_download_button",
                                     "Save chart (.svg)")),
             tabPanel("Data",
                      br(),
                      DT::dataTableOutput("replicatesHistogram_data"))
           ),
           br(),
           br(),
           tabsetPanel(
             tabPanel("Plot",
                      br(),
                      div(style = "position:relative",
                          plotOutput_h("allReplicatesHistogram", hover = hoverOpts("allReplicatesHistogram_hover", delay = 10, delayType = "debounce")),
                          uiOutput("allReplicatesHistogram_debug")
                      ),
                      downloadButton("allReplicatesHistogram_download_button",
                                     "Save chart (.svg)")),
             tabPanel("Data",
                      br(),
                      DT::dataTableOutput("allReplicatesHistogram_data"))
           )
         )
         
)
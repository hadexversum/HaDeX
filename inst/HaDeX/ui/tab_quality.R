tab_quality <- tabPanel("Quality control",
         br(),
         sidebarPanel(
           
           ##### SETTINGS ##### 
           
           h3("Select parameters for the plot."),
           h5("The plot can be rendered only for fractional experimental values."),
           selectInput_h(inputId = "qc_time_0",
                         label = "Choose in time: ",
                         choices = c("0", "1", "5", "25", "1440")),
           selectInput_h(inputId = "qc_time_t",
                         label = "Choose time: ",
                         choices = c("0", "1", "5", "25", "1440")),
           selectInput_h(inputId = "qc_state_1",
                         label = "State 1",
                         choices = c("CD160", "CD160VEM")),
           selectInput_h(inputId = "qc_state_2",
                         label = "State 2",
                         choices = c("CD160", "CD160VEM"))
         ),
         mainPanel(
           tabsetPanel(
             
             ##### QUALITY CONTROL PLOT ##### 
             
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
)
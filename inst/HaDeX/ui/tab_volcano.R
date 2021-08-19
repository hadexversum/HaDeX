tab_volcano <- tabPanel("Volcano plot",
         br(),
         sidebarPanel(
           class = "scrollable",
           
           ##### SETTINGS ##### 
           
           h3("Select parameters for the plot."),
           h5("Volcano plot presents the uptake difference [Da] between State 1 and State 2."),
           splitLayout(selectInput_h(inputId = "vol_state_1",
                                     label = "State 1",
                                     choices = c("CD160", "CD160_HVEM")),
                       selectInput_h(inputId = "vol_state_2",
                                     label = "State 2",
                                     choices = c("CD160_HVEM", "CD160"))
           ),
           fluidRow(
             column(width = 6,
                    checkboxGroupInput_h(inputId = "vol_timepoints",
                                         label = "Show time points: ",
                                         choices = c(0.167, 1, 5, 25, 120, 1440),
                                         selected = c(0.167, 1, 5, 25, 120, 1440))
             ),
             column(width = 6,
                    selectInput_h(inputId = "vol_confidence_level",
                                  label = "Confidence level:",
                                  choices = c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                                  selected = 0.98),
                    selectInput_h(inputId = "vol_p_adjustment_method",
                                  label = "Choose method of adjustment",
                                  choices = c("none", "BH", "bonferroni"),
                                  selected = "none"),
                    selectInput_h(inputId = "vol_interval",
                                  label = "Show confidence limit for: ",
                                  choices = c("All time points", "Selected time points"),
                                  selected = "All time points")
                    
             )
           ),
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
             fluidRow(
               column(width = 10,
                      textInput(inputId = "volcano_plot_title",
                                label = "Volcano plot title:",
                                value = ""),
                      textInput(inputId = "volcano_plot_x_label",
                                label = "Volcano plot axis x label:",
                                value = "Mass difference [Da]"),
                      textInput(inputId = "volcano_plot_y_label",
                                label = "Volcano plot axis y label:",
                                value = "-log(P value)")),
               column(width = 2,
                      numericInput_h(inputId = "volcano_plot_title_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5),
                      numericInput_h(inputId = "volcano_plot_x_label_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5),
                      numericInput_h(inputId = "volcano_plot_y_label_size",
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
             
             ##### VOLCANO PLOT ##### 
             
             tabPanel("Volcano plot",
                      br(),
                      plotOutput_h("volcanoPlot", width = "80%", height = "800px", hover = hoverOpts("volcanoPlot_hover", delay = 10, delayType = "debounce")),
                      h5(textOutput("vol_thresholds")),
                      h5("For more information see about hybrid testing see: Hageman, T. S. & Weis, D. D. Reliable Identification of Significant Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
"),
                      downloadButton("volcanoPlot_download_button",
                                     "Save chart (.svg)")
             ),
             tabPanel("Data",
                      br(),
                      DT::dataTableOutput("volcanoPlot_data"),
                      br()
             )
           ),
           uiOutput("volcanoPlot_debug"),
           br()
         )
)
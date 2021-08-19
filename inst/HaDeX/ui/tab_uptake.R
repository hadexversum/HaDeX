tab_uptake <- tabPanel("Uptake curves",
         br(),
         sidebarPanel(
           class = "scrollable",
           
           ##### SETTINGS ##### 
           
           h3("Select parameters for the plot."),
           fluidRow(
             column(width = 6,
                    checkboxInput_h(inputId = "kin_theory",
                                    label = "Theoretical calculations",
                                    value = FALSE),
                    checkboxInput_h(inputId = "kin_fractional",
                                    label = "Fractional values",
                                    value = FALSE)
             )
           ),
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
           br(),
           actionButton(inputId = "reset_peptide_list",
                        label = "Reset chosen peptides"),
           br(),
           br(),
           h4("Visualization:"),
           fluidRow(
             column(width = 6,
                    checkboxInput_h(inputId = "kin_log_x",
                                    label = "Logaritmic x scale",
                                    value = TRUE),
                    selectInput_h(inputId = "kin_uncertainty",
                                  label = "Show uncertainty as:",
                                  choices = c("ribbon", "bars", "bars + line"),
                                  selected = "ribbon")
             )
           ),
           h4("Zoom:"),
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
             fluidRow(
               column(width = 10,
                      textInput(inputId = "kin_plot_title",
                                label = "Uptake curve title:",
                                value = "Uptake curve for chosen peptides"),
                      textInput(inputId = "kin_plot_x_label",
                                label = "Uptake curve axis x label:",
                                value = "Time point [min]"),
                      textInput(inputId = "kin_plot_y_label",
                                label = "Uptake curve axis y label:",
                                value = "Deuteration")),
               column(width = 2,
                      numericInput_h(inputId = "kin_plot_title_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5),
                      numericInput_h(inputId = "kin_plot_x_label_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5),
                      numericInput_h(inputId = "kin_plot_y_label_size",
                                     label = "Size:",
                                     value = 15,
                                     min = 5))
             ),                                                                               
             h4("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.") 
           )
         ),
         mainPanel(
           tabsetPanel(
             
             ##### UPTAKE CURVE ##### 
             
             tabPanel(
               "Uptake curve",
               br(),
               plotOutput_h("kinetic_plot_chosen_peptides", hover = hoverOpts("kinetic_plot_chosen_peptides_hover", delay = 10, delayType = "debounce"), height = 600),
               uiOutput("kinetic_plot_chosen_peptides_debug"),
               downloadButton("kineticPlot_download_button",
                              "Save chart (.svg)")),
             tabPanel("Data",
                      br(),
                      DT::dataTableOutput("kin_plot_data")),
             
             ## DOWNLOAD ALL PLOTS ##
             
             tabPanel("Download",
                      h3("Download uptake curves for all peptides in selected form, based on the parameters from the Settings panel."),
                      h4("Preparing the plots may take a while."),
                      fluidRow(
                        column(width = 4,
                               h4("Download the zipped folder with separate files:"),
                               downloadButton(outputId = "kin_download_folder",
                                              label = "Download folder")
                        ),
                        column(width = 4,
                               h4("Download the pdf file with plots arranged in a grid:"),
                               downloadButton(outputId = "kin_download_file",
                                              label = "Download file"),
                               br(),
                               br(),
                               numericInput_h(inputId = "kin_download_file_columns",
                                              label = "Select number of columns on a page",
                                              min = 1, max = 5,
                                              value = 2),
                               numericInput_h(inputId = "kin_download_file_rows",
                                              label = "Select number of rows on a page",
                                              min = 1, max = 5,
                                              value = 2)
                        )
                      )
             )
           )
         )
)
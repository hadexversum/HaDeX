tab_woods <- function() HaDeX_plotTab(
  
  title = "Comparison and Woods plot",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    diff_comp_general_settings(),
    diff_comp_time_parameters(),
    comp_plot_parameters(),
    diff_plot_parameters(),
    diff_test(),
    diff_comp_zoom(),
    diff_comp_labels_adjustement()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    comp_plot_panel(),
    # comp_debug_panel(),
    diff_plot_panel()
    )
)

##


diff_comp_general_settings <- function() HaDeX_plotSettingsSection(
  
  checkboxInput_h(inputId = "theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = "comp_fractional", 
                  label = "Fractional values",
                  value = FALSE)
)

##

diff_comp_time_parameters <- function() HaDeX_plotSettingsSection(
  
  title = "Select time parameters",
  
  splitLayout(
    div(id = "time_0_part",
        selectInput_h(inputId = "time_0",
                      label = "Deut 0%",
                      choices = c("0", "1", "5", "25", "1440"))
    ),
    selectInput_h(inputId = "time_t",
                  label = "Exposure",
                  choices = c("0", "1", "5", "25", "1440")),
    div(id = "time_100_part",
        selectInput_h(inputId = "time_100",
                      label = "Deut 100%",
                      choices = c("0", "1", "5", "25", "1440"))
    )
  ),
  div(id = "diff_comp_times_t_part",
      checkboxGroupInput_h(input = "diff_comp_times_t",
                           label = "Select time points for the plots:",
                           choices = c(0, 1, 5, 25, 120),
                           inline = TRUE))
) 

##

comp_plot_parameters <- function() HaDeX_plotSettingsSection(
  
  title = "Comparison plot parameters",
  
  fluidRow(
    column(6,
           checkboxGroupInput_h(inputId = "compare_states",
                                label = "Choose states for comparison:",
                                choices = c("CD160", "CD160VEM"),
                                selected = c("CD160", "CD160VEM")),
           class = "states-to-compare-column"),
    column(6,
           helper(
             HaDeX_collapseButton(
               title = "Adjust colors",
               target = "#HaDeX-woods-colors-adjusting-panel"
             ),
             content = "adjust_colors", 
             type = "markdown", 
             buttonLabel = "Okay",
             easyClose = TRUE, 
             colour = "#856C9D"
           ),
           HaDeX_collapsablePanel(
             id = "HaDeX-woods-colors-adjusting-panel",
             uiOutput("states_colors")
           ),
           class = "states-colors-column"
    )
  )
)

##

diff_plot_parameters <- function() HaDeX_plotSettingsSection(
  
  title = "Woods plot parameters",
  
  p("Differential plot presents the uptake difference between State 1 and State 2."),
  splitLayout(
    selectInput_h(inputId = "diff_state_1",
                  label = "State 1",
                  choices = c("CD160", "CD160VEM")),
    selectInput_h(inputId = "diff_state_2",
                  label = "State 2",
                  choices = c("CD160", "CD160VEM"))
  )
)

diff_test <- function() HaDeX_plotSettingsSection(
    
  title = "Test",
    
  fluidPage(
    fluidRow(
      column(
        width = 6,
        checkboxInput_h(inputId = "diff_show_houde",
                        label = "Houde test",
                        value = TRUE),
        checkboxInput_h(inputId = "diff_show_tstud", 
                        label = "t-Student test", 
                        value = FALSE),
        checkboxInput_h(inputId = "diff_hide_insignificant",
                        label = "Hide insignificant?",
                        value = FALSE)
      ),
      column(
        width = 6,
        selectInput_h(inputId = "confidence_level",
                      label = "Confidence level:",
                      choices = c("80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
                      selected = 0.98),
        div(id = "diff_correction_part",
            selectInput_h(inputId = "diff_p_adjustment_method",
                          label = "Choose method of adjustment:",
                          choices = c("none", "BH", "bonferroni"),
                          selected = "none"))
      )
    )
  ) 
)

diff_comp_zoom <- function() HaDeX_plotSettingsSection(
  
  title = "Zoom",
  
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
)

diff_comp_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-woods-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    id = "HaDeX-woods-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "comparison_plot_title",
                       label = "Comparison plot title:",
                       value = ""),
             textInput(inputId = "comparison_plot_x_label",
                       label = "Comparison plot axis x label:",
                       value = "Position in sequence"),
             textInput(inputId = "comparison_plot_y_label",
                       label = "Comparison plot axis y label:",
                       value = "")
      ),
      column(width = 2,
             numericInput_h(inputId = "comparison_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "comparison_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "comparison_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5)
             
      )
    )
    ,
    fluidRow(
      column(width = 10,
             textInput(inputId = "woods_plot_title",
                       label = "Woods plot title:",
                       value = ""),
             textInput(inputId = "woods_plot_x_label",
                       label = "Woods plot axis x label:",
                       value = "Position in sequence"),
             textInput(inputId = "woods_plot_y_label",
                       label = "Woods plot axis y label:",
                       value = "")),
      column(width = 2,
             numericInput_h(inputId = "woods_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "woods_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "woods_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5)
             
      )
    ),
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

comp_plot_panel <- function() tabsetPanel(
  
  tabPanel("Comparison plot",
           plotOutput_h("comparisonPlot", hover = hoverOpts("comparisonPlot_hover", delay = 10, delayType = "debounce")),
           downloadButton("comparisonPlot_download_button",
                          "Save chart (.svg)")
  ),
  tabPanel("Data",
           DT::dataTableOutput("comparisonPlot_data"),
           p(
             "The empty values (e.q. `Frac DU`) mean there was not sufficient data for this peptide.",
             "Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )        
  )
)

comp_debug_panel <- function() uiOutput("comparisonPlot_debug")

diff_plot_panel <- function() tabsetPanel(
  
  tabPanel("Woods plot",
           br(),
           div(style = "position:relative",
               plotOutput_h("differentialPlot", hover = hoverOpts("differentialPlot_hover", delay = 10, delayType = "debounce"))),
           fluidRow(
             column(
               width = 6,
               downloadButton("differentialPlot_download_button",
                              "Save chart (.svg)")
             ),
             column(
               width = 6,
               actionButton(inputId = "diff_viewer_settings",
                            label = "HDXViewer"),
               br(), 
               br(),
               div(id = "diff_viewer_part",
                   wellPanel(
                     h3("Import data for HDX Viewer"),
                     h5("Feature under construction"),
                     selectInput(inputId = "diff_viewer_chain",
                                 label = "Select chain:",
                                 choices = c("A", "B"),
                                 selected = "A"),
                     selectInput(inputId = "diff_viewer_datatype",
                                 label = "Select data to import", 
                                 choices = c("Hybrid significance"),
                                 selected = c("Hybrid significance")),
                     h5("The first amino acid of the peptide is omitted."),
                     downloadButton(outputId = "diff_viewer_download_button",
                                    label = "Create file"),
                     actionButton(inputId = "diff_open_viewer", 
                                  label = "Open Page", 
                                  icon = icon("th"), 
                                  onclick ="window.open('http://proteomique.ipbs.fr:8080/', '_blank')")
                   )
                  )
             )
           )
           
           ),
  
  tabPanel("Data",
           DT::dataTableOutput("differentialPlot_data"),
           p(
             "The empty values (e.q. `Frac Diff DU`) mean there was not sufficient data for this peptide. There is a possibility that the measurement result is available for only one state of the peptide.",
             "Abbreviations from the table: Diff DU - differential deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )
  )
)

diff_debug_panel <- function() uiOutput("differentialPlot_debug")
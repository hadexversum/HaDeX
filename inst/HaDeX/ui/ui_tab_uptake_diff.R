tab_diff_uptake <- function() HaDeX_plotTab(
  
  title = "Differential uptake curves",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    diff_uptake_general_settings(),
    diff_uptake_states(),
    diff_uptake_timepoints(),
    diff_uptake_peptide(),
    diff_uptake_visualization(),
    diff_uptake_test(), 
    diff_uptake_zoom(),
    diff_uptake_labels_adjustement()
  ),
  
  displayPanel = HaDeX_plotDisplayPanel(
    diff_uptake_plot_panel()
  )
)

##########
## SIDE ##
##########

diff_uptake_general_settings <- function() HaDeX_plotSettingsSection(
  
  checkboxInput_h(inputId = "diff_kin_theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  
  checkboxInput_h(inputId = "diff_kin_fractional",
                  label = "Fractional values",
                  value = FALSE)
)

diff_uptake_states <- function() HaDeX_plotSettingsSection(
  
  title = "States",
  
  p("Differential uptake curve presents the difference between two biological states."),
  splitLayout(selectInput_h(inputId = "diff_kin_state_1",
                            label = "State 1",
                            choices = c("CD160", "CD160_HVEM")),
              selectInput_h(inputId = "diff_kin_state_2",
                            label = "State 2",
                            choices = c("CD160_HVEM", "CD160"))
  )
  
)

diff_uptake_timepoints <- function() HaDeX_plotSettingsSection(
  
  title = "Timepoints",
  
  div(id = "diff_kin_time_part",
      h5("Choose time parameters:"),
      splitLayout(
        div(id = "diff_kin_time_0_part",
            selectInput_h(inputId = "diff_kin_time_0",
                          label = "TIME IN",
                          choices = c("0", "1", "5", "25", "1440"))
        ),
        div(id = "diff_kin_time_100_part",
            selectInput_h(inputId = "diff_kin_time_100",
                          label = "TIME OUT",
                          choices = c("0", "1", "5", "25", "1440")))
      )
  )
)

##

diff_uptake_peptide <- function() HaDeX_plotSettingsSection(
  
  title = "Peptide",
  p("Choose peptide:"),
  dataTableOutput_h("diff_peptide_list_data")
)

##

diff_uptake_visualization <- function() HaDeX_plotSettingsSection(
  
  title = "Visualization",
  checkboxInput_h(inputId = "diff_kin_log_x",
                  label = "Logaritmic x scale",
                  value = TRUE),
  selectInput_h(inputId = "diff_kin_uncertainty",
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon")
)

##

diff_uptake_test <- function() HaDeX_plotSettingsSection(
  
  title = "Test",
  
  checkboxInput_h(inputId = "diff_kin_houde",
                  label = "Houde test",
                  value = FALSE),
  checkboxInput_h(inputId = "diff_kin_tstud", 
                  label = "t-Student test", 
                  value = FALSE)
)

##

diff_uptake_zoom <- function() HaDeX_plotSettingsSection(
  
  title = "Zoom",
  sliderInput(inputId = 'diff_kin_plot_y_range',
              label = 'Choose y range for the plot:',
              min = -50,
              max = 200,
              value = c(-10, 100),
              step = 10)
)

##

diff_uptake_labels_adjustement <- function() HaDeX_plotSettingsSection(
  
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-uptake-labels-adjusting-panel"
  ),
  
  HaDeX_collapsablePanel(
    id = "HaDeX-uptake-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = "diff_kin_plot_title",
                       label = "Uptake curve title:",
                       value = "Uptake curve for chosen peptides"),
             textInput(inputId = "diff_kin_plot_x_label",
                       label = "Uptake curve axis x label:",
                       value = "Time point [min]"),
             textInput(inputId = "diff_kin_plot_y_label",
                       label = "Uptake curve axis y label:",
                       value = "Deuteration")),
      column(width = 2,
             numericInput_h(inputId = "diff_kin_plot_title_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "diff_kin_plot_x_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = "diff_kin_plot_y_label_size",
                            label = "Size:",
                            value = 15,
                            min = 5))
    ), 
    
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

##########
## MAIN ##
##########

diff_uptake_plot_panel <- function() tabsetPanel(
  
  ##### UPTAKE CURVE ##### 
  
  tabPanel(
    "Differential uptake curve",
    plotOutput_h("diff_kinetic_plot_chosen_peptides", hover = hoverOpts("diff_kinetic_plot_chosen_peptides_hover", delay = 10, delayType = "debounce"), height = 600),
    uiOutput("diff_kinetic_plot_chosen_peptides_debug"),
    downloadButton("diff_kineticPlot_download_button",
                   "Save chart (.svg)")),
  
  ##### UPTAKE CURVE DATA #####
  
  tabPanel("Data",
           br(),
           DT::dataTableOutput("diff_kin_plot_data")),
  
  ## DOWNLOAD ALL PLOTS ##
  
  tabPanel("Download",
           p(
             "Download uptake curves for all peptides in selected form, based on the parameters from the Settings panel.",
             "Preparing the plots may take a while."
           ),
           
           
           fluidRow(
             column(width = 5,
                    h5("Download the zipped folder with separate files:"),
                    downloadButton(outputId = "diff_kin_download_folder",
                                   label = "Download folder")
             ),
             column(width = 4,
                    h5("Download the pdf file with plots arranged in a grid:"),
                    downloadButton(outputId = "diff_kin_download_file",
                                   label = "Download file"),
                    
                    numericInput_h(inputId = "diff_kin_download_file_columns",
                                   label = "Select number of columns on a page",
                                   min = 1, max = 5,
                                   value = 2),
                    numericInput_h(inputId = "diff_kin_download_file_rows",
                                   label = "Select number of rows on a page",
                                   min = 1, max = 5,
                                   value = 2)
             )
           )
  )
)
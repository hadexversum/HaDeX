tab_uptake <- function() HaDeX_plotTab(
  title = "Uptake curves",
  settingsPanel = HaDeX_plotSettingsPanel(
    uptake_general_settings(),
    uptake_timepoints(),
    uptake_peptide(),
    uptake_visualization(),
    uptake_zoom(),
    uptake_labels_adjustement()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    uptake_plot_panel()
  )
)

uptake_general_settings <- function() HaDeX_plotSettingsSection(
  checkboxInput_h(inputId = "kin_theory",
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = "kin_fractional",
                  label = "Fractional values",
                  value = FALSE)
)

uptake_timepoints <- function() HaDeX_plotSettingsSection(
  title = "Timepoints",
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
      )
  )
)
 
uptake_peptide <- function() HaDeX_plotSettingsSection(
  title = "Peptide",
  p("Choose peptide:"),
  dataTableOutput_h("peptide_list_data"),
  
  actionButton(inputId = "reset_peptide_list",
               label = "Reset chosen peptides")
)

uptake_visualization <- function() HaDeX_plotSettingsSection(
  title = "Visualization",
  checkboxInput_h(inputId = "kin_log_x",
                  label = "Logaritmic x scale",
                  value = TRUE),
  selectInput_h(inputId = "kin_uncertainty",
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon")
)

uptake_zoom <- function() HaDeX_plotSettingsSection(
  title = "Zoom",
  sliderInput(inputId = 'kin_plot_y_range',
              label = 'Choose y range for the plot:',
              min = -50,
              max = 200,
              value = c(-10, 100),
              step = 10)
)

uptake_labels_adjustement <- function() HaDeX_plotSettingsSection(
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-uptake-labels-adjusting-panel"
  ),
  HaDeX_collapsablePanel(
    id = "HaDeX-uptake-labels-adjusting-panel",
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
    p("The axis ticks have the same size as the axis label. The legend text size is the same as the x axis label.")
  )
)

uptake_plot_panel <- function() tabsetPanel(
  
  ##### UPTAKE CURVE ##### 
  
  tabPanel(
    "Uptake curve",
    plotOutput_h("kinetic_plot_chosen_peptides", hover = hoverOpts("kinetic_plot_chosen_peptides_hover", delay = 10, delayType = "debounce"), height = 600),
    uiOutput("kinetic_plot_chosen_peptides_debug"),
    downloadButton("kineticPlot_download_button",
                   "Save chart (.svg)")),
  tabPanel("Data",
           br(),
           DT::dataTableOutput("kin_plot_data")),
  
  ## DOWNLOAD ALL PLOTS ##
  
  tabPanel("Download",
           p(
             "Download uptake curves for all peptides in selected form, based on the parameters from the Settings panel.",
             "Preparing the plots may take a while."
           ),
           h5("Download the zipped folder with separate files:"),
           downloadButton(outputId = "kin_download_folder",
                          label = "Download folder"),
           h5("Download the pdf file with plots arranged in a grid:"),
           fluidRow(
             column(width = 4,
                    downloadButton(outputId = "kin_download_file",
                                   label = "Download file"),
                    
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
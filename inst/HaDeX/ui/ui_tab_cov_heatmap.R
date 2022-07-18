tab_cov_heatmap <- function() HaDeX_plotTab(
  
  title = "Coverage heatmap",
  
  settingsPanel = HaDeX_plotSettingsPanel(
    cov_heatmap_value(),
    cov_heatmap_time_t()
  ),
  displayPanel = HaDeX_plotDisplayPanel(
    h3("Page under construction, works only for example data."),
    br(),
    cov_heatmap_plot_panel()
  )
  
)

##

cov_heatmap_value <- function() HaDeX_plotSettingsSection(
  
  selectInput_h(inputId = "cov_heat_value",
                label = "Select value:",
                choices = c("theo_frac_deut_uptake", "deut_uptake", "frac_deut_uptake", "theo_deut_uptake", "diff_frac_deut_uptake", "diff_deut_uptake", "diff_theo_frac_deut_uptake", "diff_theo_deut_uptake", "err_frac_deut_uptake", "err_deut_uptake", "err_theo_frac_deut_uptake", "err_theo_deut_uptake", "err_diff_frac_deut_uptake", "err_diff_deut_uptake", "err_diff_theo_frac_deut_uptake", "err_diff_theo_deut_uptake"),
                selected = "frac_deut_uptake")
  
)

cov_heatmap_time_t <- function() HaDeX_plotSettingsSection(
 
  radioButtons(inputId = "cov_heat_time_t",
               label = "Select time point:",
               choices = c(0.001, 0.167, 1.000, 5.000, 25.000, 120.000, 1440.000),
               selected = 1)
  
)

##

cov_heatmap_plot_panel <- function() tabsetPanel(
  
  tabPanel("Coverage heatmap",
           plotOutput_h("coverageHeatmapPlot")
  ),
  tabPanel("Data",
           DT::dataTableOutput("coverageHeatmapPlot_data"),
           p("Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value.")
  )
)
tab_report <- function() HaDeX_nonplotTab(
  
  title = "Report",
  
  wellPanel(
    report_elements()
  )
  
  
  
)

report_elements <- function() HaDeX_plotSettingsSection(
  
  title = "Select elements for the raport",
  
 
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
             # checkboxInput(inputId = "export_theo_comparison_plot",
             #               label = "Theoretical Comparison Plot",
             #               value = TRUE),
             checkboxInput(inputId = "export_woods_plot",
                           label = "Woods Plot",
                           value = TRUE),
             # checkboxInput(inputId = "export_theo_woods_plot",
             #               label = "Theoretical Woods Plot",
             #               value = TRUE),
             checkboxInput(inputId = "export_kin_plot",
                           label = "Uptake Curve",
                           value = FALSE),
             # checkboxInput(inputId = "export_theo_kin_plot",
             #               label = "Theoretical Uptake Curve",
             #               value = FALSE),
             checkboxInput(inputId = "export_quality_control_plot",
                           label = "Quality Control Plot",
                           value = FALSE),
             checkboxInput(inputId = "export_butterfly_plot",
                           label = "Butterfly Plot",
                           value = FALSE),
             checkboxInput(inputId = "export_butterfly_differential_plot",
                           label = "Butterfly Differential Plot",
                           value = FALSE),
             checkboxInput(inputId = "export_volcano_plot",
                           label = "Volcano Plot",
                           value = FALSE),
             checkboxInput(inputId = "export_chiclet_plot",
                           label = "Chiclet Plot",
                           value = FALSE),
             checkboxInput(inputId = "export_chiclet_differential_plot",
                           label = "Chiclet Differential Plot",
                           value = FALSE),
             checkboxInput(inputId = "export_measures_plots",
                           label = "Measurement Plots",
                           value = FALSE),
             checkboxInput(inputId = "export_replicate_histograms",
                           label = "Replicate Histograms",
                           value = FALSE),
             actionButton(inputId = "export_all_plots",
                          label = "Select all plots")
      ),
      column(6,
             checkboxInput(inputId = "export_overlap_dist_data",
                           label = "Position Frequency Data"),
             checkboxInput(inputId = "export_overlap_graph_data",
                           label = "Peptide Coverage Data"),
             checkboxInput(inputId = "export_comparison_plot_data",
                           label = "Comparison Plot Data"),
             # checkboxInput(inputId = "export_theo_comparison_plot_data",
             #               label = "Theoretical Comparison Plot Data"),
             checkboxInput(inputId = "export_woods_plot_data",
                           label = "Woods Plot Data"),
             # checkboxInput(inputId = "export_theo_woods_plot_data",
             #               label = "Theoretical Woods Plot Data"),
             checkboxInput(input = "export_kin_plot_data",
                           label = "Uptake Curve Data"),
             # checkboxInput(inputId = "export_theo_kin_plot_data",
             #               label = "Theoretical Uptake Curve Data"),
             checkboxInput(inputId = "export_quality_control_plot_data",
                           label = "Quality Control Plot Data"),
             checkboxInput(inputId = "export_butterfly_plot_data",
                           label = "Butterfly Plot Data"),
             checkboxInput(inputId = "export_butterfly_differential_plot_data",
                           label = "Butterfly Differential Plot Data"),
             checkboxInput(inputId = "export_volcano_plot_data",
                           label = "Volcano Plot Data"),
             checkboxInput(inputId = "export_chiclet_plot_data",
                           label = "Chiclet Plot Data"),
             checkboxInput(inputId = "export_chiclet_differential_plot_data",
                           label = "Chiclet Differential Plot Data"),
             checkboxInput(inputId = "export_replicate_plots_data",
                           label = "Replicate Plots (mass, charge) Data"),
             checkboxInput(inputId = "export_replicate_histograms_data",
                           label = "Replicate Histograms (one and all time points) Data"),
             actionButton(inputId = "export_all_data",
                          label = "Select all data")
      )
    ),
    br(),
    p("Elements chosen for report have the same parameters as chosen in suitable panels e.g. axis range, plot title or theoretical maximal exchange control. Adjust parameters as needed in the report."),
    p(textOutput("report_message_uptake_curve")),
    p(textOutput("report_message_replicate")),
  
  downloadButton(outputId = "export_action",
                 label = "  Create report!",
                 icon = icon("fas fa-download"),
                 style = "background-color:red")
  
  
  
  
)
  
  

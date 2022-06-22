#################################
######### SETTINGS ##############
#################################

observeEvent(input[["export_all_plots"]], {
  
  updateCheckboxInput(session, 
                      inputId = "export_overlap_dist",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_overlap_graph",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_comparison_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_woods_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_kin_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_quality_control_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_butterfly_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_butterfly_differential_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_volcano_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_chiclet_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_chiclet_differential_plot",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_measures_plots",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_replicate_histograms",
                      value = TRUE)
  
  
})

##

observeEvent(input[["export_all_data"]], {
  
  updateCheckboxInput(session, 
                      inputId = "export_overlap_dist_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_overlap_graph_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_comparison_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_woods_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_kin_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_quality_control_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_butterfly_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_butterfly_differential_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_volcano_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_chiclet_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_chiclet_differential_plot_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_replicate_plots_data",
                      value = TRUE)
  
  updateCheckboxInput(session, 
                      inputId = "export_replicate_histograms_data",
                      value = TRUE)
  
  
})

#################################
######### DOWNLOAD ##############
#################################

output[["report_message_uptake_curve"]] <- renderText({
  
  if(nrow(peptide_list()[input[["peptide_list_data_rows_selected"]], ]) == 0){
    
    kin_peptides <- "None"
    
  } else {
    
    kin_peptides <- paste0(unlist(unique(peptide_list()[input[["peptide_list_data_rows_selected"]], ][["Sequence"]])), collapse = ", ")
    
  }
  
  paste0("Chosen peptides for uptake curve plot: ", kin_peptides, "\n")
  
})

##

output[["report_message_replicate"]] <- renderText({
  
  # browser()
  
  if(nrow(measures_peptide_list()[input[["measures_sequence_rows_selected"]], ]) == 0){
    
    rep_peptide <- "None"
  
  } else{
      
    rep_peptide <- measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1][[1]]
    
  }
  
  paste0("Chosen peptide for measurement plot: ", rep_peptide, " \n")
  
})

##

output[["export_action"]] <- downloadHandler(

  # browser()
  
  filename <- "HaDeX_Report.html",
  
  content <- function(file) {
    
    rmarkdown::render(input = "report_template.Rmd",
                      output_file = file, quiet = TRUE)
    
  }
  
)
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
  
  if(nrow(rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2]) == 0){
    
    rep_peptide <- "None"
  
  } else{
      
    rep_peptide <- rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2][[1]]
    
  }
  
  paste0("Chosen peptide for replicate plot: ", rep_peptide, " \n")
  
})

##

output[["export_action"]] <- downloadHandler(
  
  filename <- "HaDeX_Report.html",
  
  content <- function(file) {
    
    rmarkdown::render(input = "report_template.Rmd",
                      output_file = file, quiet = TRUE)
    
})
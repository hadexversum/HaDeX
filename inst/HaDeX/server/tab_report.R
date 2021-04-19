output[["export_action"]] <- downloadHandler(
  
  filename <- "HaDeX_Report.html",
  
  content <- function(file) {
    
    rmarkdown::render(input = "report_template.Rmd",
                      output_file = file, quiet = TRUE)
    
})
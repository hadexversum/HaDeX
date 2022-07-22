summary_data <- reactive({
  
  validate(need(!is.null(input[["confidence_level"]]), "Wait for the parameters to be loaded."))
  
  show_summary_data(dat = dat(),
                    confidence_level = as.double(input[["confidence_level"]]),
                    protein_length = max_range())

  })

##

output[["summary_table"]] <- DT::renderDataTable(server = FALSE, {
  
  datatable(data = summary_data(),
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tBi", autoWidth = TRUE, buttons = c("excel", "pdf")),
            filter = "bottom",
            rownames = FALSE)
  
})
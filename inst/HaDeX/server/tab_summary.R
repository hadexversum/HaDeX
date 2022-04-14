summary_data <- reactive({
  
  # browser()
  
  show_summary_data(dat = dat(),
                    confidence_limit_1 = input[["confidence_limit"]],
                    confidence_limit_2 = input[["confidence_limit_2"]],
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
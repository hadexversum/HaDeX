
cov_heat_dataset <- reactive({
  
  if(grepl("diff", input[["cov_heat_value"]])){
    
    validate(need(length(states_from_file())>1, "There is only one biological state."))
    
    calculate_diff_uptake(dat = dat(),
                          time_t = as.numeric(input[["cov_heat_time_t"]]))
    
  } else if (input[["cov_heat_value"]] == "auc") {
    
    tmp_dat <- dat()[dat()[["Exposure"]] < 99999, ]
    calculate_auc(create_uptake_dataset(tmp_dat), preserve_values = F)
    
  } else if (input[["cov_heat_value"]] == "back_exchange") {
    
    tmp_dat <- dat()[dat()[["Exposure"]] < 99999, ]
    calculate_back_exchange(tmp_dat, 
                            states = dat()[["State"]][1])
  } else {
    
      calculate_state_uptake(dat = dat(),
                             time_t = as.numeric(input[["cov_heat_time_t"]]))
  }
  
})



output[["coverageHeatmapPlot"]] <- renderPlot({
  
  # browser()
  
  plot_coverage_heatmap(x_dat = cov_heat_dataset(),
                        value = input[["cov_heat_value"]])
  
}) 
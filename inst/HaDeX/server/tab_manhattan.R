#################################
######### DATASET ###############
#################################

mannhattan_data <- reactive({
  
  validate(need(input[["mann_state_1"]]!=input[["mann_state_2"]], "Choose two different states for comparison!"))
  
  create_p_diff_uptake_dataset(dat = dat(),
                               protein = input[["chosen_protein"]],
                               state_1 = input[["mann_state_1"]],
                               state_2 = input[["mann_state_2"]],
                               p_adjustment_method = input[["mann_p_adjustment_method"]])
  
})



#################################
######### PLOT ##################
#################################

manhattan_plot_out <- reactive({
  
  plot_manhattan(p_dat = mannhattan_data(),
                 plot_title = paste0("Difference between ", input[["mann_state_1"]], " and ", input[["mann_state_2"]]),
                 confidence_level = as.numeric(input[["mann_confidence_level"]]))
  
  
})

##

output[["manhattanPlot"]] <- renderPlot({
  
  manhattan_plot_out() 
  
})


#################################
######### DATA ##################
#################################

manhattan_plot_data_out <- reactive({
  
  mannhattan_data() %>%
    select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, P_value, log_p_value) %>%
    mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
           P_value = round(P_value, 4),
           log_p_value = round(log_p_value, 4))
  
})

##
output[["manhattanPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  manhattan_plot_data_out() %>%
    dt_format()
  
})
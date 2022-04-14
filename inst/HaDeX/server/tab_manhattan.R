#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "man_state_1",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
  
  updateSelectInput(session,
                    inputId = "man_state_2",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[length(states_chosen_protein())])
  
})

##

observe({
  
  updateCheckboxGroupInput(session,
                           inputId = "man_times",
                           choices = times_t(), 
                           selected = times_t())  
  
  
})

#################################
######### DATASET ###############
#################################

mannhattan_data <- reactive({
  
  validate(need(input[["man_state_1"]]!=input[["man_state_2"]], "Choose two different states for comparison!"))
  
  create_p_diff_uptake_dataset(dat = dat(),
                               protein = input[["chosen_protein"]],
                               state_1 = input[["man_state_1"]],
                               state_2 = input[["man_state_2"]],
                               p_adjustment_method = input[["man_p_adjustment_method"]])
  
})



#################################
######### PLOT ##################
#################################

manhattan_plot_out <- reactive({
  
  plot_manhattan(p_dat = mannhattan_data(),
                 plot_title = paste0("Difference between ", input[["man_state_1"]], " and ", input[["man_state_2"]]),
                 confidence_level = as.numeric(input[["man_confidence_level"]]),
                 times = input[["man_times"]],
                 separate_times = input[["man_separate_times"]],
                 show_confidence_limit = TRUE)
  
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
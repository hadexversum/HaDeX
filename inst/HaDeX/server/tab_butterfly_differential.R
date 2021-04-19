#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "butt_diff_state_first",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  updateSelectInput(session,
                    inputId = "butt_diff_state_second",
                    choices = states_from_file(),
                    selected = states_from_file()[2])
  
  if(input[["butt_diff_fractional"]]){
    
    times_t <- times_from_file()[times_from_file() > input[["butt_diff_time_0"]] & times_from_file() < as.numeric(input[["butt_diff_time_100"]])]
    
  } else {
    
    times_t <- times_from_file()[times_from_file() > input[["butt_diff_time_0"]] & times_from_file() < 99999]
    
  }
  
  updateCheckboxGroupInput(session,
                           inputId = "butt_diff_timepoints",
                           choices = times_t,
                           selected = times_t)
})

##

observe({
  
  updateSelectInput(session, 
                    inputId = "butt_diff_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
  
  updateSelectInput(session, 
                    inputId = "butt_diff_time_100",
                    choices = times_from_file(),
                    selected = max(times_from_file()[times_from_file() < 99999]))
})

##

observe({
  
  updateTextInput(session,
                  inputId = "butterflyDifferential_plot_title",
                  value = case_when(
                    input[["butt_diff_theory"]] ~ paste0("Thereotical butterfly differential plot between ", input[["butt_diff_state_first"]], " and ", input[["butt_diff_state_second"]]),
                    !input[["butt_diff_theory"]] ~ paste0("Butterfly differential plot between ", input[["butt_diff_state_first"]], " and ", input[["butt_diff_state_second"]])
                  ))
  
  updateTextInput(session,
                  inputId = "butterflyDifferential_plot_y_label",
                  value = case_when(
                    input[["butt_diff_fractional"]] ~ "Fractional deuterium uptake difference [%]",
                    !input[["butt_diff_fractional"]] ~ "Deuterium uptake difference [Da]"
                  ))
  
})


##

observe({
  
  if (input[["butt_diff_fractional"]]) {
    
    max_y <- ceiling(max(butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_frac_deut_uptake"]]), ][["diff_frac_deut_uptake"]], 
                         butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_theo_frac_deut_uptake"]]), ][["diff_theo_frac_deut_uptake"]])) + 1
    min_y <- floor(min(butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_frac_deut_uptake"]]), ][["diff_frac_deut_uptake"]], 
                       butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_theo_frac_deut_uptake"]]), ][["diff_theo_frac_deut_uptake"]])) - 1
    
  } else {
    
    max_y <- ceiling(max(butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_deut_uptake"]]), ][["diff_deut_uptake"]], 
                         butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_theo_deut_uptake"]]), ][["diff_theo_deut_uptake"]])) + 1
    min_y <- floor(min(butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_deut_uptake"]]), ][["diff_deut_uptake"]], 
                       butt_diff_dataset()[!is.na(butt_diff_dataset()[["diff_theo_deut_uptake"]]), ][["diff_theo_deut_uptake"]])) - 1
  }
  
  max_x <- max(butt_diff_dataset()[["ID"]])
  min_x <- min(butt_diff_dataset()[["ID"]])
  
  updateSliderInput(
    session,
    inputId = "butt_diff_x_range",
    min = min_x,
    max = max_x,
    value = c(min_x, max_x)
  )
  
  updateSliderInput(
    session,
    inputId = "butt_diff_y_range",
    min = min_y,
    max = max_y,
    value = c(min_y, max_y)
  )
  
})


#################################
######### DATASET ###############
#################################

butt_diff_dataset <- reactive({
  
  validate(need(input[["butt_diff_state_first"]]!=input[["butt_diff_state_second"]], "There is no difference between the same state, choose different second state."))
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  
  generate_butterfly_differential_dataset(dat(),
                                          protein = input[["chosen_protein"]],
                                          state_1 = input[["butt_diff_state_first"]],
                                          state_2 = input[["butt_diff_state_second"]],
                                          time_0 = as.numeric(input[["butt_diff_time_0"]]),
                                          time_100 = as.numeric(input[["butt_diff_time_100"]]),
                                          deut_part = input[["deut_part"]])
})

##

butt_diff_dat <- reactive({
  
  butt_diff_dataset() %>%
    filter(Exposure %in% input[["butt_diff_timepoints"]])
  
})

#################################
######### PLOT ##################
#################################

butterfly_differential_plot_out <- reactive({
  
  generate_butterfly_differential_plot(butt_diff_dat(),
                                       theoretical = input[["butt_diff_theory"]],
                                       fractional = input[["butt_diff_fractional"]],
                                       uncertainty_type = input[["butt_diff_uncertainty"]]) + 
    coord_cartesian(xlim = c(input[["butt_diff_x_range"]][[1]], input[["butt_diff_x_range"]][[2]]),
                    ylim = c(input[["butt_diff_y_range"]][[1]], input[["butt_diff_y_range"]][[2]])) +
    labs(title = input[["butterflyDifferential_plot_title"]],
         x = input[["butterflyDifferential_plot_x_label"]],
         y = input[["butterflyDifferential_plot_y_label"]]) +
    theme(plot.title = element_text(size = input[["butterflyDifferential_plot_title_size"]]),
          axis.text.x = element_text(size = input[["butterflyDifferential_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["butterflyDifferential_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["butterflyDifferential_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["butterflyDifferential_plot_y_label_size"]]),
          legend.text = element_text(size = input[["butterflyDifferential_plot_x_label_size"]]),
          legend.title = element_text(size = input[["butterflyDifferential_plot_x_label_size"]]))
  
})

##

output[["butterflyDifferentialPlot"]] <- renderPlot({
  
  butterfly_differential_plot_out()
  
})

#################################
######### DATA ##################
#################################

output[["butterflyDifferentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  generate_butterfly_differential_data(butt_diff_dat(),
                                       theoretical = input[["butt_diff_theory"]],
                                       fractional = input[["butt_diff_fractional"]]) %>%
    filter(ID >= input[["butt_diff_x_range"]][[1]] & ID <= input[["butt_diff_x_range"]][[2]]) %>%
    dt_format()
  
})



#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "chic_diff_state_first",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  updateSelectInput(session,
                    inputId = "chic_diff_state_second",
                    choices = states_from_file(),
                    selected = states_from_file()[2])
  
  if(input[["chic_diff_fractional"]]){
    
    times_t <- times_from_file()[times_from_file() > input[["chic_diff_time_0"]] & times_from_file() < as.numeric(input[["chic_diff_time_100"]])]
    
  } else {
    
    times_t <- times_from_file()[times_from_file() > input[["chic_diff_time_0"]] & times_from_file() < 99999]
    
  }
  
  updateCheckboxGroupInput(session,
                           inputId = "chic_diff_timepoints",
                           choices = times_t,
                           selected = times_t)
})

##

observe({
  
  updateSelectInput(session, 
                    inputId = "chic_diff_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
  
  updateSelectInput(session,
                    inputId = "chic_diff_time_100",
                    choices = times_with_control(),
                    selected = max(times_with_control()[times_with_control() < 99999]))
})

##

observe({
  
  updateTextInput(session,
                  inputId = "chicletDifferential_plot_title",
                  value = case_when(
                    input[["chic_diff_theory"]] ~ paste0("Thereotical chiclet differential plot between ", input[["chic_diff_state_first"]], " and ", input[["chic_diff_state_second"]]),
                    !input[["chic_diff_theory"]] ~ paste0("Chiclet differential plot between ", input[["chic_diff_state_first"]], " and ", input[["chic_diff_state_second"]])
                  ))

})

##

observe({
  
  max_x <- max(chiclet_diff_dataset()[["ID"]])
  min_x <- min(chiclet_diff_dataset()[["ID"]])
  
  updateSliderInput(
    session,
    inputId = "chic_diff_x_range",
    min = min_x,
    max = max_x,
    value = c(min_x, max_x)
  )

})

##

observe({
  
  if(input[["chic_diff_theory"]]){
    hide(id = "chic_diff_time_0_part")
    hide(id = "chic_diff_time_100_part")
  }
  
})

##

observe({
  
  if(!input[["chic_diff_theory"]]){
    show(id = "chic_diff_time_0_part")
    show(id = "chic_diff_time_100_part")
  }
  
})

##

observe({
  
  if(!input[["chic_diff_fractional"]]){
    hide(id = "chic_diff_time_100_part")
  }
  
})

##

observe({
  
  if(input[["chic_diff_fractional"]] & !input[["chic_diff_theory"]]){
    show(id = "chic_diff_time_100_part")
  }
  
})

#################################
######### DATASET ###############
#################################

chiclet_diff_dataset <- reactive({
  
  validate(need(input[["chic_diff_state_first"]]!=input[["chic_diff_state_second"]], "There is no difference between the same state, choose different second state."))
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  
  generate_butterfly_differential_dataset(dat(),
                                          protein = input[["chosen_protein"]],
                                          state_1 = input[["chic_diff_state_first"]],
                                          state_2 = input[["chic_diff_state_second"]],
                                          time_0 = as.numeric(input[["chic_diff_time_0"]]),
                                          time_100 = as.numeric(input[["chic_diff_time_100"]]),
                                          deut_part = as.numeric(input[["deut_part"]])/100)
})

##

chiclet_diff_dataset_timepoints <- reactive({
  
  
  chiclet_diff_dataset() %>%
    filter(Exposure %in% input[["chic_diff_timepoints"]])
  
})

#################################
######### PLOT ##################
#################################

chiclet_differential_plot_out <- reactive({
  
  # browser()
  
  generate_chiclet_differential_plot(chiclet_diff_dataset_timepoints(),
                                     theoretical = input[["chic_diff_theory"]],
                                     fractional = input[["chic_diff_fractional"]],
                                     show_uncertainty = input[["chic_diff_show_uncertainty"]]) + 
    coord_cartesian(xlim = c(input[["chic_diff_x_range"]][[1]], input[["chic_diff_x_range"]][[2]])) +
    labs(title = input[["chicletDifferential_plot_title"]],
         x = input[["chicletDifferential_plot_x_label"]],
         y = input[["chicletDifferential_plot_y_label"]]) +
    theme(plot.title = element_text(size = input[["chicletDifferential_plot_title_size"]]),
          axis.text.x = element_text(size = input[["chicletDifferential_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["chicletDifferential_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["chicletDifferential_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["chicletDifferential_plot_y_label_size"]]),
          legend.text = element_text(size = input[["chicletDifferential_plot_x_label_size"]]),
          legend.title = element_text(size = input[["chicletDifferential_plot_x_label_size"]]))
  
})

##

output[["chicletDifferentialPlot"]] <- renderPlot({
  
  chiclet_differential_plot_out()
  
})

##

output[["chicletDifferentialPlot_download_button"]] <- downloadHandler("chicletDifferentialPlot.svg",
                                                                         content = function(file) {
                                                                           ggsave(file, chiclet_differential_plot_out(), device = svg,
                                                                                  height = 300, width = 400, units = "mm")
                                                                         })
##

## TODO: HOVER

#################################
######### DATA ##################
#################################

chiclet_differential_plot_data_out <- reactive({
  
  chiclet_diff_dataset_timepoints() %>%
    generate_chiclet_differential_data(theoretical = input[["chic_diff_theory"]],
                                       fractional = input[["chic_diff_fractional"]]) %>%
    filter(ID >= input[["chic_diff_x_range"]][[1]] & ID <= input[["chic_diff_x_range"]][[2]]) 
  
})

##

output[["chicletDifferentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  chiclet_differential_plot_data_out() %>%
    dt_format()
  
})
#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "chic_state",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  if(input[["chic_fractional"]]){
    
    times_t <- times_from_file()[times_from_file() > input[["chic_time_0"]] & times_from_file() < as.numeric(input[["chic_time_100"]])]
    
  } else {
    
    times_t <- times_from_file()[times_from_file() > input[["chic_time_0"]] & times_from_file() < 99999]
    
  }
  
  updateCheckboxGroupInput(session,
                           inputId = "chic_timepoints",
                           choices = times_t,
                           selected = times_t)
  
})

##

observe({
  
  updateSelectInput(session, 
                    inputId = "chic_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
  
  updateSelectInput(session,
                    inputId = "chic_time_100",
                    choices = times_with_control(),
                    selected = max(times_with_control()[times_with_control() < 99999]))
})

##

observe({
  
  if(input[["chic_fractional"]]){
    
    max_y <- ceiling(max(chiclet_dataset()[["frac_deut_uptake"]], chiclet_dataset()[["theo_frac_deut_uptake"]])) + 1
    min_y <- floor(min(chiclet_dataset()[["frac_deut_uptake"]], chiclet_dataset()[["theo_frac_deut_uptake"]])) - 1
    
  } else {
    
    max_y <- ceiling(max(chiclet_dataset()[["deut_uptake"]], chiclet_dataset()[["theo_deut_uptake"]])) + 1
    min_y <- floor(min(chiclet_dataset()[["deut_uptake"]], chiclet_dataset()[["theo_deut_uptake"]])) - 1
  }

  updateSliderInput(session,
                    inputId = "chic_y_range",
                    min = min_y,
                    max = max_y,
                    value = c(min_y, max_y))
  
})

##

observe({
  
  max_x <- max(chiclet_dataset()[["ID"]])
  min_x <- min(chiclet_dataset()[["ID"]])
  
  updateSliderInput(session,
                    inputId = "chic_x_range",
                    min = min_x,
                    max = max_x,
                    value = c(min_x, max_x))
  

})

##

observe({
  
  updateTextInput(session,
                  inputId = "chiclet_plot_title",
                  value = case_when(
                    input[["butt_theory"]] ~ paste0("Theoreotical chiclet plot for ", input[["chic_state"]], " state for ", input[["chosen_protein"]]),
                    !input[["butt_theory"]] ~ paste0("Chiclet plot for ", input[["chic_state"]], " state for ", input[["chosen_protein"]])
                  ))
})

##

observe({
  
  if(input[["chic_theory"]]){
    hide(id = "chic_time_0_part")
    hide(id = "chic_time_100_part")
  }
  
})

##

observe({
  
  if(!input[["chic_theory"]]){
    show(id = "chic_time_0_part")
    show(id = "chic_time_100_part")
  }
  
})

##

observe({
  
  if(!input[["chic_fractional"]]){
    hide(id = "chic_time_100_part")
  }
  
})

##

observe({
  
  if(input[["chic_fractional"]] & !input[["chic_theory"]]){
    show(id = "chic_time_100_part")
  }
  
})


#################################
######### DATASET ###############
#################################

chiclet_dataset <- reactive({
  
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  
  generate_butterfly_dataset(dat(),
                             protein = input[["chosen_protein"]],
                             state = input[["chic_state"]],
                             time_0 = as.numeric(input[["chic_time_0"]]),
                             time_100 = as.numeric(input[["chic_time_100"]]),
                             deut_part = as.numeric(input[["deut_part"]])/100)
  
})

##

chiclet_dataset_timepoints <- reactive({
  
  chiclet_dataset() %>%
    filter(Exposure %in% input[["chic_timepoints"]])
  
})

##

chiclet_plot_out <- reactive({
  
  generate_chiclet_plot(chiclet_dataset_timepoints(),
                        theoretical = input[["chic_theory"]],
                        fractional = input[["chic_fractional"]],
                        show_uncertainty = input[["chic_show_uncertainty"]]) +
    coord_cartesian(xlim = c(input[["chic_x_range"]][[1]], input[["chic_x_range"]][[2]])) +
                    # ylim = c(input[["chic_y_range"]][[1]], input[["chic_y_range"]][[2]])) +
    labs(title = input[["chiclet_plot_title"]],
         x = input[["chiclet_plot_x_label"]],
         y = input[["chiclet_plot_y_label"]]) +
    theme(plot.title = element_text(size = input[["chiclet_plot_title_size"]]),
          axis.text.x = element_text(size = input[["chiclet_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["chiclet_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["chiclet_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["chiclet_plot_y_label_size"]]),
          legend.text = element_text(size = input[["chiclet_plot_x_label_size"]]),
          legend.title = element_text(size = input[["chiclet_plot_x_label_size"]]))
  
})

##

output[["chicletPlot"]] <- renderPlot({
  
  chiclet_plot_out()
  
})

#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "un_state",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
  
})

observe({
  
  times <- times_from_file()[times_from_file() < 99999]
    
  updateCheckboxGroupInput(session,
                           inputId = "un_timepoints",
                           choices = times, 
                           selected = times)  
})

#################################
######### DATASET ###############
#################################

uncertainty_dat <- reactive({
  
  dat() %>%
    filter(Exposure %in% as.numeric(input[["un_timepoints"]]))
  
})

#################################
######### PLOT ##################
#################################

uncertainty_plot <- reactive({
  
  plot_uncertainty(dat = uncertainty_dat(),
                   protein = input[["chosen_protein"]],
                   state = input[["un_state"]],
                   aggregated = input[["un_aggregated"]],
                   separate_times = input[["un_separate_times"]])
  
})

uncertainty_plot_out <- reactive({
  
  ## prepared for plot adjustements
  uncertainty_plot()   
  
})

output[["uncertaintyPlot"]] <- renderPlot({
  
  uncertainty_plot_out()
    
})

output[["uncertaintyPlot_download_button"]] <- downloadHandler("uncertaintyPlot.svg",
                                                                content = function(file) {
                                                                  ggsave(file, uncertainty_plot_out(), device = svg,
                                                                         height = 300, width = 400, units = "mm")
                                                                })

#################################
######### DATA ##################
#################################

uncertainty_plot_data <- reactive({
  
  if(input[["un_aggregated"]]){
    
    calculate_exp_masses(uncertainty_dat()) 
    
  } else {
    
    uncertainty_dat() %>% 
      mutate(exp_mass = Center*z - z*1.00727647) %>%
      select(-Inten, -Center,  -MaxUptake, -z) %>%
      group_by(Protein, Sequence, Start, End, MHP, State, Exposure) %>%
      mutate(err_avg_mass = sd(exp_mass)) %>%
      select(-exp_mass) %>%
      unique(.)
    
  }
  
})
  
output[["uncertaintyPlot_data"]] <- DT::renderDataTable(server = FALSE, {
    
  uncertainty_plot_data() %>%
    mutate(avg_mass = round(avg_mass, 4),
           err_avg_mass = round(err_avg_mass, 4)) %>%
    dt_format()

})
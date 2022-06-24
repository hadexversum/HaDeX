#################################
######### SETTINGS ##############
#################################


observe({
  
  updateSelectInput(session,
                    inputId = "measures_state",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "measures_time",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = times_from_file()[3])
})

measures_peptide_list <- reactive({
  
  dat() %>% 
    filter(Protein == input[["chosen_protein"]],
           State == input[["measures_state"]])  %>% 
    select(Sequence, Start, End) %>%
    unique(.) %>%
    arrange(Start, End)
  
})

##

output[["measures_sequence"]] <- DT::renderDataTable({
  
  datatable(data = measures_peptide_list(),
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 8, dom = "tip", autoWidth = TRUE, target = 'cell'),
            selection = list(mode = "single"),
            filter = "bottom",
            rownames = FALSE)
  
})

##

measures_list_proxy <- DT::dataTableProxy("measures_sequence", session = session)

##

observe({

  updateTextInput(session,
                  inputId = "measures_plot_title",
                  value = paste0("Measurements of ", measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1], " in ", input[["measures_time"]], " min in ", input[["measures_state"]], " state"))
  
})

observe({
  
  updateTextInput(session,
                  inputId = "mass_uptake_plot_title",
                  value = paste0("Mass uptake of ", measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1], " in ", input[["measures_state"]], " state"))
  
})

observe({
  
  # browser()

  max_mu <- round_any(max(mass_uptake_plot_data()[["mass_uptake"]], na.rm = TRUE), 1, ceiling)
    
  updateSliderInput(session,
                    inputId = "mass_uptake_plot_y_range",
                    max = max_mu + 5,
                    value = c(0, max_mu),
                    step = 1)
})

measures_plot_dat <- reactive({
  
  ## temporarly, for compability
  
  filter(data.frame(dat()), Exposure < 99999)
  
})


#################################
######### PLOT ##################
#################################
## measurements 

measures_plot_out <- reactive({
  
  validate(need(input[["measures_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  plot_peptide_mass_measurement(measures_plot_dat(),
                                protein = input[["chosen_protein"]],
                                show_charge_values = !input[["measures_show_charge"]],
                                state =  input[["measures_state"]],
                                sequence = measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1][[1]],
                                time_t = as.numeric(input[["measures_time"]]))  + 
    labs(x = input[["measures_plot_x_label"]],
         y = input[["measures_plot_y_label"]],
         title = input[["measures_plot_title"]]) +
    theme(plot.title = element_text(size = input[["measures_plot_title_size"]]),
          axis.text.x = element_text(size = input[["measures_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["measures_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["measures_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["measures_plot_y_label_size"]]),
          legend.text = element_text(size = input[["measures_plot_x_label_size"]]),
          legend.title = element_text(size = input[["measures_plot_x_label_size"]]))
  
})

##

output[["measuresPlot"]] <- renderPlot({
  
  measures_plot_out()
  
})

##

output[["measuresPlot_debug"]] <- renderUI({

})

##

output[["measuresPlot_download_button"]] <- downloadHandler("measuresPlot.svg",
                                                              content = function(file){
                                                                ggsave(file, measures_plot_out(), device = svg,
                                                                       height = 300, width = 400, units = "mm")
                                                              })


#################################
######### DATA ##################
#################################
## measurements

measures_plot_data_out <- reactive({
  
  validate(need(input[["measures_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  show_peptide_mass_measurement(calculate_exp_masses_per_replicate(measures_plot_dat()),
                                protein = input[["chosen_protein"]],
                                state =  input[["measures_state"]],
                                sequence = measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1],
                                time_t = as.numeric(input[["measures_time"]])) 
})

output[["measuresPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  measures_plot_data_out() %>%
    dt_format()
  
})


#################################
######### PLOT ##################
#################################
## mass uptake

mass_uptake_plot_out <- reactive({
  
  validate(need(input[["measures_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  plot_replicate_mass_uptake(dat = measures_plot_dat(),
                             protein = input[["chosen_protein"]],
                             state = input[["measures_state"]],
                             sequence = measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1][[1]],
                             log_x = input[["mass_uptake_log_x"]],
                             show_aggregated = input[["measures_show_charge"]]) +
    coord_cartesian(ylim = c(input[["mass_uptake_plot_y_range"]][1], input[["mass_uptake_plot_y_range"]][2])) +
    labs(x = input[["mass_uptake_plot_x_label"]],
         y = input[["mass_uptake_plot_y_label"]],
         title = input[["mass_uptake_plot_title"]]) +
    theme(plot.title = element_text(size = input[["mass_uptake_plot_title_size"]]),
          axis.text.x = element_text(size = input[["mass_uptake_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["mass_uptake_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["mass_uptake_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["mass_uptake_plot_y_label_size"]]),
          legend.title = element_text(size = input[["mass_uptake_plot_x_label_size"]]),
          legend.text = element_text(size = input[["mass_uptake_plot_x_label_size"]]))
  
  
})

output[["massUptakePlot"]] <- renderPlot({
  
  mass_uptake_plot_out()
  
})

output[["massUptakePlot_download_button"]] <- downloadHandler("massUptakePlot.svg",
                                                           content = function(file){
                                                             ggsave(file, mass_uptake_plot_out(), device = svg,
                                                                    height = 300, width = 400, units = "mm")
                                                           })

#################################
######### DATA ##################
#################################
## mass uptake

mass_uptake_plot_data <- reactive({
  
  validate(need(input[["measures_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  filtered_dat <- measures_plot_dat() %>%
    filter(Protein == input[["chosen_protein"]], 
           State == input[["measures_state"]],
           Sequence == measures_peptide_list()[input[["measures_sequence_rows_selected"]], 1][[1]],
           Exposure > as.numeric(input[["no_deut_control"]]))
    
  if(input[["measures_show_charge"]]) {
    
    filtered_dat %>%
      calculate_exp_masses_per_replicate() %>%
      mutate(mass_uptake = avg_exp_mass - MHP) 
    
  } else {
    
    filtered_dat %>%
      mutate(exp_mass = Center*z - z*1.00727647,
             weighted_Inten = scale(Inten),
             mass_uptake = exp_mass - MHP) 
  }
})

output[["massUptakePlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  if(!input[["measures_show_charge"]]) {
    
    mass_uptake_plot_data() %>%
      select(-MaxUptake, -Inten, -Center) %>%
      mutate(exp_mass = round(exp_mass, 4),
             weighted_Inten = round(weighted_Inten, 4),
             mass_uptake = round(mass_uptake, 4)) %>%
      rename("Exp Mass" = exp_mass,
             "Rel Inten" = weighted_Inten,
             "Mass Uptake" = mass_uptake) %>%
      arrange(Start, End) %>%
      dt_format()
    
  } else {
    
    mass_uptake_plot_data() %>%
      mutate(avg_exp_mass = round(avg_exp_mass, 4),
             mass_uptake = round(mass_uptake, 4)) %>%
      rename("Avg Exp Mass" = avg_exp_mass,
             "Mass Uptake" = mass_uptake) %>%
      arrange(Start, End) %>%
      dt_format()
    
  }
  
  
})
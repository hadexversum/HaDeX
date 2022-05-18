#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "chic_diff_state_1",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "chic_diff_state_2",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[length(states_chosen_protein())])
})

##

observe({
  
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
})

##

observe({
  
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
                    input[["chic_diff_theory"]] ~ paste0("Thereotical chiclet differential plot between ", input[["chic_diff_state_1"]], " and ", input[["chic_diff_state_2"]]),
                    !input[["chic_diff_theory"]] ~ paste0("Chiclet differential plot between ", input[["chic_diff_state_1"]], " and ", input[["chic_diff_state_2"]])
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
  
  validate(need(input[["chic_diff_state_1"]]!=input[["chic_diff_state_2"]], "There is no difference between the same state, select different state 2."))
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  validate(need(input[["chic_diff_state_1"]] %in% states_chosen_protein(), "Wait for the parameters to be loaded."))
  
  create_diff_uptake_dataset(dat(),
                             protein = input[["chosen_protein"]],
                             state_1 = input[["chic_diff_state_1"]],
                             state_2 = input[["chic_diff_state_2"]],
                             time_0 = as.numeric(input[["chic_diff_time_0"]]),
                             time_100 = as.numeric(input[["chic_diff_time_100"]]),
                             deut_part = as.numeric(input[["deut_part"]])/100)
})

##

chiclet_diff_dataset_timepoints <- reactive({
  
  validate(need(input[["chic_diff_timepoints"]], "Select time points on the left to see the data."))
  
  chiclet_diff_dataset() %>%
    filter(Exposure %in% input[["chic_diff_timepoints"]])
  
})

chilet_p_dif_dataset_timepoints <- reactive({
  
  create_p_diff_uptake_dataset(dat = dat(),
                               diff_uptake_dat = chiclet_diff_dataset_timepoints(),
                               protein = input[["chosen_protein"]],
                               state_1 = input[["chic_diff_state_1"]],
                               state_2 = input[["chic_diff_state_2"]],
                               confidence_level = as.numeric(input[["chic_confidence_level"]]),
                               # p_adjustment_method = input[[""]],
                               time_0 = as.numeric(input[["chic_diff_time_0"]]),
                               time_100 = as.numeric(input[["chic_diff_time_100"]]),
                               deut_part = as.numeric(input[["deut_part"]])/100)
  
})


#################################
######### PLOT ##################
#################################

chiclet_differential_plot_out <- reactive({
  
  plot_differential_chiclet(diff_uptake_dat = chiclet_diff_dataset_timepoints(),
                            diff_p_uptake_dat = chilet_p_dif_dataset_timepoints(),
                            theoretical = input[["chic_diff_theory"]],
                            fractional = input[["chic_diff_fractional"]],
                            show_houde_interval = input[["chic_diff_show_houde"]],
                            show_tstud_confidence = input[["chic_diff_show_tstud"]],
                            confidence_level = as.numeric(input[["chic_confidence_level"]]),
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

output[["chicletDifferentialPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["chicletDifferentialPlot_hover"]])) {
    
    plot_data <- chiclet_differential_plot_out()[["data"]]
    hv <- input[["chicletDifferentialPlot_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         x_plot = plot_data[[ hv[["mapping"]][["x"]] ]],
                         y_plot = plot_data[[ "Exposure" ]], 
                         Sequence = plot_data[["Sequence"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         ID = plot_data[["ID"]],
                         value = plot_data[["value"]],
                         err_value = plot_data[["err_value"]])
    
    tt_df <- filter(hv_dat) %>%
      filter(abs(ID - x) < 0.5) 
    
    if(nrow(tt_df) != 0) {
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      
      style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none; ",
                      tt_pos_adj, ":", tt_pos,
                      "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
      
      tmp1 <- paste0(unique(tt_df[["Sequence"]]),
                     "<br/> Position: ", unique(tt_df[["Start"]]), "-", unique(tt_df[["End"]]))
      
      tmp2 <- paste0("<br/> Exposure: ", tt_df[["y_plot"]], " min, ",
                     "Value: ", round(tt_df[["value"]], 2))
      div(
        style = style,
        p(HTML(tmp1), HTML(tmp2)  
          
        ))
      
    }
  }
})


#################################
######### DATA ##################
#################################

chiclet_differential_plot_data_out <- reactive({
  
  chiclet_diff_dataset_timepoints() %>%
    show_diff_uptake_data(theoretical = input[["chic_diff_theory"]],
                          fractional = input[["chic_diff_fractional"]]) %>%
    filter(ID >= input[["chic_diff_x_range"]][[1]] & ID <= input[["chic_diff_x_range"]][[2]]) 
  
})

##

output[["chicletDifferentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  chiclet_differential_plot_data_out() %>%
    dt_format()
  
})
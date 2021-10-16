#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "qc_time_t",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > input[["qc_time_0"]]]))
})

observe({
  
  updateSelectInput(session,
                    inputId = "qc_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
  
  updateSelectInput(session,
                    inputId = "qc_state_1",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
  
  updateSelectInput(session,
                    inputId = "qc_state_2",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[length(states_chosen_protein())])
  
})

#################################
######### DATASET ###############
#################################

quality_control_dat <- reactive({
  
  qc_dat <- dat() %>%
    filter(Exposure < 99999)
  
  validate(need(as.numeric(input[["qc_time_t"]]) > as.numeric(input[["qc_time_0"]]), "Chosen time must be bigger than in time. "))
  validate(need(sum(unique(qc_dat[["Exposure"]]) > as.numeric(input[["qc_time_t"]])) > 1, "Not enough time points (bigger than chosen time) to generate a plot. "))
  validate(need(input[["qc_state_1"]]!=input[["qc_state_2"]], "The states must be different."))
  
  result <- create_quality_control_dataset(dat = qc_dat,
                                           state_1 = input[["qc_state_1"]],
                                           state_2 = input[["qc_state_2"]],
                                           time_t = as.numeric(input[["qc_time_t"]]),
                                           time_0 = as.numeric(input[["qc_time_0"]]),
                                           protein = input[["chosen_protein"]],
                                           deut_part = 0.01*as.integer(input[["deut_part"]]))
  
})

#################################
######### PLOT ##################
#################################

qc_out <- reactive({
  
  plot_quality_control(qc_dat = quality_control_dat())
  
})

output[["quality_control_plot"]] <- renderPlot({
  
  qc_out()
  
})

##

output[["quality_control_plot_debug"]] <- renderUI({
  
  if(!is.null(input[["quality_control_plot_hover"]])) {
    
    plot_data <- qc_out()[["data"]]
    hv <- input[["quality_control_plot_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]])
    
    tt_df <- hv_dat %>%
      filter(abs(y_plot - y) == min(abs(y_plot - y)), abs(x_plot - x) < 0.1*x_plot)
    
    if(nrow(tt_df) != 0) {
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      
      style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); ",
                      tt_pos_adj, ":", tt_pos,
                      "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
      
      div(
        style = style,
        p(HTML(paste0("<br/> x: ", round(tt_df[["x_plot"]], 0), " [min]",
                      "<br/> y: ", round(tt_df[["y_plot"]], 2), " [%] ")))
      )
    }
  }
})

##

##

output[["quality_control_plot_download_button"]] <- downloadHandler("qualityControlPlot.svg",
                                                                    content = function(file){
                                                                      ggsave(file, qc_out(), device = svg,
                                                                             height = 300, width = 400, units = "mm")
                                                                    })


#################################
######### DATA ##################
#################################

quality_control_plot_data_out <- reactive({
  
  show_quality_control_data(qc_dat = quality_control_dat()) 
  
})

##

output[["quality_control_plot_data"]] <- DT::renderDataTable({
  
  quality_control_plot_data_out() %>%
    dt_format()
  
})
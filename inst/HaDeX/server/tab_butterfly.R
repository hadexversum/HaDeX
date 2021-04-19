#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "butt_state",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  if(input[["butt_fractional"]]){
    
    times_t <- times_from_file()[times_from_file() > input[["butt_time_0"]] & times_from_file() < as.numeric(input[["butt_time_100"]])]
    
  } else {
    
    times_t <- times_from_file()[times_from_file() > input[["butt_time_0"]] & times_from_file() < 99999]
    
  }
  
  updateCheckboxGroupInput(session,
                           inputId = "butt_timepoints",
                           choices = times_t,
                           selected = times_t)
  
})

##

observe({
  
  updateSelectInput(session, 
                    inputId = "butt_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
  
  updateSelectInput(session, 
                    inputId = "butt_time_100",
                    choices = times_from_file(),
                    selected = max(times_from_file()[times_from_file() < 99999]))
})

##

observe({
  
  
  if(input[["butt_fractional"]]){
    
    max_y <- ceiling(max(butterfly_dataset()[["frac_deut_uptake"]], butterfly_dataset()[["theo_frac_deut_uptake"]])) + 1
    min_y <- floor(min(butterfly_dataset()[["frac_deut_uptake"]], butterfly_dataset()[["theo_frac_deut_uptake"]])) - 1
    
  } else {
    
    max_y <- ceiling(max(butterfly_dataset()[["deut_uptake"]], butterfly_dataset()[["theo_deut_uptake"]])) + 1
    min_y <- floor(min(butterfly_dataset()[["deut_uptake"]], butterfly_dataset()[["theo_deut_uptake"]])) - 1
  }
  
  max_x <- max(butterfly_dataset()[["ID"]])
  min_x <- min(butterfly_dataset()[["ID"]])
  
  updateSliderInput(session,
                    inputId = "butt_x_range",
                    min = min_x,
                    max = max_x,
                    value = c(min_x, max_x))
  
  updateSliderInput(session,
                    inputId = "butt_y_range",
                    min = min_y,
                    max = max_y,
                    value = c(min_y, max_y))
})

##

observe({
  
  updateTextInput(session,
                  inputId = "butterfly_plot_title",
                  value = case_when(
                    input[["butt_diff_theory"]] ~ paste0("Thereotical butterfly plot for ", input[["butt_state"]], " state"),
                    !input[["butt_diff_theory"]] ~ paste0("Butterfly plot for ", input[["butt_state"]])
                  ))
  
  updateTextInput(session,
                  inputId = "butterfly_y_label",
                  value = case_when(
                    input[["butt_fractional"]] ~ "Fractional deuterium uptake [%]",
                    !input[["butt_fractional"]] ~ "Deuterium uptake [Da]"
                  ))
})


#################################
######### DATASET ###############
#################################

butterfly_dataset <- reactive({
  
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  
  generate_butterfly_dataset(dat(),
                             protein = input[["chosen_protein"]],
                             state = input[["butt_state"]],
                             time_0 = as.numeric(input[["butt_time_0"]]),
                             time_100 = as.numeric(input[["butt_time_100"]]),
                             deut_part = input[["deut_part"]])
  
})

#################################
######### PLOT ##################
#################################

butterfly_plot_out <- reactive({
  
  butterfly_dataset() %>%
    filter(Exposure %in% input[["butt_timepoints"]]) %>%
    generate_butterfly_plot(theoretical = input[["butt_theory"]],
                            fractional = input[["butt_fractional"]],
                            uncertainty_type = input[["butt_uncertainty"]]) + 
    coord_cartesian(xlim = c(input[["butt_x_range"]][[1]], input[["butt_x_range"]][[2]]),
                    ylim = c(input[["butt_y_range"]][[1]], input[["butt_y_range"]][[2]])) +
    labs(title = input[["butterfly_plot_title"]],
         x = input[["butterfly_plot_x_label"]],
         y = input[["butterfly_plot_y_label"]]) +
    theme(plot.title = element_text(size = input[["butterfly_plot_title_size"]]),
          axis.text.x = element_text(size = input[["butterfly_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["butterfly_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["butterfly_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["butterfly_plot_y_label_size"]]),
          legend.text = element_text(size = input[["butterfly_plot_x_label_size"]]),
          legend.title = element_text(size = input[["butterfly_plot_x_label_size"]]))
  
  
})

##

output[["butterflyPlot"]] <- renderPlot({
  
  butterfly_plot_out()
  
})

##

output[["butterflyPlot_debug"]] <- renderUI({
  
  ## update
  
  if(!is.null(input[["butterflyPlot_hover"]])) {
    
    plot_data <- buttp_out()[["data"]]
    hv <- input[["butterflyPlot_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[[hv[["mapping"]][["x"]]]],
                         End = plot_data[["End"]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         State = plot_data[["State"]])
    
    tt_df <- filter(hv_dat, Start < x, End > x) %>%
      filter(abs(y_plot - y) < 10) %>%
      filter(abs(y_plot - y) == min(abs(y_plot - y)))
    
    
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
        p(HTML(paste0(tt_df[["Sequence"]],
                      "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
                      "<br/> Value: ", round(tt_df[["y_plot"]], 2),
                      "<br/> State: ", tt_df[["State"]])))
      )
    }
  }
})

#################################
######### DATA ##################
#################################

butterfly_plot_data_out <- reactive({
  
  butterfly_dataset() %>%
    generate_butterfly_data(theoretical = input[["butt_theory"]],
                            fractional = input[["butt_fractional"]]) %>%
    filter(Exposure %in% input[["butt_timepoints"]]) %>%
    filter(ID >= input[["butt_x_range"]][[1]] & ID <= input[["butt_x_range"]][[2]]) 
  
})

##

output[["butterflyPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  butterfly_plot_data_out() %>%
    dt_format()
  
})
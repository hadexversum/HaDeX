#################################
######### SETTINGS ##############
#################################

observeEvent(input[["chosen_protein"]], { 
  
  updateSelectInput(session, 
                    inputId = "vol_state_1",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
  
  updateSelectInput(session, 
                    inputId = "vol_state_2",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[length(states_chosen_protein())])
  
})

observe({
  
  updateSliderInput(session,
                    inputId = "vol_sequence_range",
                    max = max_range(),
                    value = c(1, max_range())
                    
  )
  
  updateTextInput(session, 
                  inputId = "volcano_plot_title",
                  value = paste0("Deuterium uptake difference between ", input[["vol_state_1"]], " and ", input[["vol_state_2"]])
  )
  
  updateCheckboxGroupInput(session,
                           inputId = "vol_timepoints",
                           choices = times_t(),
                           selected = times_t()
  )
  
})

##

observe({
  
  if(input[["vol_fractional"]]){
    
    # if(input[["vol_theoretical"]]){ value <- "diff_theo_frac_deut_uptake" } 
   value <- "diff_frac_deut_uptake" 
    
  } else {
    
    # if(input[["vol_theoretical"]]){ value <- "diff_theo_deut_uptake" }
    value <- "diff_deut_uptake"
  }
  
  max_x <- ceiling(max(abs(volcano_dataset()[[value]]), na.rm = TRUE)) ## TODO
  
  updateSliderInput(session,
                    inputId = "vol_x_range",
                    max = max_x + 2,
                    min = -max_x - 2,
                    value = c(-max_x, max_x))
  
  max_y <- ceiling(max(volcano_dataset()[["log_p_value"]], na.rm = TRUE)) 
  
  updateSliderInput(session,
                    inputId = "vol_y_range",
                    max = max_y + 2,
                    value = c(0, max_y))
  
})

observe({
  
  updateTextInput(session,
                  inputId = "volcano_plot_x_label",
                  value = case_when(
                    input[["vol_fractional"]] ~ "Fractional deuterium uptake difference [%]",
                    !input[["vol_fractional"]] ~ "Deuterium uptake difference [Da]"
                  ))
  
})



#################################
######### DATASET ###############
#################################

volcano_dataset <- reactive({
  
  # browser()
  
  validate(need(input[["vol_state_1"]]!=input[["vol_state_2"]], "There is no difference between the same state, choose different second state."))
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  validate(need(input[["vol_state_1"]] %in% states_chosen_protein(), "Wait for the parameters to be loaded."))

  dat() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    create_p_diff_uptake_dataset(state_1 = input[["vol_state_1"]],
                                 state_2 = input[["vol_state_2"]],
                                 p_adjustment_method = input[["vol_p_adjustment_method"]],
                                 confidence_level = as.numeric(input[["vol_confidence_level"]]))
  
  
})

##

volcano_data <- reactive({
  
  volcano_dataset() %>%
    filter(Exposure %in% input[["vol_timepoints"]]) %>%
    filter(Start >= input[["vol_sequence_range"]][1], End <= input[["vol_sequence_range"]][2])
  
})

#################################
######### PLOT ##################
#################################

all_timepoints_data <- reactive({
  
  lapply(times_t(), function(t){
    
    calculate_diff_uptake(dat(), 
                          states = c(input[["vol_state_1"]], input[["vol_state_2"]]),
                          protein = input[["chosen_protein"]],
                          time_0 = times_from_file()[1],
                          time_t = t,
                          time_100 = times_from_file()[length(times_from_file())],
                          deut_part = as.numeric(input[["deut_part"]])/100) %>%
      mutate(Exposure = t)
    
  }) %>% bind_rows()
  
})

##

chosen_timepoints_data <- reactive({
  
  if(input[["vol_interval"]] == "Selected time points"){
    
    all_timepoints_data() %>%
      filter(Exposure %in% as.numeric(input[["vol_timepoints"]]))
    
  } else {
    
    all_timepoints_data()
    
  }
  
  
})

##

houde_intervals <- reactive({
  
  chosen_timepoints_data() %>%
    calculate_confidence_limit_values(confidence_level = as.numeric(input[["vol_confidence_level"]]),
                                      theoretical = FALSE,
                                      fractional = input[["vol_fractional"]])
  
})

##

alpha_interval <- reactive({
  
  -log(1 - as.numeric(input[["vol_confidence_level"]]))
  
})

##

volcano_plot_out <- reactive({
  
  plot_volcano(volcano_data(), 
               state_1 = input[["vol_state_1"]], 
               state_2 = input[["vol_state_2"]],
               color_times = input[["vol_color_times"]],
               fractional = input[["vol_fractional"]],
               theoretical = FALSE) + ## hard coded, no theoretical
    # ## statistics
    geom_segment(aes(x = houde_intervals()[1], xend = houde_intervals()[1], y = alpha_interval(), yend = input[["vol_y_range"]][2]), linetype = "dashed", color = "red") +
    geom_segment(aes(x = houde_intervals()[2], xend = houde_intervals()[2], y = alpha_interval(), yend = input[["vol_y_range"]][2]), linetype = "dashed", color = "red") +
    geom_segment(aes(y = alpha_interval(), yend = alpha_interval(), x = input[["vol_x_range"]][1], xend = houde_intervals()[1]), linetype = "dashed", color = "red") +
    geom_segment(aes(y = alpha_interval(), yend = alpha_interval(), x = houde_intervals()[2], xend = input[["vol_x_range"]][2]), linetype = "dashed", color = "red") +
    ## visualization
    labs(title = input[["volcano_plot_title"]],
         x = input[["volcano_plot_x_label"]],
         y = input[["volcano_plot_y_label"]],
         caption = paste0("CI ", input[["vol_confidence_level"]], "%: ", round(houde_intervals()[2], 4))) +
    coord_cartesian(xlim = c(input[["vol_x_range"]][[1]], input[["vol_x_range"]][[2]]),
                    ylim = c(input[["vol_y_range"]][[1]], input[["vol_y_range"]][[2]]),
                    expand = FALSE) +
    theme(plot.title = element_text(size = input[["volcano_plot_title_size"]]),
          axis.text.x = element_text(size = input[["volcano_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["volcano_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["volcano_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["volcano_plot_y_label_size"]]),
          legend.text = element_text(size = input[["volcano_plot_x_label_size"]]),
          legend.title = element_text(size = input[["volcano_plot_x_label_size"]]))
  
})

##

output[["volcanoPlot"]] <- renderPlot({
  
  volcano_plot_out() 
  
})

##

output[["vol_thresholds"]] <- renderText({
  
  if(input[["vol_fractional"]]){ ci_unit <-  " [%]" } else { ci_unit <- " [Da]" }
  
  paste0("Based on the chosen criteria, the threshold of -log(P value) is ", round(alpha_interval(), 4), " and threshold on deuterium uptake difference is ", round(houde_intervals()[1], 4), " and ", round(houde_intervals()[2], 4), ci_unit, ".") 
  
})

##

output[["volcanoPlot_download_button"]] <- downloadHandler("volcanoPlot.svg",
                                                           content = function(file) {
                                                             ggsave(file, volcano_plot_out(), device = svg,
                                                                    height = 300, width = 400, units = "mm")
                                                           })

##

output[["volcanoPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["volcanoPlot_hover"]])) {
    
    # browser()
    
    plot_data <- volcano_plot_out()[["data"]]
    hv <- input[["volcanoPlot_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         Exposure = plot_data[["Exposure"]],
                         P_value = plot_data[["P_value"]])
    
    tt_df <- filter(hv_dat) %>%
      filter(abs(x_plot - x) < 0.1) %>%
      filter(abs(y_plot - y) < 0.5) %>%
      filter(abs(y_plot - y) == min(abs(y_plot - y)))
    
    
    if(nrow(tt_df) != 0) {
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      
      style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                      tt_pos_adj, ":", tt_pos, "px; padding: 0px;",
                      "top:", hv[["coords_css"]][["y"]] , "px; ")
      
      div(
        style = style,
        p(HTML(paste0(tt_df[["Sequence"]],
                      "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
                      "<br/> Exposure: ", tt_df[["Exposure"]], " min",
                      "<br/> Difference: ", round(tt_df[["x_plot"]], 2),
                      "<br/> P value: ", round(tt_df[["P_value"]], 4),
                      "<br/> -log(P value): ", round(tt_df[["y_plot"]], 2)
        )))
      )
    }
  }
})

#################################
######### DATA ##################
#################################

volcano_plot_data_out <- reactive({
  
  show_volcano_data(volcano_data(),
                    D_diff_threshold = houde_intervals()[2],
                    log_P_threshold = alpha_interval(),
                    confidence_level = input[["vol_confidence_level"]], 
                    fractional = input[["vol_fractional"]])
  
})

##

output[["volcanoPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  volcano_plot_data_out() %>%
    dt_format()
  
})
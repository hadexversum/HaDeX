#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "butt_diff_state_1",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  updateSelectInput(session,
                    inputId = "butt_diff_state_2",
                    choices = states_from_file(),
                    selected = states_from_file()[2])
  
  if(input[["butt_diff_fractional"]]){
    
    times_t <- times_from_file()[times_from_file() > as.numeric(input[["butt_diff_time_0"]]) & times_from_file() < as.numeric(input[["butt_diff_time_100"]])]
    
  } else {
    
    times_t <- times_from_file()[times_from_file() > as.numeric(input[["butt_diff_time_0"]]) & times_from_file() < 99999]
    
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
                    choices = times_with_control(),
                    selected = max(times_with_control()[times_with_control() < 99999]))
})

##

observe({
  
  updateTextInput(session,
                  inputId = "butterflyDifferential_plot_title",
                  value = case_when(
                    input[["butt_diff_theory"]] ~ paste0("Thereotical butterfly differential plot between ", input[["butt_diff_state_1"]], " and ", input[["butt_diff_state_2"]]),
                    !input[["butt_diff_theory"]] ~ paste0("Butterfly differential plot between ", input[["butt_diff_state_1"]], " and ", input[["butt_diff_state_2"]])
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
    
    max_y <- ceiling(max(butt_diff_dataset()[["diff_frac_deut_uptake"]], butt_diff_dataset()[["diff_theo_frac_deut_uptake"]], na.rm = TRUE)) + 1
    min_y <- floor(min(butt_diff_dataset()[["diff_frac_deut_uptake"]], butt_diff_dataset()[["diff_theo_frac_deut_uptake"]], na.rm = TRUE)) - 1
    
  } else {
    
    max_y <- ceiling(max(butt_diff_dataset()[["diff_deut_uptake"]], butt_diff_dataset()[["diff_theo_deut_uptake"]], na.rm = TRUE)) + 1
    min_y <- floor(min(butt_diff_dataset()[["diff_deut_uptake"]], butt_diff_dataset()[["diff_theo_deut_uptake"]], na.rm = TRUE)) - 1
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
    min = min_y - 5 ,
    max = max_y + 5,
    value = c(min_y, max_y)
  )
  
})

##

observe({
  
  if(input[["butt_diff_theory"]]){
    hide(id = "butt_diff_time_0_part")
    hide(id = "butt_diff_time_100_part")
  }
  
})

##

observe({
  
  if(!input[["butt_diff_theory"]]){
    show(id = "butt_diff_time_0_part")
    show(id = "butt_diff_time_100_part")
  }
  
})

##

observe({
  
  if(!input[["butt_diff_fractional"]]){
    hide(id = "butt_diff_time_100_part")
  }
  
})

##

observe({
  
  if(input[["butt_diff_fractional"]] & !input[["butt_diff_theory"]]){
    show(id = "butt_diff_time_100_part")
  }
  
})

##

observe({
  
  if(input[["butt_diff_show_test"]]){
    show(id = "butt_diff_confidence_level_part")
  }
  
})

##

observe({
  
  if(!input[["butt_diff_show_test"]]){
    hide(id = "butt_diff_confidence_level_part")
  }
  
})

#################################
######### DATASET ###############
#################################

butt_diff_dataset <- reactive({
  
  validate(need(input[["butt_diff_state_1"]]!=input[["butt_diff_state_2"]], "There is no difference between the same state, choose different state 2."))
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  
  create_diff_uptake_dataset(dat(),
                            protein = input[["chosen_protein"]],
                            state_1 = input[["butt_diff_state_1"]],
                            state_2 = input[["butt_diff_state_2"]],
                            time_0 = as.numeric(input[["butt_diff_time_0"]]),
                            time_100 = as.numeric(input[["butt_diff_time_100"]]),
                            deut_part = as.numeric(input[["deut_part"]])/100)
})

##

butt_diff_dat <- reactive({
  
  validate(need(input[["butt_diff_timepoints"]], "Select time points on the left to see the data."))
  
  butt_diff_dataset() %>%
    filter(Exposure %in% input[["butt_diff_timepoints"]])
  
})

#################################
######### PLOT ##################
#################################

butterfly_differential_plot <- reactive({
  
  # browser()
  
  plot_differential_butterfly(butt_diff_dat(),
                              theoretical = input[["butt_diff_theory"]],
                              fractional = input[["butt_diff_fractional"]],
                              uncertainty_type = input[["butt_diff_uncertainty"]],
                              show_confidence_limit = input[["butt_diff_show_test"]],
                              confidence_level = as.numeric(input[["butt_diff_confidence_level"]]))
})

##

butterfly_differential_plot_out <- reactive({
  
  butterfly_differential_plot() + 
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

##

output[["butterflyDifferentialPlot_download_button"]] <- downloadHandler("butterflyDifferentialPlot.svg",
                                                             content = function(file) {
                                                               ggsave(file, butterfly_differential_plot_out(), device = svg,
                                                                      height = 300, width = 400, units = "mm")
                                                             })
##

output[["butterflyDifferentialPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["butterflyDifferentialPlot_hover"]])) {
    
    plot_data <- butterfly_differential_plot_out()[["data"]]
    hv <- input[["butterflyDifferentialPlot_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         Exposure = plot_data[["Exposure"]],
                         ID = plot_data[["ID"]])
    
    tt_df <- filter(hv_dat) %>%
      filter(abs(ID - x) < 0.5) %>%
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
                      "<br/> Exposure: ", tt_df[["Exposure"]], " min"
        )))
      )
    }
  }
})

#################################
######### DATA ##################
#################################

output[["butterflyDifferentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  show_diff_uptake_data(butt_diff_dat(),
                        theoretical = input[["butt_diff_theory"]],
                        fractional = input[["butt_diff_fractional"]]) %>%
    filter(ID >= input[["butt_diff_x_range"]][[1]] & ID <= input[["butt_diff_x_range"]][[2]]) %>%
    dt_format()
  
})



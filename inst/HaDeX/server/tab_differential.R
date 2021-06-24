#################################
######### SETTINGS ##############
#################################

observe({
  
  if (input[["comp_fractional"]]) {
    
    updateSliderInput(session,
                      inputId = "woods_plot_y_range",
                      min = -200,
                      max = 200,
                      value = c(-50, 50),
                      step = 10)
    
  } else {
    
    min_woods_abs <- round_any(min(woods_plot_dat()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 2, floor)
    max_woods_abs <- round_any(max(woods_plot_dat()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 2, ceiling)
    
    updateSliderInput(session,
                      inputId = "woods_plot_y_range",
                      min = min_woods_abs - 2,
                      max = max_woods_abs + 2,
                      value = c(min_woods_abs, max_woods_abs),
                      step = 0.5)
    
  }
  
})

##

observe({
  
  updateTextInput(session,
                  inputId = "woods_plot_title",
                  value = case_when(
                    input[["theory"]] ~ paste0("Theoretical deuterium uptake difference in ", input[["time_t"]], " min between ", gsub("_", " ", input[["diff_state_1"]]), " and ", gsub("_", " ", input[["diff_state_2"]]), " for ", input[["chosen_protein"]]),
                    !input[["theory"]] ~ paste0("Deuterium uptake difference in ", input[["time_t"]], " min between ", gsub("_", " ", input[["diff_state_1"]]), " and ", gsub("_", " ", input[["diff_state_2"]]), " for ", input[["chosen_protein"]])
                  ))
  
  updateTextInput(session,
                  inputId = "woods_plot_y_label",
                  value = case_when(
                    input[["comp_fractional"]] ~ "Fractional deuterium uptake difference [%]",
                    !input[["comp_fractional"]] ~ "Deuterium uptake difference [Da]"
                  ))
})

##

observe({
  
  
  updateSelectInput(session,
                    inputId = "diff_state_1",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  updateSelectInput(session,
                    inputId = "diff_state_2",
                    choices = states_from_file(),
                    selected = states_from_file()[length(states_from_file())])
  
  
  
  updateSelectInput(session,
                    inputId = "confidence_limit_2",
                    choices = confidence_limit_choices[confidence_limit_choices >= input[["confidence_limit"]]],
                    selected = confidence_limit_choices[confidence_limit_choices > input[["confidence_limit"]]][1])
  
  
  
})

#################################
######### DATASET ###############
#################################

woods_plot_dat <- reactive({
  
  # browser()
  
  validate(need(input[["diff_state_1"]]!=input[["diff_state_2"]], "There is no difference between the same state, choose different state 2."))
  validate(need(length(unique(filter(dat(), !is.na("Modification"), Protein == input[["chosen_protein"]])[["State"]])) > 1, "Not sufficient number of states without modifications."))
  
  if(!input[["theory"]]){
    validate(need(as.numeric(input[["time_0"]]) < as.numeric(input[["time_t"]]), "In time must be smaller than chosen time."))
    validate(need(as.numeric(input[["time_t"]]) < as.numeric(input[["time_100"]]), "Out time must be bigger than chosen time."))
  }
  
  tryCatch({
    calculate_diff_uptake(dat = dat(),
                          states = c(input[["diff_state_1"]], input[["diff_state_2"]]),
                          protein = input[["chosen_protein"]],
                          time_0 = input[["time_0"]],
                          time_t = input[["time_t"]],
                          time_100 = input[["time_100"]],
                          deut_part = 0.01*as.integer(input[["deut_part"]]))
  },
  error = function(e){
    validate(need(FALSE), "Check chosen parameters - not sufficient data.")
  })
  
})

#################################
######### PLOT ##################
#################################

differential_plot <- reactive({
  
  plot_differential(dat = woods_plot_dat(),
                    theoretical = input[["theory"]],
                    fractional = input[["comp_fractional"]],
                    confidence_limit = as.double(input[["confidence_limit"]]),
                    confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
})

##

differential_plot_out <- reactive({
  
  validate(need(input[["diff_state_1"]]!=input[["diff_state_2"]], "There is no difference between the same state, choose different state 2."))
  
  differential_plot() + 
    coord_cartesian(xlim = c(input[["plot_x_range"]][[1]], input[["plot_x_range"]][[2]]),
                    ylim = c(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]])) +
    labs(title = input[["woods_plot_title"]],
         x = input[["woods_plot_x_label"]],
         y = input[["woods_plot_y_label"]]) + 
    theme(plot.title = element_text(size = input[["woods_plot_title_size"]]),
          axis.text.x = element_text(size = input[["woods_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["woods_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["woods_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["woods_plot_y_label_size"]]),
          legend.text = element_text(size = input[["woods_plot_x_label_size"]]))
  
})

##

output[["differentialPlot"]] <- renderPlot({
  
  differential_plot_out()
  
})

##

output[["differentialPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["differentialPlot_hover"]])) {
    
    wp_plot_data <- differential_plot_out()[["data"]]
    wp_hv <- input[["differentialPlot_hover"]]
    
    wp_hv_dat <- data.frame(x = wp_hv[["x"]],
                            y = wp_hv[["y"]],
                            Start = wp_plot_data[[wp_hv[["mapping"]][["x"]]]],
                            End = wp_plot_data[["End"]],
                            y_plot = wp_plot_data[[wp_hv[["mapping"]][["y"]]]],
                            Sequence = wp_plot_data[["Sequence"]])
    
    wp_tt_df <- filter(wp_hv_dat, Start < x, End > x) %>%
      filter(abs(y_plot - y) < 10) %>%
      filter(abs(y_plot - y) == min(abs(y_plot - y)))
    
    if(nrow(wp_tt_df) != 0) {
      
      wp_tt_pos_adj <- ifelse(wp_hv[["coords_img"]][["x"]]/wp_hv[["range"]][["right"]] < 0.5,
                              "left", "right")
      
      wp_tt_pos <- ifelse(wp_hv[["coords_img"]][["x"]]/wp_hv[["range"]][["right"]] < 0.5,
                          wp_hv[["coords_css"]][["x"]],
                          wp_hv[["range"]][["right"]]/wp_hv[["img_css_ratio"]][["x"]] - wp_hv[["coords_css"]][["x"]])
      
      
      style <- paste0("position:absolute; z-index:1072; background-color: rgba(245, 245, 245, 1); pointer-events: none; ",
                      wp_tt_pos_adj, ":", wp_tt_pos, "px; padding: 0px;",
                      "top:", wp_hv[["coords_css"]][["y"]] , "px; ")
      
      wellPanel(
        style = style,
        p(HTML(paste0(wp_tt_df[["Sequence"]],
                      "<br/> Position: ", wp_tt_df[["Start"]], "-", wp_tt_df[["End"]],
                      "<br/> Value: ", round(wp_tt_df[["y_plot"]], 2))))
      )
      
    }
  }
})

##

output[["differentialPlot_download_button"]] <- downloadHandler("differentialPlot.svg",
                                                                content = function(file) {
                                                                  ggsave(file, differential_plot_out(), device = svg,
                                                                         height = 300, width = 400, units = "mm")
                                                                })

#################################
######### DATA ##################
#################################

differential_plot_data <- reactive({
  
  show_diff_uptake_data_confidence(dat = woods_plot_dat(),
                                   theoretical = input[["theory"]],
                                   fractional = input[["comp_fractional"]],
                                   confidence_limit_1 = as.double(input[["confidence_limit"]]),
                                   confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
})

##

output[["differentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  differential_plot_data() %>%
    filter(Protein == input[["chosen_protein"]],
           Start >= input[["plot_x_range"]][[1]],
           End <= input[["plot_x_range"]][[2]]) %>%
    dt_format()
  
})
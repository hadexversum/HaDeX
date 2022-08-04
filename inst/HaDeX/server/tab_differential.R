#################################
######### MESSAGE ###############
#################################

hdx_message <- reactive({
  
  message <- list(results = woods_test_results(), 
                  protein = input[["chosen_protein"]],
                  chain = input[["diff_viewer_chain"]],
                  type = input[["diff_viewer_datatype"]])
  
  jsonlite::toJSON(message)
  
})


observeEvent(input[["diff_open_viewer"]],{
  
  print("Request to contect HDX Viewer...")
  
  session$sendCustomMessage("hdx_handler", hdx_message())
  
  print("Message sent.")
  
}) 


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

})

##

observe({
  
  if(input[["time_t"]] == -1){
    
    updateTextInput(session,
                    inputId = "woods_plot_title",
                    value = case_when(
                      input[["theory"]] ~ paste0("Theoretical deuterium uptake difference between ", gsub("_", " ", input[["diff_state_1"]]), " and ", gsub("_", " ", input[["diff_state_2"]]), " for ", input[["chosen_protein"]]),
                      !input[["theory"]] ~ paste0("Deuterium uptake difference between ", gsub("_", " ", input[["diff_state_1"]]), " and ", gsub("_", " ", input[["diff_state_2"]]), " for ", input[["chosen_protein"]])
                    ))
    
  }
  
})

##

observe({
  
  if(input[["diff_viewer_settings"]][1]%%2 == 0) { shinyjs::hide("diff_viewer_part") }
  
})

observe({

  if(input[["diff_viewer_settings"]][1]%%2 != 0) { shinyjs::show("diff_viewer_part") }
  
})

##

observe({
  
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
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "diff_state_2",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[2])
})

##

observe({
  
  if(input[["diff_show_tstud"]]){ show(id = "diff_correction_part")  }
  
})

##

observe({
  
  if(!input[["diff_show_tstud"]]){ hide(id = "diff_correction_part")  }
  
})

#################################
######### DATASET ###############
#################################

woods_plot_dat <- reactive({
  
  validate(need(input[["diff_state_1"]]!=input[["diff_state_2"]], "There is no difference between the same state, choose different state 2."))
  validate(need(length(unique(filter(dat(), !is.na("Modification"), Protein == input[["chosen_protein"]])[["State"]])) > 1, "Not sufficient number of states without modifications."))
  validate(need(input[["diff_state_1"]] %in% states_chosen_protein(), "The first state is not compatible with chosen protein."))
  validate(need(input[["diff_state_2"]] %in% states_chosen_protein(), "The second state is not compatible with chosen protein."))
  
  if(!input[["theory"]]){
    
    if(!(input[["time_t"]] == -1)) { validate(need(as.numeric(input[["time_0"]]) < as.numeric(input[["time_t"]]), "In time must be smaller than chosen time.")) }
    validate(need(as.numeric(input[["time_t"]]) < as.numeric(input[["time_100"]]), "Out time must be bigger than chosen time."))
  }
  
  tryCatch({
    tmp_dat <- create_diff_uptake_dataset(dat = dat(),
                               protein = input[["chosen_protein"]],
                               state_1 = input[["diff_state_1"]],
                               state_2 = input[["diff_state_2"]],
                               time_0 = as.numeric(input[["time_0"]]),
                               time_100 = as.numeric(input[["time_100"]]),
                               deut_part = 0.01*as.integer(input[["deut_part"]]))
  },
  error = function(e){
    validate(need(FALSE), "Check chosen parameters - not sufficient data.")
  })
  
  if(input[["time_t"]] == -1) {
    
    tmp_dat %>%
      filter(Exposure %in% as.numeric(input[["diff_comp_times_t"]]))
    
  } else {
    tmp_dat %>%
      filter(Exposure == input[["time_t"]])
  }
  
})

##

woods_p_dat <- reactive({
  
  create_p_diff_uptake_dataset(dat = dat(),
                               diff_uptake_dat = woods_plot_dat(),
                               protein = input[["chosen_protein"]],
                               state_1 = input[["diff_state_1"]], 
                               state_2 = input[["diff_state_2"]],
                               p_adjustment_method = input[["diff_p_adjustment_method"]],
                               confidence_level = as.double(input[["confidence_level"]]),
                               time_0 = as.numeric(input[["time_0"]]),
                               time_100 = as.numeric(input[["time_100"]]),
                               deut_part = 0.01*as.integer(input[["deut_part"]])
                               )
  
})

#################################
######### HDX Viewer ############
#################################

woods_test_results <- reactive({
  
  # browser()
  
  calculate_aggregated_test_results(create_p_diff_uptake_dataset_with_confidence(p_diff_uptake_dat = woods_p_dat(),
                                               theoretical = input[["theory"]],
                                               fractional = input[["comp_fractional"]]),
                                 time_t = as.numeric(input[["time_t"]]))
  
})

output[["diff_viewer_download_button"]] <- downloadHandler(paste0(input[["chosen_protein"]], "-", input[["diff_viewer_chain"]], ".csv"),
                                                           content = function(file){ 
                                                             write.csv(woods_test_results(), 
                                                                       file = file,
                                                                       quote = F,
                                                                       row.names = F)
                                                             } )


#################################
######### PLOT ##################
#################################

differential_plot <- reactive({
  
  plot_differential(diff_uptake_dat = woods_plot_dat(),
                    diff_p_uptake_dat = woods_p_dat(),
                    theoretical = input[["theory"]],
                    fractional = input[["comp_fractional"]],
                    show_houde_interval = input[["diff_show_houde"]],
                    hide_houde_insignificant = input[["diff_hide_insignificant"]] & input[["diff_show_houde"]] ,
                    show_tstud_confidence = input[["diff_show_tstud"]],
                    hide_tstud_insignificant = input[["diff_hide_insignificant"]] & input[["diff_show_tstud"]],
                    time_t = as.numeric(input[["time_t"]]),
                    line_size = 1,
                    confidence_level = as.double(input[["confidence_level"]]),
                    all_times = (input[["time_t"]] == -1))
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
  
  # browser()

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
  
  show_diff_uptake_data_confidence(diff_uptake_dat = woods_plot_dat(),
                                   theoretical = input[["theory"]],
                                   fractional = input[["comp_fractional"]],
                                   confidence_level = as.double(input[["confidence_level"]]))
})

##

output[["differentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  differential_plot_data() %>%
    filter(Protein == input[["chosen_protein"]],
           Start >= input[["plot_x_range"]][[1]],
           End <= input[["plot_x_range"]][[2]]) %>%
    dt_format()
  
})
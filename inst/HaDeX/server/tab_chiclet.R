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
                    input[["chic_theory"]] ~ paste0("Theoreotical chiclet plot for ", input[["chic_state"]], " state for ", input[["chosen_protein"]]),
                    !input[["chic_theory"]] ~ paste0("Chiclet plot for ", input[["chic_state"]], " state for ", input[["chosen_protein"]])
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
  
  create_state_uptake_dataset(dat(),
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

#################################
######### PLOT ##################
#################################

chiclet_plot_out <- reactive({
  
  plot_chiclet(chiclet_dataset_timepoints(),
               theoretical = input[["chic_theory"]],
               fractional = input[["chic_fractional"]],
               show_uncertainty = input[["chic_show_uncertainty"]]) +
    coord_cartesian(xlim = c(input[["chic_x_range"]][[1]], input[["chic_x_range"]][[2]])) +
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

##

output[["chicletPlot_download_button"]] <- downloadHandler("chicletPlot.svg",
                                                           content = function(file) {
                                                               ggsave(file, chiclet_plot_out(), device = svg,
                                                                      height = 300, width = 400, units = "mm")
})

##

output[["chicletPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["chicletPlot_hover"]])) {
    
    ## TO FIX
    
    plot_data <- chiclet_plot_out()[["data"]]
    hv <- input[["chicletPlot_hover"]]
    
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

chiclet_plot_data_out <- reactive({
  
  chiclet_dataset_timepoints() %>%
    show_uptake_data(theoretical = input[["chic_theory"]],
                     fractional = input[["chic_fractional"]]) %>%
    filter(ID >= input[["chic_x_range"]][[1]] & ID <= input[["chic_x_range"]][[2]]) 
    
})

##

output[["chicletPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  chiclet_plot_data_out() %>%
    dt_format()
  
})
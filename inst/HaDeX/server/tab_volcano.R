#################################
######### SETTINGS ##############
#################################

observe({ 
  
  updateSelectInput(session, 
                    inputId = "vol_state_first",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
  
  updateSelectInput(session, 
                    inputId = "vol_state_second",
                    choices = states_from_file(),
                    selected = states_from_file()[2])
  
})

observe({
  
  updateTextInput(session, 
                  inputId = "volcano_plot_title",
                  value = paste0("Deuterium uptake difference between ", input[["vol_state_first"]], " and ", input[["vol_state_second"]]))
  
  times_t <- times_from_file()[times_from_file() > 0 & times_from_file()<99999]
  
  updateCheckboxGroupInput(session,
                           inputId = "vol_timepoints",
                           choices = times_t,
                           selected = times_t)
})

##

observe({
  
  times_t <- times_from_file()[times_from_file() > 0]
  intervals_t <- setNames(times_t, c(head(times_t, -1), "All time points"))
  
  updateSelectInput(session,
                    inputId = "vol_interval",
                    choices = intervals_t,
                    selected = 99999)
  
})

##

observe({
  
  max_x <- ceiling(max(abs(volcano_dataset()[["D_diff"]])))
  
  updateSliderInput(session,
                    inputId = "vol_x_range",
                    max = max_x,
                    min = -max_x,
                    value = c(-max_x, max_x))
  
  max_y <- ceiling(max(volcano_dataset()[["log_p_value"]]))
  
  updateSliderInput(session,
                    inputId = "vol_y_range",
                    max = max_y,
                    value = c(0, max_y))
  
})

#################################
######### DATASET ###############
#################################

volcano_dataset <- reactive({
  
  validate(need(input[["chosen_protein"]] %in% unique(dat()[["Protein"]]), "Wait for the parameters to be loaded."))
  
  dat() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    generate_volcano_dataset(state_1 = input[["vol_state_first"]],
                             state_2 = input[["vol_state_second"]])
  
  
})

##

volcano_data <- reactive({
  
  volcano_dataset() %>%
    filter(Exposure %in% input[["vol_timepoints"]])
  
})

#################################
######### PLOT ##################
#################################

vp_out <- reactive({
  
  generate_volcano_plot(volcano_data(), 
                        state_1 = input[["vol_state_first"]], 
                        state_2 = input[["vol_state_second"]]) +
    labs(title = input[["volcano_plot_title"]],
         x = input[["volcano_plot_x_label"]],
         y = input[["volcano_plot_y_label"]]) +
    coord_cartesian(xlim = c(input[["vol_x_range"]][[1]], input[["vol_x_range"]][[2]]),
                    ylim = c(input[["vol_y_range"]][[1]], input[["vol_y_range"]][[2]])) +
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
  
  vp_out() 
  
})

##

output[["volcanoPlot_download_button"]] <- downloadHandler("volcanoPlot.svg",
                                                           content = function(file) {
                                                             ggsave(file, vp_out(), device = svg,
                                                                    height = 300, width = 400, units = "mm")
                                                           })
#################################
######### DATA ##################
#################################

output[["volcanoPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  generate_volcano_data(volcano_data()) %>%
    dt_format()
  
})
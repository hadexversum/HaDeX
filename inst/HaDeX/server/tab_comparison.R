#################################
######### SETTINGS ##############
#################################

observe({

  ## TODO: this should be dynamic as well 
  
  if (input[["comp_fractional"]]) {

    updateSliderInput(session,
                      inputId = "comp_plot_y_range",
                      min = -200,
                      max = 200,
                      value = c(0, 120),
                      step = 10)


  }

})


##

observe({

  ## problematic but validate propagates badly in updates
  
  if(!input[["comp_fractional"]]) {

    if(as.numeric(input[["time_0"]]) < as.numeric(input[["time_t"]]) | input[["theory"]]){

      if(input[["chosen_protein"]] %in% unique(dat()[["Protein"]])){
        
        min_comparison_abs <- if (nrow(prep_dat()) > 0) round_any(min(prep_dat()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, floor)  else -1
        max_comparison_abs <- if (nrow(prep_dat()) > 0) round_any(max(prep_dat()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, ceiling) else 1
        
      } else {
        
        min_comparison_abs <- -1
        max_comparison_abs <- 1
        
      }

      
    } else {

      min_comparison_abs <- -1
      max_comparison_abs <- 1

    }

    updateSliderInput(session,
                      inputId = "comp_plot_y_range",
                      min = min_comparison_abs - 5,
                      max = max_comparison_abs + 5,
                      value = c(min_comparison_abs, max_comparison_abs),
                      step = 1)

  }

})


#

observe({

  updateTextInput(session,
                  inputId = "comparison_plot_title",
                  value = case_when(
                    input[["theory"]] ~ paste0("Theoretical deuterium uptake in ", input[["time_t"]], " min for ", input[["chosen_protein"]]),
                    !input[["theory"]]  ~ paste0("Deuterium uptake in ", input[["time_t"]], " min for ", input[["chosen_protein"]])
                  ))
})

##

observe({
  
  updateTextInput(session,
                  inputId = "comparison_plot_y_label",
                  value = case_when(
                    input[["comp_fractional"]] ~ "Fractional deuterium uptake [%]",
                    !input[["comp_fractional"]] ~ "Deuterium uptake [Da]"
                  ))

})

##

observe({

  # browser()

  tmp <- sort(unique(round(dat()[["Exposure"]], 3)))
  choose_time_100 <- setNames(tmp, c(head(tmp, -1), "chosen control"))

  if(has_modifications()){

    updateSelectInput(session,
                      inputId = "time_100",
                      choices = times_from_file()[times_from_file() < 99999],
                      selected = max(times_from_file()[times_from_file() < 99999]))

  }

  if(!has_modifications()){

    updateSelectInput(session,
                      inputId = "time_100",
                      choices = choose_time_100,
                      selected = choose_time_100["chosen control"])
  }
  
})

##

observe({

  updateSelectInput(session,
                    inputId = "time_t",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > input[["time_0"]]]))
})

##

observe({

  updateSelectInput(session,
                    inputId = "time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
})

##

observe({

  updateCheckboxGroupInput(session,
                           inputId = "compare_states",
                           choices = states_chosen_protein(),
                           selected = states_chosen_protein())
})

##

observe({

  updateSliderInput(session,
                    inputId = "plot_range",
                    max = max_range(),
                    value = c(1, max_range()))
})

##

observe({
  
  updateSliderInput(session,
                    inputId = "plot_x_range",
                    max = max_range(),
                    value = c(1, max_range()))
})

##

observe({

  if(input[["theory"]]){
    hide(id = "time_0_part")
    hide(id = "time_100_part")
  }

})

observe({

  if(!input[["theory"]]){
    show(id = "time_0_part")
    show(id = "time_100_part")
  }

})

##

observe({

  if(!input[["comp_fractional"]]){
    hide(id = "time_100_part")
  }

})

##

observe({

  if(input[["comp_fractional"]] & !input[["theory"]]){
    show(id = "time_100_part")
  }

})

##

comparison_plot_colors <- reactive({

  hcl.colors(length(states_from_file()), palette = "Set 2", alpha = NULL, rev = FALSE, fixup = TRUE)

})

##

output[["states_colors"]] <- renderUI({

  lapply(1:length(states_from_file()), function(i) {
    textInput(inputId = paste0(states_from_file()[i], "_color"),
              label = paste(states_from_file()[i], " color"),
              value = comparison_plot_colors()[i])
  })
})

##

comparison_plot_colors_chosen <- reactive({

  lapply(paste0(states_from_file(), "_color"), function(i) input[[i]])

  tmp <- t(sapply(paste0(input[["compare_states"]],"_color"), function(i) input[[i]][1], simplify = TRUE))

  tmp[tmp == "NULL"] <- NA

  if (all(is.na(tmp))) {
    comparison_plot_colors()[1:length(states_from_file())]
  } else {
    coalesce(as.vector(tmp), comparison_plot_colors()[1:length(input[["compare_states"]])])
  }

})



#################################
######### DATASET ###############
#################################

all_dat <- reactive({

  if(!input[["theory"]]){
    validate(need(as.numeric(input[["time_0"]]) < as.numeric(input[["time_t"]]), "In time must be smaller than chosen time."))
    validate(need(as.numeric(input[["time_t"]]) < as.numeric(input[["time_100"]]), "Out time must be bigger than chosen time."))
  }

  bind_rows(lapply(states_from_file(), function(i) calculate_state_uptake(dat(),
                                                                          protein = input[["chosen_protein"]],
                                                                          state = i,
                                                                          time_0 = input[["time_0"]],
                                                                          time_t = input[["time_t"]],
                                                                          time_100 = input[["time_100"]],
                                                                          deut_part = 0.01*as.integer(input[["deut_part"]]))))
})

##

prep_dat <- reactive({

  validate(need(input[["compare_states"]], "Please select at least one state."))

  filter(all_dat(), State %in% input[["compare_states"]])

})

#################################
######### PLOT ##################
#################################

comparison_plot <- reactive({
  
  plot_state_comparison(dat = prep_dat(),
                        theoretical = input[["theory"]],
                        fractional = input[["comp_fractional"]])
})


##

cp_out <- reactive({

  comparison_plot_colors_chosen()

  comparison_plot() +
    coord_cartesian(xlim = c(input[["plot_x_range"]][[1]], input[["plot_x_range"]][[2]]),
                       ylim = c(input[["comp_plot_y_range"]][[1]], input[["comp_plot_y_range"]][[2]])) +
    labs(title = input[["comparison_plot_title"]],
         x = input[["comparison_plot_x_label"]],
         y = input[["comparison_plot_y_label"]]) +
    scale_color_manual(values = comparison_plot_colors_chosen()) +
    theme(plot.title = element_text(size = input[["comparison_plot_title_size"]]),
          axis.text.x = element_text(size = input[["comparison_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["comparison_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["comparison_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["comparison_plot_y_label_size"]]),
          legend.text = element_text(size = input[["comparison_plot_x_label_size"]]),
          legend.title = element_text(size = input[["comparison_plot_x_label_size"]]))

})

##

output[["comparisonPlot"]] <- renderPlot({

  cp_out()

})

##

output[["comparisonPlot_debug"]] <- renderUI({

  if(!is.null(input[["comparisonPlot_hover"]])) {

    plot_data <- cp_out()[["data"]]
    hv <- input[["comparisonPlot_hover"]]

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

##

output[["comparisonPlot_download_button"]] <- downloadHandler("comparisonPlot.svg",
                                                              content = function(file) {
                                                                ggsave(file, cp_out(), device = svg,
                                                                       height = 300, width = 400, units = "mm")
                                                              })
#################################
######### DATA ##################
#################################

comparison_plot_data <- reactive({

  # browser()

  show_uptake_data(uptake_dat = prep_dat(),
                   theoretical = input[["theory"]],
                   fractional = input[["comp_fractional"]])
})

##

output[["comparisonPlot_data"]] <- DT::renderDataTable(server = FALSE, {

  comparison_plot_data() %>%
    filter(Protein == input[["chosen_protein"]],
           Start >= input[["plot_x_range"]][[1]],
           End <= input[["plot_x_range"]][[2]]) %>%
    dt_format()
})

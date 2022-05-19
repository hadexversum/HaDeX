#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "diff_kin_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = times_from_file()[times_from_file() == as.numeric(input[["no_deut_control"]])])
})

##

observe({
  
  tmp <- sort(unique(round(dat()[["Exposure"]], 3)))
  choose_time_100 <- setNames(tmp, c(head(tmp, -1), "chosen control"))
  
  if(!has_modifications()){
    
    updateSelectInput(session,
                      inputId = "diff_kin_time_100",
                      choices =  choose_time_100,
                      selected = choose_time_100["chosen control"])
  }
  
  if(has_modifications()){
    
    updateSelectInput(session,
                      inputId = "diff_kin_time_100",
                      choices =  times_from_file()[times_from_file() < 99999],
                      selected = max(times_from_file()[times_from_file() < 99999]))
  }
  
  
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "diff_kin_state_1",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "diff_kin_state_2",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[length(states_chosen_protein())])
  
})


##

observe({
  
  updateTextInput(session,
                  inputId = "diff_kin_plot_title",
                  value = case_when(
                    input[["kin_theory"]] ~ paste0("Theoretical differential uptake curve for chosen peptides for ", input[["chosen_protein"]]),
                    !input[["kin_theory"]]  ~ paste0("Differential uptake curve for chosen peptides for ", input[["chosen_protein"]])
                  ))

})

##

observe({
  
  updateTextInput(session,
                  inputId = "diff_kin_plot_y_label",
                  value = case_when(
                    input[["kin_fractional"]] ~ "Differential fractional deuterium uptake [%]",
                    !input[["kin_fractional"]] ~ "Differential deuterium uptake [Da]",
                  ))
  
})

##

observe({
  
  if(input[["diff_kin_theory"]]){
    hide(id = "diff_kin_time_part")
  }
  
})

observe({
  
  if(!input[["diff_kin_theory"]]){
    show(id = "diff_kin_time_part")
  }
  
})

##

observe({
  
  if(!input[["diff_kin_fractional"]]){
    hide(id = "diff_kin_time_100_part")
  }
  
})

##

observe({
  
  if(input[["diff_kin_fractional"]]){
    show(id = "diff_kin_time_100_part")
  }
  
})

##

observe({
  
  if (!input[["diff_kin_fractional"]]) {
    
    min_kin_abs <- round_any(min(diff_kin_dat()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 1, floor)
    max_kin_abs <- round_any(max(diff_kin_dat()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 1, ceiling)
    
    updateSliderInput(session,
                      inputId = "diff_kin_plot_y_range",
                      min = -5,
                      max = max_kin_abs + 5,
                      value = c(0, max_kin_abs),
                      step = 1)
    
  } else {
    
    min_kin_abs <- round_any(min(diff_kin_dat()[c("diff_frac_deut_uptake", "diff_theo_frac_deut_uptake")], na.rm = TRUE), 5, floor)
    max_kin_abs <- round_any(max(diff_kin_dat()[c("diff_frac_deut_uptake", "diff_theo_frac_deut_uptake")], na.rm = TRUE), 5, ceiling)
    
    updateSliderInput(session,
                      inputId = "diff_kin_plot_y_range",
                      min = -20,
                      max = max_kin_abs + 20,
                      value = c(0, max_kin_abs),
                      step = 5)

  }
  
  
  
})

##

diff_peptide_list <- reactive({
  
  dat() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    select(Sequence, Start, End) %>%
    unique(.) %>%
    arrange(Start, End)
  
})

##

output[["diff_peptide_list_data"]] <- DT::renderDataTable({
  
  datatable(data = diff_peptide_list(),
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            selection = "single",
            options = list(pageLength = 10, dom = "tip", autoWidth = TRUE, target = 'cell'),
            filter = "bottom",
            rownames = FALSE)
  
})


#################################
######### DATASET ###############
#################################

diff_kin_dat <- reactive({
  
  validate(need(input[["diff_peptide_list_data_rows_selected"]], "Please select at least one peptide from the table on the left."))
  
  validate(need(input[["diff_kin_state_1"]]!=input[["diff_kin_state_2"]], "Choose two different states for comparison!"))
  
  # validate(need(!input[["diff_kin_houde"]], "This feature will be available soon!"))
  # validate(need(!input[["diff_kin_tstud"]], "This feature will be available soon!"))
  
  times_from_file <- unique(round(dat()[["Exposure"]], 3))
  
  if(input[["diff_kin_theory"]]){
    
    v_time_0 <- min(times_from_file[times_from_file > 0])
    v_time_100 <- max(times_from_file)
    
  } else {
    
    validate(need(as.numeric(input[["diff_kin_time_100"]]) > as.numeric(input[["diff_kin_time_0"]]), "Out time must be bigger than in time. "))
    
    validate(need(sum(times_from_file < as.numeric(input[["diff_kin_time_100"]]) & times_from_file > as.numeric(input[["diff_kin_time_0"]])) > 1, "Not enough time points between in and out time. "))
    
    v_time_0 <- as.numeric(input[["diff_kin_time_0"]])
    v_time_100 <- as.numeric(input[["diff_kin_time_100"]])
    
  }
  
  create_p_diff_uptake_dataset(dat = dat(),
                               protein = input[["chosen_protein"]],
                               state_1 = input[["diff_kin_state_1"]],
                               state_2 = input[["diff_kin_state_2"]],
                               # p_adjustment_method = "none",
                               confidence_level = as.numeric(input[["diff_kin_confidence_level"]]),
                               time_0 = v_time_0,
                               time_100 = v_time_100,
                               deut_part = as.numeric(input[["deut_part"]])/100)
})



#################################
######### PLOT ##################
#################################

diff_kin_plot <- reactive({
  
  plot_differential_uptake_curve(diff_p_uptake_dat = diff_kin_dat(),
                                 sequence = diff_peptide_list()[input[["diff_peptide_list_data_rows_selected"]], "Sequence"],
                                 theoretical = input[["diff_kin_theory"]],
                                 fractional = input[["diff_kin_fractional"]],
                                 uncertainty_type = input[["diff_kin_uncertainty"]],
                                 log_x = input[["diff_kin_log_x"]],
                                 show_houde_interval = input[["diff_kin_show_houde"]],
                                 show_tstud_confidence = input[["diff_kin_show_tstud"]])
})

##

diff_kp_out <- reactive({
  
  diff_kin_plot() +
    # geom_point(size = 3) +
    labs(title = input[["diff_kin_plot_title"]],
         x = input[["diff_kin_plot_x_label"]],
         y = input[["diff_kin_plot_y_label"]]) +
    coord_cartesian(ylim = c(input[["diff_kin_plot_y_range"]][1], input[["diff_kin_plot_y_range"]][2])) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(size = input[["diff_kin_plot_title_size"]]),
          axis.text.x = element_text(size = input[["diff_kin_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["diff_kin_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["diff_kin_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["diff_kin_plot_y_label_size"]]),
          legend.text = element_text(size = input[["diff_kin_plot_x_label_size"]]))
  
})

##

output[["diff_kinetic_plot_chosen_peptides"]] <- renderPlot({
  
  diff_kp_out()
  
})

##

output[["diff_kinetic_plot_chosen_peptides_debug"]] <- renderUI({

  if(!is.null(input[["diff_kinetic_plot_chosen_peptides_hover"]])) {

    plot_data <- diff_kp_out()[["data"]]
    hv <- input[["diff_kinetic_plot_chosen_peptides_hover"]]

    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]])

    tt_df <- filter(hv_dat, abs(y_plot - y) < 10, abs(y_plot - y) == min(abs(y_plot - y))) %>%
      filter(abs(x_plot - x) < 0.1*x_plot, abs(x_plot - x) == min(abs(x_plot - x)))

    if(nrow(tt_df) != 0) {

      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")

      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])


      style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                      tt_pos_adj, ":", tt_pos,
                      "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")

      div(
        style = style,
        p(HTML(paste0(tt_df[["Sequence"]],
                      "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
                      "<br/> Value: ", round(tt_df[["y_plot"]], 2),
                      "<br/> Time point: ", tt_df[["x_plot"]], " min")))
      )
    }
  }
})

##

output[["diff_kineticPlot_download_button"]] <- downloadHandler("diff_kineticPlot.svg",
                                                           content = function(file){
                                                             ggsave(file, diff_kp_out(), device = svg,
                                                                    height = 300, width = 400, units = "mm")
                                                           })
#################################
######### DATA ##################
#################################

diff_kin_plot_data <- reactive({
  
  show_diff_uptake_data(diff_uptake_dat = diff_kin_dat(),
                        theoretical = input[["diff_kin_theory"]],
                        fractional = input[["diff_kin_fractional"]])
})

##

output[["diff_kin_plot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  diff_kin_plot_data() %>%
    dt_format()
  
})

#################################
######### DOWNLOAD ##############
#################################

diff_all_kinetic_plots <- reactive({
  
  # browser()
  
  diff_peptide_list_download <- diff_peptide_list() %>%
    select(Sequence, Start, End) %>%
    unique(.)
  
  states_download <- unique(diff_peptide_list()[["State"]])
  
  plots <- lapply(1:nrow(diff_peptide_list_download), function(i){
    
    sequence = diff_peptide_list_download[i, 1]
    start = diff_peptide_list_download[i, 2]
    end = diff_peptide_list_download[i, 3]
    
    calculate_peptide_kinetics(dat(),
                               protein = input[["chosen_protein"]],
                               sequence = sequence,
                               states = states_download,
                               start = start,
                               end = end,
                               time_0 = as.numeric(input[["diff_kin_time_0"]]),
                               time_100 = as.numeric(input[["diff_kin_time_100"]])) %>%
      plot_kinetics(fractional = input[["diff_kin_fractional"]],
                    theoretical = input[["diff_kin_theory"]],
                    uncertainty_type = input[["diff_kin_uncertainty"]],
                    log_x = input[["diff_kin_log_x"]]) +
      labs(title = paste0(sequence, " (", start, "-", end, ")" ))
    
  })
  
  plots
  
})

##

diff_all_kinetic_plots_arranged <- reactive({
  
  marrangeGrob(grobs = diff_all_kinetic_plots(), 
               ncol = input[["diff_kin_download_file_columns"]], 
               nrow = input[["diff_kin_download_file_rows"]])
  
})

##

output[["diff_kin_download_file"]] <- downloadHandler("diff_all_deut_uptake_curves.pdf",
                                                 content = function(file){
                                                   ggsave(file, diff_all_kinetic_plots_arranged(), device = pdf,
                                                          height = 300, width = 400, units = "mm")
                                                 })

##

output[["diff_kin_download_folder"]] <- downloadHandler("diff_deut_uptake_curves.zip",
                                                   content = function(file){
                                                     owd <- setwd( tempdir())
                                                     on.exit( setwd( owd))
                                                     
                                                     plot_files <- lapply(1:length(diff_all_kinetic_plots()), function(i){
                                                       
                                                       label = paste0(i, "-", diff_all_kinetic_plots()[[i]][["labels"]][["title"]])
                                                       filename = paste0(label, ".png")
                                                       ggsave(filename, plot = diff_all_kinetic_plots()[[i]], device = "png")
                                                       
                                                       filename
                                                       
                                                     })
                                                     
                                                     zip( file, unlist(plot_files))
                                                     
                                                   })
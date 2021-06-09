#################################
######### SETTINGS ##############
#################################

observe({
  
  tmp <- sort(unique(round(dat()[["Exposure"]], 3)))
  choose_time_100 <- setNames(tmp, c(head(tmp, -1), "chosen control"))
  
  updateSelectInput(session,
                    inputId = "kin_time_0",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = min(times_from_file()[times_from_file() > 0]))
  
  if(!has_modifications()){
    
    updateSelectInput(session,
                      inputId = "kin_time_100",
                      choices =  choose_time_100,
                      selected = choose_time_100["chosen control"])
  }
  
  if(has_modifications()){
    
    updateSelectInput(session,
                      inputId = "kin_time_100",
                      choices =  times_from_file()[times_from_file() < 99999],
                      selected = max(times_from_file()[times_from_file() < 99999]))
  }
  
  
})

##

observe({
  
  updateTextInput(session,
                  inputId = "kin_plot_title",
                  value = case_when(
                    input[["kin_theory"]] ~ paste0("Theoretical uptake curve for chosen peptides for ", input[["chosen_protein"]]),
                    !input[["kin_theory"]]  ~ paste0("Uptake curve for chosen peptides for ", input[["chosen_protein"]])
                  ))
  
  updateTextInput(session,
                  inputId = "kin_plot_y_label",
                  value = case_when(
                    input[["kin_fractional"]] ~ "Fractional deuterium uptake [%]",
                    !input[["kin_fractional"]] ~ "Deuterium uptake [Da]",
                  ))
  
})

##

observe({
  
  if(input[["kin_theory"]]){
    hide(id = "kin_time_part")
  }
  
})

observe({
  
  if(!input[["kin_theory"]]){
    show(id = "kin_time_part")
  }
  
})

##

observe({
  
  if(!input[["kin_fractional"]]){
    hide(id = "kin_time_100_part")
  }
  
})

##

observe({
  
  if(input[["kin_fractional"]]){
    show(id = "kin_time_100_part")
  }
  
})

##

observe({
  
  if (!input[["kin_fractional"]]) {
    
    min_kin_abs <- round_any(min(kin_dat()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, floor)
    max_kin_abs <- round_any(max(kin_dat()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, ceiling)
    
    updateSliderInput(session,
                      inputId = "kin_plot_y_range",
                      min = 0,
                      max = max_kin_abs + 5,
                      value = c(0, max_kin_abs),
                      step = 1)
    
  } else {
    
    updateSliderInput(session,
                      inputId = "kin_plot_y_range",
                      min = -50,
                      max = 200,
                      value = c(-10, 100),
                      step = 10)
  }
  
})

##

peptide_list <- reactive({
  
  dat() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    select(Sequence, State, Start, End) %>%
    unique(.) %>%
    arrange(Start, End)
  
})

##

output[["peptide_list_data"]] <- DT::renderDataTable({
  
  datatable(data = peptide_list(),
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tip", autoWidth = TRUE, target = 'cell'),
            filter = "bottom",
            rownames = FALSE)
  
})

##

peptide_list_proxy <- DT::dataTableProxy("peptide_list_data", session = session)

##

observeEvent(input[["reset_peptide_list"]], {
  
  DT::selectRows(peptide_list_proxy, NULL)
  
})

#################################
######### DATASET ###############
#################################

kin_dat <- reactive({
  
  validate(need(input[["peptide_list_data_rows_selected"]], "Please select at least one peptide from the table on the left."))
  
  times_from_file <- unique(round(dat()[["Exposure"]], 3))
  
  if(input[["kin_theory"]]){
    
    v_time_0 <- min(times_from_file[times_from_file > 0])
    v_time_100 <- max(times_from_file)
    
  } else {
    
    validate(need(as.numeric(input[["kin_time_100"]]) > as.numeric(input[["kin_time_0"]]), "Out time must be bigger than in time. "))
    
    validate(need(sum(times_from_file < as.numeric(input[["kin_time_100"]]) & times_from_file > as.numeric(input[["kin_time_0"]])) > 1, "Not enough time points between in and out time. "))
    
    v_time_0 <- as.numeric(input[["kin_time_0"]])
    v_time_100 <- as.numeric(input[["kin_time_100"]])
    
  }
  
  create_kinetic_dataset(dat = dat(),
                         peptide_list = peptide_list()[input[["peptide_list_data_rows_selected"]], ],
                         protein = input[["chosen_protein"]],
                         deut_part = input[["deut_part"]],
                         time_0 = v_time_0,
                         time_100 = v_time_100)
})


#################################
######### PLOT ##################
#################################

kin_plot <- reactive({
  
  plot_kinetics(kin_dat = kin_dat(),
                theoretical = input[["kin_theory"]],
                fractional = input[["kin_fractional"]],
                uncertainty_type = input[["kin_uncertainty"]],
                log_x = input[["kin_log_x"]])
})

##

kp_out <- reactive({
  
  kin_plot() +
    geom_point(size = 3) +
    labs(title = input[["kin_plot_title"]],
         x = input[["kin_plot_x_label"]],
         y = input[["kin_plot_y_label"]]) +
    coord_cartesian(ylim = c(input[["kin_plot_y_range"]][1], input[["kin_plot_y_range"]][2])) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(size = input[["kin_plot_title_size"]]),
          axis.text.x = element_text(size = input[["kin_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["kin_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["kin_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["kin_plot_y_label_size"]]),
          legend.text = element_text(size = input[["kin_plot_x_label_size"]]))
  
})

##

output[["kinetic_plot_chosen_peptides"]] <- renderPlot({
  
  kp_out()
  
})

##

output[["kinetic_plot_chosen_peptides_debug"]] <- renderUI({
  
  if(!is.null(input[["kinetic_plot_chosen_peptides_hover"]])) {
    
    plot_data <- kp_out()[["data"]]
    hv <- input[["kinetic_plot_chosen_peptides_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         State = plot_data[["State"]])
    
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
                      "<br/> State: ", tt_df[["State"]],
                      "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
                      "<br/> Value: ", round(tt_df[["y_plot"]], 2),
                      "<br/> Time point: ", tt_df[["x_plot"]], " min")))
      )
    }
  }
})

##

output[["kineticPlot_download_button"]] <- downloadHandler("kineticPlot.svg",
                                                           content = function(file){
                                                             ggsave(file, kp_out(), device = svg,
                                                                    height = 300, width = 400, units = "mm")
                                                           })
#################################
######### DATA ##################
#################################

kin_plot_data <- reactive({
  
  show_kinetic_data(dat = kin_dat(),
                    theoretical = input[["kin_theory"]],
                    fractional = input[["kin_fractional"]])
})

##

output[["kin_plot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  kin_plot_data() %>%
    dt_format()
  
})

#################################
######### DOWNLOAD ##############
#################################

all_kinetic_plots <- reactive({
  
  peptide_list_download <- peptide_list() %>%
    select(Sequence, Start, End) %>%
    unique(.)
  
  states_download <- unique(peptide_list()[["State"]])
  
  plots <- lapply(1:nrow(peptide_list_download), function(i){
    
    sequence = peptide_list_download[i, 1]
    start = peptide_list_download[i, 2]
    end = peptide_list_download[i, 3]
    
    calculate_peptide_kinetics(dat(),
                               protein = input[["chosen_protein"]],
                               sequence = sequence,
                               states = states_download,
                               start = start,
                               end = end,
                               time_0 = as.numeric(input[["kin_time_0"]]),
                               time_100 = as.numeric(input[["kin_time_100"]])) %>%
      plot_kinetics(fractional = input[["kin_fractional"]],
                    theoretical = input[["kin_theory"]]) +
                    # uncertainty_type = input[["kin_uncertainty"]],
                    # log_x = input[["kin_log_x"]]) +
      labs(title = paste0(sequence, " (", start, "-", end, ")" ))
    
  })
  
  plots
  
})

##

all_kinetic_plots_arranged <- reactive({
  
  marrangeGrob(grobs = all_kinetic_plots(), 
               ncol = input[["kin_download_file_columns"]], 
               nrow = input[["kin_download_file_rows"]])
  
})

##

output[["kin_download_file"]] <- downloadHandler("all_deut_uptake_curves.pdf",
                                                 content = function(file){
                                                   ggsave(file, all_kinetic_plots_arranged(), device = pdf,
                                                          height = 300, width = 400, units = "mm")
                                                 })

##

output[["kin_download_folder"]] <- downloadHandler("deut_uptake_curves.zip",
                                                   content = function(file){
                                                     owd <- setwd( tempdir())
                                                     on.exit( setwd( owd))
                                                     
                                                     plot_files <- lapply(1:length(all_kinetic_plots()), function(i){
                                                       
                                                       label = all_kinetic_plots()[[i]][["labels"]][["title"]]
                                                       filename = paste0(label, ".png")
                                                       ggsave(filename, device = "png")
                                                       
                                                       filename
                                                     
                                                     })
                                                     
                                                     zip( file, unlist(plot_files))
                                                     
                                                   })
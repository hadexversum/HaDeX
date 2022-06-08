#################################
######### SETTINGS ##############
#################################


observe({
  
  updateSelectInput(session,
                    inputId = "measures_state",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "measures_time",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = times_from_file()[3])
})

measures_peptide_list <- reactive({
  
  measures_of_peptides() %>%
    filter(Protein == input[["chosen_protein"]],
           State == input[["measures_state"]],
           Exposure == input[["measures_time"]]) %>%
    select(N, Sequence, Start, End, charges) %>%
    arrange(Start, End)
  
})

##

output[["measures_sequence"]] <- DT::renderDataTable({
  
  datatable(data = measures_peptide_list(),
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 8, dom = "tip", autoWidth = TRUE, target = 'cell'),
            selection = list(mode = "single"),
            filter = "bottom",
            rownames = FALSE)
  
})

##

measures_list_proxy <- DT::dataTableProxy("measures_sequence", session = session)

##

observe({
  
  updateTextInput(session,
                  inputId = "measures_plot_title",
                  value = paste0("Measurements of ", measures_peptide_list()[input[["measures_sequence_rows_selected"]], 2], " in ", input[["measures_time"]], " min in ", input[["measures_state"]], " state"))
})

#################################
######### DATASET ###############
#################################

##

measures_of_peptides <- reactive({
  
  dat() %>% 
    filter(Protein == input[["chosen_protein"]],
           State == input[["measures_state"]],
           Exposure == input[["measures_time"]])  %>% 
    select(Protein, State, Sequence, Start, End, Exposure, File, z) %>%
    group_by(Protein, State, Sequence, Start, End, Exposure) %>%
    summarize(N = length(unique(File)),
              charges = paste(unique(z), collapse = " ")) %>%
    ungroup(.)
  
})

#################################
######### PLOT ##################
#################################
## measurements 

measures_plot_out <- reactive({
  
  validate(need(input[["measures_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  plot_peptide_mass_measurement(dat(),
                                protein = input[["chosen_protein"]],
                                show_charge_values = input[["measures_show_charge"]],
                                state =  input[["measures_state"]],
                                sequence = measures_peptide_list()[input[["measures_sequence_rows_selected"]], 2][[1]],
                                time_t = as.numeric(input[["measures_time"]]))  + 
    labs(x = input[["measures_plot_x_label"]],
         y = input[["measures_plot_y_label"]],
         title = input[["measures_plot_title"]]) +
    theme(plot.title = element_text(size = input[["measures_plot_title_size"]]),
          axis.text.x = element_text(size = input[["measures_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["measures_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["measures_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["measures_plot_y_label_size"]]),
          legend.text = element_text(size = input[["measures_plot_x_label_size"]]),
          legend.title = element_text(size = input[["measures_plot_x_label_size"]]))
  
})

##

output[["measuresPlot"]] <- renderPlot({
  
  measures_plot_out()
  
})

##

output[["measuresPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["measuresPlot_hover"]])) {
    
    plot_data <- measures_plot_out()[["data"]]
    hv <- input[["measuresPlot_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         State = plot_data[["State"]])
    
    tt_df <- filter(hv_dat, abs(x_plot - x) < 0.1*x_plot, abs(x_plot - x) == min(abs(x_plot - x)))
    
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
                      "<br/> Value: ", round(tt_df[["x_plot"]], 2), " Da")))
      )
    }
  }
})

##

output[["measuresPlot_download_button"]] <- downloadHandler("measuresPlot.svg",
                                                              content = function(file){
                                                                ggsave(file, measures_plot_out(), device = svg,
                                                                       height = 300, width = 400, units = "mm")
                                                              })


#################################
######### DATA ##################
#################################

measures_plot_data_out <- reactive({
  
  validate(need(input[["measures_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  show_peptide_mass_measurement(calculate_exp_masses_per_replicate(dat()),
                                protein = input[["chosen_protein"]],
                                state =  input[["measures_state"]],
                                sequence = measures_peptide_list()[input[["measures_sequence_rows_selected"]], 2][[1]],
                                time_t = as.numeric(input[["measures_time"]]))
  
})

output[["measuresPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  measures_plot_data_out() %>%
    dt_format()
  
})
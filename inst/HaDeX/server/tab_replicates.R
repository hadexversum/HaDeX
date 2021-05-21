#################################
######### SETTINGS ##############
#################################

rep_peptide_list <- reactive({
  
  replicates_of_peptides() %>%
    filter(Protein == input[["chosen_protein"]],
           State == input[["rep_state"]],
           Exposure == input[["rep_time"]]) %>%
    select(N, Sequence, Start, End) %>%
    arrange(Start, End)
  
})

##

output[["rep_sequence"]] <- DT::renderDataTable({
  
  datatable(data = rep_peptide_list(),
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 8, dom = "tip", autoWidth = TRUE, target = 'cell'),
            selection = list(mode = "single"),
            filter = "bottom",
            rownames = FALSE)
  
})

##

peptide_list_proxy <- DT::dataTableProxy("rep_sequence", session = session)

##

observe({
  
  updateTextInput(session,
                  inputId = "rep_plot_title",
                  value = paste0("Measurements of ", rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2], " in ", input[["rep_time"]], " min in ", input[["rep_state"]], " state"))
})

#################################
######### DATASET ###############
#################################

replicates_of_peptides <- reactive({
 
  replicate_masses() %>%
    select(Protein, State, Sequence, Start, End, Exposure, File) %>%
    group_by(Protein, State, Sequence, Start, End, Exposure) %>%
    summarize(N = n()) %>%
    ungroup(.)
   
})

##

replicate_masses <- reactive({
  
  dat() %>%
    calculate_exp_masses_per_replicate() %>%
    group_by(Start, End) %>%
    mutate(ID = cur_group_id())
  
})

##

replicate_masses_time_t <- reactive({
  
  validate(need(input[["rep_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  replicate_masses() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    filter(State == input[["rep_state"]]) %>%
    filter(Sequence == rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2]) %>%
    filter(Exposure == as.numeric(input[["rep_time"]]))

})

#################################
######### PLOT ##################
#################################

replicate_plot_out <- reactive({

  avg_value <- mean(replicate_masses_time_t()[["avg_exp_mass"]])
  
  ggplot(replicate_masses_time_t(), aes(x = avg_exp_mass, y = File)) +
    geom_point(size = 3) +
    geom_vline(xintercept = avg_value, color = "red", linetype = "dashed", size = 1.5) + 
    labs(x = input[["rep_plot_x_label"]],
         y = input[["rep_plot_y_label"]],
         title = input[["rep_plot_title"]]) +
    theme(plot.title = element_text(size = input[["rep_plot_title_size"]]),
          axis.text.x = element_text(size = input[["rep_plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["rep_plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["rep_plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["rep_plot_y_label_size"]]))
  
})

##

output[["replicatesPlot"]] <- renderPlot({
  
  replicate_plot_out()
  
})

##

output[["replicatesPlot_debug"]] <- renderUI({
  
  if(!is.null(input[["replicatesPlot_hover"]])) {
    
    # browsser()
    
    plot_data <- replicate_plot_out()[["data"]]
    hv <- input[["replicatesPlot_hover"]]
    
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

output[["replicatesPlot_download_button"]] <- downloadHandler("replicatesPlot.svg",
                                                              content = function(file){
                                                                ggsave(file, replicate_plot_out(), device = svg,
                                                                       height = 300, width = 400, units = "mm")
})

#################################
######### DATASET ###############
#################################

output[["replicatesPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  replicate_masses_time_t() %>%
    mutate(avg_exp_mass = round(avg_exp_mass, 4)) %>%
    select(Protein, Sequence, Start, End, Exposure, File, avg_exp_mass) %>%
    rename(`Mass` = avg_exp_mass) %>%
    dt_format()
  
})

#################################
######## HISTOGRAM ##############
#################################

output[["replicates_histogram"]] <- renderPlot({
  
  replicate_masses() %>%
    filter(Exposure == input[["rep_time"]],
           Protein == input[["chosen_protein"]],
           State == input[["rep_state"]]) %>%
    select(Sequence, Start, End, ID) %>%
    group_by(Sequence, Start, End, ID) %>%
    summarize(n = n()) %>%
    ggplot() + 
      geom_col(aes(x = ID, y = n, fill = n)) +
    labs(title = paste0("Number of replicates for each peptide in ", input[["rep_state"]], " in ", input[["rep_time"]], " min"),
         x = "Peptide ID",
         y = "Number of replicates") +
    theme(legend.position = "none")
  
})

##

output[["all_replicates_histogram"]] <- renderPlot({

  replicate_masses() %>%
    filter(Protein == input[["chosen_protein"]],
           State == input[["rep_state"]],
           Exposure < 99999) %>%
    select(Sequence, Exposure, Start, End, ID) %>%
    group_by(Sequence, Exposure, Start, End, ID) %>%
    summarize(n = n()) %>%
    ggplot() +
      geom_col(aes(x = ID, y = n, fill = as.factor(Exposure))) +
    labs(title = paste0("Number of replicates for each peptide in ", input[["rep_state"]], " state"),
         x = "Peptide ID",
         y = "Number of replicates",
         fill = "Exposure") +
    theme(legend.position = "bottom")
  
})

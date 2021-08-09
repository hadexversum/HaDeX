#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "rep_state",
                    choices = states_from_file(),
                    selected = states_from_file()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "rep_time",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = times_from_file()[3])
})


##

rep_peptide_list <- reactive({
  
  replicates_of_peptides() %>%
    filter(Protein == input[["chosen_protein"]],
           State == input[["rep_state"]],
           Exposure == input[["rep_time"]]) %>%
    select(N, Sequence, Start, End, charges) %>%
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

##

replicates_of_peptides <- reactive({
  
  dat() %>% 
    filter(Protein == input[["chosen_protein"]],
           State == input[["rep_state"]],
           Exposure == input[["rep_time"]])  %>% 
    select(Protein, State, Sequence, Start, End, Exposure, File, z) %>%
    group_by(Protein, State, Sequence, Start, End, Exposure) %>%
    summarize(N = length(unique(File)),
              charges = paste(unique(z), collapse = " ")) %>%
    ungroup(.)
  
})

##

replicate_masses <- reactive({
  
  dat() %>%
    calculate_exp_masses_per_replicate() %>%
    group_by(Start, End) %>%
    arrange(Start, End) %>%
    mutate(ID = cur_group_id())
  
})

##

replicate_masses_time_t <- reactive({
  
  validate(need(input[["rep_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  replicate_masses() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    filter(State == input[["rep_state"]]) %>%
    filter(Sequence == rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2][[1]]) %>%
    filter(Exposure == as.numeric(input[["rep_time"]]))

})

##

replicates_z_values_time_t <- reactive({
  
  validate(need(input[["rep_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
  
  dat() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    filter(State == input[["rep_state"]]) %>%
    filter(Sequence == rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2][[1]]) %>%
    filter(Exposure == as.numeric(input[["rep_time"]])) %>%
    select(Protein, Sequence, Start, End, Exposure, State, File, z)
  
})

#################################
######### PLOT ##################
#################################
## measurements 

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
######### PLOT ##################
#################################
## charge values

replicate_charge_plot_out <- reactive({
  
  n_bins <- length(unique(replicates_z_values_time_t()[["z"]]))
  min_z <- min(replicates_z_values_time_t()[["z"]])
  max_z <- max(replicates_z_values_time_t()[["z"]])
  
  replicates_z_values_time_t() %>%
    ggplot(aes(x = z)) +
    geom_histogram(aes(fill = File), bins = n_bins) + 
    scale_x_continuous(breaks = c(min_z:max_z)) 
  
})

##

output[["replicatesChargePlot"]] <- renderPlot({
  
  replicate_charge_plot_out()
    
})

##

output[["replicatesChargePlot_debug"]] <- renderUI({
  
  # if(!is.null(input[["replicatesChargePlot_hover"]])) {
  #   
  #   plot_data <- replicate_charge_plot_out()[["data"]]
  #   hv <- input[["replicatesChargePlot_hover"]]
  #   
  #   hv_dat <- data.frame(x = hv[["x"]],
  #                        y = hv[["y"]],
  #                        Start = plot_data[["Start"]],
  #                        End = plot_data[["End"]],
  #                        x_plot = plot_data[[hv[["mapping"]][["x"]]]],
  #                        y_plot = plot_data[[hv[["mapping"]][["y"]]]],
  #                        Sequence = plot_data[["Sequence"]],
  #                        State = plot_data[["State"]])
  #   
  #   tt_df <- filter(hv_dat, abs(x_plot - x) < 0.1*x_plot, abs(x_plot - x) == min(abs(x_plot - x)))
  #   
  #   if(nrow(tt_df) != 0) {
  #     
  #     tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
  #                          "left", "right")
  #     
  #     tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
  #                      hv[["coords_css"]][["x"]],
  #                      hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
  #     
  #     
  #     style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
  #                     tt_pos_adj, ":", tt_pos,
  #                     "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
  #     
  #     div(
  #       style = style,
  #       p(HTML(paste0(tt_df[["Sequence"]],
  #                     "<br/> State: ", tt_df[["State"]],
  #                     "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
  #                     "<br/> Value: ", round(tt_df[["x_plot"]], 2), " Da")))
  #     )
  #   }
  # }
})

output[["replicatesChargePlot_download_button"]] <- downloadHandler("replicatesChargePlot.svg",
                                                              content = function(file){
                                                                ggsave(file, replicate_charge_plot_out(), device = svg,
                                                                       height = 300, width = 400, units = "mm")
                                                              })

#################################
######### DATASET ###############
#################################

replicates_plot_data_out <- reactive({
  
  replicate_masses_time_t() %>%
    mutate(avg_exp_mass = round(avg_exp_mass, 4)) %>%
    select(Protein, Sequence, Start, End, Exposure, State, File, avg_exp_mass) %>%
    rename(`Mass` = avg_exp_mass)
  
})

output[["replicatesPlot_data"]] <- DT::renderDataTable(server = FALSE, {
  
  replicates_plot_data_out() %>%
    dt_format()
  
})

##

replicate_charge_plot_data_out <- reactive({
  
  replicates_z_values_time_t()
  
})


output[["replicatesChargePlot_data"]] <- DT::renderDataTable(server = FALSE, {

  replicate_charge_plot_data_out() %>%
    dt_format()
  
})


#################################
######## HISTOGRAM PLOT #########
#################################
## replicates in selected time point

replicates_histogram_data <- reactive({
  
  replicate_masses() %>%
    filter(Exposure == input[["rep_time"]],
           Protein == input[["chosen_protein"]],
           State == input[["rep_state"]]) %>%
    select(Sequence, Start, End, ID) %>%
    group_by(Sequence, Start, End, ID) %>%
    summarize(n = n())
  
})

##

replicates_histogram_out <- reactive({
  
  replicates_histogram_data() %>%
    ggplot() + 
    geom_col(aes(x = ID, y = n, fill = n)) +
    labs(title = paste0("Number of replicates for each peptide in ", input[["rep_state"]], " in ", input[["rep_time"]], " min"),
         x = "Peptide ID",
         y = "Number of replicates") +
    theme(legend.position = "none")
  
})

##

output[["replicatesHistogram"]] <- renderPlot({
  
  replicates_histogram_out()
  
})

##

output[["replicatesHistogram_debug"]] <- renderUI({
  
  if(!is.null(input[["replicatesHistogram_hover"]])) {

    plot_data <- replicates_histogram_out()[["data"]]
    hv <- input[["replicatesHistogram_hover"]]

    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         ID = plot_data[["ID"]],
                         n = plot_data[["n"]])

    tt_df <- filter(hv_dat, 
                    abs(x_plot - x) < 0.5, 
                    abs(x_plot - x) == min(abs(x_plot - x)))

    if(nrow(tt_df) != 0) {

      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")

      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])

      style <- paste0("position:absolute; z-index:1072; background-color: rgba(245, 245, 245, 1); pointer-events: none; ",
                      tt_pos_adj, ":", tt_pos, "px; padding: 0px;",
                      "top:", hv[["coords_css"]][["y"]] , "px; ")
      
      div(
        style = style,
        p(HTML(paste0(tt_df[["Sequence"]],
                      "<br/> ID: ", tt_df[["ID"]],
                      "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
                      "<br/> Replicates: ", tt_df[["n"]])))
      )
    }
  }
  
})

##

output[["replicatesHistogram_download_button"]] <- downloadHandler("replicatesHistogram.svg",
                                                                    content = function(file){
                                                                      ggsave(file, replicates_histogram_out(), device = svg,
                                                                             height = 300, width = 400, units = "mm")})

####################################
######## HISTOGRAM DATASET #########
####################################
## replicates in selected time point

replicates_histogram_data_out <- reactive({
  
  replicates_histogram_data() %>%
    arrange(ID)
  
})


output[["replicatesHistogram_data"]] <- DT::renderDataTable(server = FALSE, {
  
  replicates_histogram_data_out() %>%
    dt_format()
  
})

#################################
######## HISTOGRAM PLOT #########
#################################
## replicates in all time points

all_replicates_histogram_data <- reactive({
  
  replicate_masses() %>%
    filter(Protein == input[["chosen_protein"]],
           State == input[["rep_state"]],
           Exposure < 99999) %>%
    select(Sequence, Exposure, Start, End, ID) %>%
    group_by(Sequence, Exposure, Start, End, ID) %>%
    summarize(n = n())
  
})

##

all_replicates_histogram_out <- reactive({
  
  all_replicates_histogram_data() %>%
    ggplot() +
    geom_col(aes(x = ID, y = n, fill = as.factor(Exposure))) +
    labs(title = paste0("Number of replicates for each peptide in ", input[["rep_state"]], " state"),
         x = "Peptide ID",
         y = "Number of replicates",
         fill = "Exposure") +
    theme(legend.position = "bottom")
  
})

##

output[["allReplicatesHistogram"]] <- renderPlot({

  all_replicates_histogram_out()
  
})

##

output[["allReplicatesHistogram_debug"]] <- renderUI({
  
  if(!is.null(input[["allReplicatesHistogram_hover"]])) {
    
    plot_data <- all_replicates_histogram()[["data"]]
    hv <- input[["allReplicatesHistogram_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]],
                         ID = plot_data[["ID"]],
                         Exposure = plot_data[["Exposure"]],
                         n = plot_data[["n"]])
    
    tt_df <- filter(hv_dat, 
                    abs(x_plot - x) < 0.5, 
                    abs(x_plot - x) == min(abs(x_plot - x))) %>%
      arrange(Exposure)
    
    if(nrow(tt_df) != 0) {
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      style <- paste0("position:absolute; z-index:1072; background-color: rgba(245, 245, 245, 1); pointer-events: none; ",
                      tt_pos_adj, ":", tt_pos, "px; padding: 0px;",
                      "top:", hv[["coords_css"]][["y"]] , "px; ")
      
      tmp1 <- paste0("<br/> ", unique(tt_df[["Sequence"]]),
                     "<br/> ID: ", unique(tt_df[["ID"]]),
                     "<br/> Position: ", unique(tt_df[["Start"]]), "-", unique(tt_df[["End"]]))
      
      tmp2 <- paste0("<br/> Exposure: ", tt_df[["y_plot"]], " min, ",
                     "Replicates: ", round(tt_df[["n"]], 2), " ")
      div(
        style = style,
        p(HTML(tmp1), HTML(tmp2)  
        ))
      
      
    }
  }
  
})

##

output[["allReplicatesHistogram_download_button"]] <- downloadHandler("allReplicatesHistogram.svg",
                                                                   content = function(file){
                                                                     ggsave(file, all_replicates_histogram(), device = svg,
                                                                            height = 300, width = 400, units = "mm")
                                                                   })

####################################
######## HISTOGRAM DATASET #########
####################################
## replicates in all time points

all_replicates_histogram_data_out <- reactive({
  
  all_replicates_histogram_data() %>%
    arrange(ID, Exposure)
  
})


output[["allReplicatesHistogram_data"]] <- DT::renderDataTable(server = FALSE, {
  
  all_replicates_histogram_data_out() %>%
    dt_format()
  
})
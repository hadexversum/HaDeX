#################################
######### SETTINGS ##############
#################################

observe({
  
  updateSelectInput(session,
                    inputId = "rep_state",
                    choices = states_chosen_protein(),
                    selected = states_chosen_protein()[1])
})

##

observe({
  
  updateSelectInput(session,
                    inputId = "rep_time",
                    choices = times_from_file()[times_from_file() < 99999],
                    selected = times_from_file()[3])
})







#################################
######### PLOT ##################
#################################
## charge values

# replicate_charge_plot_out <- reactive({
#   
#   validate(need(input[["rep_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
#   
#   plot_peptide_charge_measurement(dat(),
#                                   protein = input[["chosen_protein"]],
#                                   state =  input[["rep_state"]],
#                                   sequence = rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2][[1]],
#                                   time_t = as.numeric(input[["rep_time"]]))
#   
# })
# 
# ##
# 
# output[["replicatesChargePlot"]] <- renderPlot({
#   
#   replicate_charge_plot_out()
#     
# })

##

# output[["replicatesChargePlot_debug"]] <- renderUI({
#   
#   # if(!is.null(input[["replicatesChargePlot_hover"]])) {
#   #   
#   #   plot_data <- replicate_charge_plot_out()[["data"]]
#   #   hv <- input[["replicatesChargePlot_hover"]]
#   #   
#   #   hv_dat <- data.frame(x = hv[["x"]],
#   #                        y = hv[["y"]],
#   #                        Start = plot_data[["Start"]],
#   #                        End = plot_data[["End"]],
#   #                        x_plot = plot_data[[hv[["mapping"]][["x"]]]],
#   #                        y_plot = plot_data[[hv[["mapping"]][["y"]]]],
#   #                        Sequence = plot_data[["Sequence"]],
#   #                        State = plot_data[["State"]])
#   #   
#   #   tt_df <- filter(hv_dat, abs(x_plot - x) < 0.1*x_plot, abs(x_plot - x) == min(abs(x_plot - x)))
#   #   
#   #   if(nrow(tt_df) != 0) {
#   #     
#   #     tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
#   #                          "left", "right")
#   #     
#   #     tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
#   #                      hv[["coords_css"]][["x"]],
#   #                      hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
#   #     
#   #     
#   #     style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
#   #                     tt_pos_adj, ":", tt_pos,
#   #                     "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")
#   #     
#   #     div(
#   #       style = style,
#   #       p(HTML(paste0(tt_df[["Sequence"]],
#   #                     "<br/> State: ", tt_df[["State"]],
#   #                     "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]],
#   #                     "<br/> Value: ", round(tt_df[["x_plot"]], 2), " Da")))
#   #     )
#   #   }
#   # }
# })

# output[["replicatesChargePlot_download_button"]] <- downloadHandler("replicatesChargePlot.svg",
#                                                               content = function(file){
#                                                                 ggsave(file, replicate_charge_plot_out(), device = svg,
#                                                                        height = 300, width = 400, units = "mm")
#                                                               })



##

# replicate_charge_plot_data_out <- reactive({
#   
#   validate(need(input[["rep_sequence_rows_selected"]], "Please select one peptide from the table on the left."))
#   
#   show_peptide_charge_measurement(dat(),
#                                   protein = input[["chosen_protein"]],
#                                   state =  input[["rep_state"]],
#                                   sequence = rep_peptide_list()[input[["rep_sequence_rows_selected"]], 2][[1]],
#                                   time_t = as.numeric(input[["rep_time"]]))
#   
# })
# 
# 
# output[["replicatesChargePlot_data"]] <- DT::renderDataTable(server = FALSE, {
# 
#   replicate_charge_plot_data_out() %>%
#     dt_format()
#   
# })


#################################
######## HISTOGRAM PLOT #########
#################################
## replicates in selected time point

replicates_histogram_data <- reactive({
  
  # subset from all_replicates_histogram_data() !!
  
  # all_replicates_histogram_data() %>%
  #   filter(Exposure == input[["rep_time"]])
  
  create_replicate_dataset(dat(), 
                           time_t = input[["rep_time"]],
                           protein = input[["chosen_protein"]],
                           input[["rep_state"]])

})

##

replicates_histogram_out <- reactive({
  
  plot_replicate_histogram(replicates_histogram_data())
  
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
  
  show_replicate_histogram_data(replicates_histogram_data())
  
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
  
  create_replicate_dataset(dat(), 
                           protein = input[["chosen_protein"]],
                           state = input[["rep_state"]])

})

##

all_replicates_histogram_out <- reactive({
  
  plot_replicate_histogram(all_replicates_histogram_data())
  
})

##

output[["allReplicatesHistogram"]] <- renderPlot({

  all_replicates_histogram_out()
  
})

##

output[["allReplicatesHistogram_debug"]] <- renderUI({
  
  if(!is.null(input[["allReplicatesHistogram_hover"]])) {
    
    plot_data <- all_replicates_histogram_out()[["data"]]
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
      
      tmp2 <- paste0("<br/> Exposure: ", tt_df[["Exposure"]], " min, ",
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
                                                                     ggsave(file, all_replicates_histogram_out(), device = svg,
                                                                            height = 300, width = 400, units = "mm")
                                                                   })

####################################
######## HISTOGRAM DATASET #########
####################################
## replicates in all time points

all_replicates_histogram_data_out <- reactive({
  
  show_replicate_histogram_data(all_replicates_histogram_data())
  
})

##

output[["allReplicatesHistogram_data"]] <- DT::renderDataTable(server = FALSE, {
  
  all_replicates_histogram_data_out() %>%
    dt_format()
  
})


#################################
######## HISTOGRAM PLOT #########
#################################
## replicates for time points

times_replicates_histogram_out <- reactive({
  
  plot_replicate_histogram(all_replicates_histogram_data(),
                           time_point = T)
  
})

output[["timesReplicatesHistogram"]] <- renderPlot({
  
  times_replicates_histogram_out()
  
})

##

output[["timesReplicatesHistogram_debug"]] <- renderUI({
  
  if(!is.null(input[["timesReplicatesHistogram_hover"]])) {
    
    plot_data <- times_replicates_histogram_out()[["data"]]
    hv <- input[["timesReplicatesHistogram_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         x_plot = plot_data[["Exposure"]],
                         Sequence = plot_data[["Sequence"]],
                         ID = plot_data[["ID"]],
                         Exposure = plot_data[["Exposure"]],
                         n = plot_data[["n"]])
    
    tt_df <- hv_dat %>%
        group_by(x, Exposure) %>%
        summarize(cnt = sum(n),
                  n_peptides = n()) %>%
        arrange(Exposure) %>%
        mutate(id = 1:nrow(.)) %>%
        filter(id == round(x))
      
    if(nrow(tt_df) != 0) {
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      style <- paste0("position:absolute; z-index:1072; background-color: rgba(245, 245, 245, 1); pointer-events: none; ",
                      tt_pos_adj, ":", tt_pos, "px; padding: 0px;",
                      "top:", hv[["coords_css"]][["y"]] , "px; ")
      
      tmp <- paste0("<br/> ", tt_df[["Exposure"]], " [min]",
                     "<br/> Peptides: ", tt_df[["n_peptides"]],
                     "<br/> Total replicates: ", tt_df[["cnt"]]
                     )

      div(
        style = style,
        p(HTML(tmp)))
      
      
    }
  }
  
})

##

output[["timesReplicatesHistogram_download_button"]] <- downloadHandler("timesReplicatesHistogram.svg",
                                                                        content = function(file){
                                                                          ggsave(file, times_replicates_histogram_out(), device = svg,
                                                                                 height = 300, width = 400, units = "mm")
                                                                      })


####################################
######## HISTOGRAM DATASET #########
####################################
## replicates for time points

times_replicates_histogram_data_out <- reactive({

  show_replicate_histogram_data(all_replicates_histogram_data()) %>%
    arrange(Exposure, ID) %>%
    select(Exposure, everything())
  
})

##

output[["timesReplicatesHistogram_data"]] <- DT::renderDataTable(server = FALSE, {

  times_replicates_histogram_data_out() %>%
    dt_format()
})


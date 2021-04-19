#################################
######### DATASET ###############
#################################

stateOverlap_data <- reactive({
  
  generate_overlap_data(dat = dat(),
                        protein = input[["chosen_protein"]],
                        state = input[["chosen_state"]],
                        start = input[["plot_range"]][[1]],
                        end = input[["plot_range"]][[2]])
})

output[["stateOverlap_data"]] <- DT::renderDataTable(server = FALSE, {
  
  stateOverlap_data() %>%
    dt_format(cols = c("Protein", "Sequence", "Start", "End"))
  
})

#################################
######### PLOT ##################
#################################

stateOverlap_out <- reactive({
  
  generate_overlap_plot(dat = stateOverlap_data()) +
    coord_cartesian(xlim = c(input[["plot_range"]][[1]], input[["plot_range"]][[2]]))
  
})

##

output[["stateOverlap"]] <- renderPlot({
  
  stateOverlap_out() +
    labs(title = paste0("Peptide coverage for ", input[["chosen_protein"]]))
  
})

output[["stateOverlap_debug"]] <- renderUI({
  
  if(!is.null(input[["stateOverlap_hover"]])) {
    
    plot_data <- stateOverlap_out()[["data"]]
    hv <- input[["stateOverlap_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         Start = plot_data[["Start"]],
                         End = plot_data[["End"]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         Sequence = plot_data[["Sequence"]])
    
    tt_df <- filter(hv_dat, Start < x, End > x) %>%
      filter(abs(y_plot - y) < 5) %>%
      filter(abs(y_plot - y) == min(abs(y_plot - y)))
    
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
                      "<br/> Position: ", tt_df[["Start"]], "-", tt_df[["End"]])))
      )
    }
  }
})

##

output[["protein_length"]] <- renderText({
  
  max_range()
  
})

##

output[["stateOverlap_download_button"]] <- downloadHandler("stateOverlap.svg",
                                                            content = function(file){
                                                              ggsave(file, stateOverlap_out(), device = svg,
                                                                     height = 300, width = 400, units = "mm")
                                                            })

#################################
######### DATASET ###############
#################################

stateOverlapDist_data <- reactive({
  
  generate_overlap_distribution_data(dat(),
                                     protein = input[["chosen_protein"]],
                                     state = input[["chosen_state"]],
                                     start = input[["plot_range"]][[1]],
                                     end = input[["plot_range"]][[2]],
                                     protein_sequence = protein_sequence())
})

##

output[["stateOverlapDist_data"]] <- DT::renderDataTable(server = FALSE, {
  
  dt_format(stateOverlapDist_data(),
            cols = c("Position", "Amino acid", "Coverage"))
  
})

#################################
######### PLOT ##################
#################################

stateOverlapDist <- reactive({
  
  generate_overlap_distribution_plot(dat = stateOverlapDist_data(),
                                     start = input[["plot_range"]][[1]],
                                     end = input[["plot_range"]][[2]])
})

##

output[["stateOverlapDist"]] <- renderPlot({
  
  stateOverlapDist()
  
})

##

output[["stateOverlapDist_debug"]] <- renderUI({
  
  if(!is.null(input[["stateOverlapDist_hover"]])) {
    
    plot_data <- stateOverlapDist()[["data"]]
    hv <- input[["stateOverlapDist_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         x_plot = plot_data[["pos"]],
                         amino = plot_data[["amino"]],
                         coverage = plot_data[["coverage"]])
    
    tt_df <- filter(hv_dat, abs(x - x_plot) < 0.5)
    
    if(nrow(tt_df) != 0) {
      
      tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                           "left", "right")
      
      tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                       hv[["coords_css"]][["x"]],
                       hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])
      
      
      style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                      tt_pos_adj, ":", tt_pos, "px; padding: 0px;",
                      "top:", hv[["coords_css"]][["y"]] , "px; ")
      
      div(
        style = style,
        p(HTML(paste0("<br/> Position: ", tt_df[["x_plot"]],
                      "<br/> Amino acid: ", tt_df[["amino"]],
                      "<br/> Coverage: ", tt_df[["coverage"]])))
      )
    }
  }
})

##

output[["stateOverlapDist_download_button"]] <- downloadHandler("stateOverlapDist.svg",
                                                                content = function(file){
                                                                  ggsave(file, stateOverlapDist(), device = svg,
                                                                         height = 300, width = 400, units = "mm")
                                                                })
##


output[["cov_protein_coverage"]] <- renderText({
  
  
  # cov_text <- 
    
  paste0("Total protein coverage: ", round(get_protein_coverage(dat(), protein = input[["chosen_protein"]], protein_length = max_range()), 4),  "% \n ")
  
  # tmp_text <- lapply(states_chosen_protein(), function(state){
  #   
  #   paste0( "Protein coverage in state ", state, ": ", round(get_protein_coverage(dat(), 
  #                                   protein = input[["chosen_protein"]],
  #                                   states = state,
  #                                   protein_length = max_range()), 4), "% \n ")
  #   
  # }) %>% paste0()
  # 
  # paste0(cov_text, tmp_text)
  
})



#################################
######### DATASET ###############
#################################

stateOverlap_data <- reactive({
  
  show_overlap_data(
    dat = dat(),
    protein = input[["chosen_protein"]],
    state = input[["chosen_state"]],
    start = input[["plot_range"]][[1]],
    end = input[["plot_range"]][[2]]
  )
  
})

output[["stateOverlap_data"]] <- DT::renderDataTable(server = FALSE, {
  
  stateOverlap_data() %>%
    dt_format(cols = c("Protein", "Sequence", "ID", "Start", "End"))
  
})

#################################
######### PLOT ##################
#################################

stateOverlap_out <- reactive({
  
  # browser()
  
  plot_coverage(dat = dat(),
                protein = input[["chosen_protein"]],
                states = input[["chosen_state"]]) +
    coord_cartesian(xlim = c(input[["plot_range"]][[1]], input[["plot_range"]][[2]]))
  
})

##

output[["stateOverlap"]] <- renderPlot({
  
  stateOverlap_out() +
    labs(title = paste0("Peptide coverage for ", input[["chosen_protein"]], " in ", input[["chosen_state"]]))
  
})


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
  
  create_overlap_distribution_dataset(dat(),
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
  
  plot_overlap_distribution(overlap_dist_dat = stateOverlapDist_data(),
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
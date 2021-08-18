#################################
######### SETTINGS ##############
#################################

observe({
  
  possible_states <- unique(dat()[["State"]])
  
  updateRadioButtons(session,
                     inputId = "chosen_state",
                     choices = possible_states)
  
})

#################################
######### DATA ##################
#################################

protein_name <- reactive({
  
  as.character(unique(dat()[["Protein"]]))
  
})

##

output[["protein_name"]] <- renderText({
  
  input[["chosen_protein"]]
  
})

##

position_in_sequence_tmp <- reactive({
  
  dat() %>%
    filter(Protein == input[["chosen_protein"]]) %>%
    select(Start, End, Sequence) %>%
    unique(.) %>%
    apply(1, function(x) data.frame(position = x[1]:x[2], amino = strsplit(x[3], '')[[1]], stringsAsFactors = FALSE)) %>%
    bind_rows() %>%
    unique(.)
  
})

##

protein_sequence <- reactive({
  
  reconstruct_sequence(filter(dat(), Protein == input[["chosen_protein"]]), end = as.numeric(input[["sequence_length"]]))
  
})

##

position_in_sequence <- reactive({
  
  position_in_sequence_tmp() %>%
    left_join(amino_prop)
  
})

##

output[["protein_stats"]] <- renderTable({
  
  data.frame(
    Name = c("Length", "Coverage", "Cys"),
    Value = as.character(c(input[["sequence_length"]],
                           paste0(100*round((max_range()-str_count(protein_sequence(), 'x'))/max_range(), 4), '%'),
                           str_count(protein_sequence(), 'C'))),
    stringsAsFactors = FALSE
  )
  
})


##

protein_sequence_colored <- reactive({
  
  paste0("<span>",
         gsubfn(pattern = 'C', replacement = function(x) paste0('<font color = "red">', x, "</font>"), x = protein_sequence()),
         "</span>")
  
})

##

output[["sequenceName"]] <- renderText({
  
  protein_sequence_colored()
  
})

#################################
######### PLOT ##################
#################################

aminoDist_out <- reactive({
  
  plot_amino_distribution(position_in_sequence = position_in_sequence(),
                          hydro_properties = input[["hydro_prop"]],
                          protein = input[["chosen_protein"]],
                          charge_colors = c("-1" = "#E41A1C", "0" = "#377EB8", "1" = "#4DAF4A"))
})

##

output[["aminoDist"]] <- renderPlot({
  
  aminoDist_out()
  
})

##

output[["aminoDist_debug"]] <- renderUI({
  
  if(!is.null(input[["aminoDist_hover"]])) {
    
    plot_data <- aminoDist_out()[["data"]] %>%
      ungroup()
    
    hv <- input[["aminoDist_hover"]]
    
    hv_dat <- data.frame(x = hv[["x"]],
                         y = hv[["y"]],
                         x_plot = plot_data[[".group"]],
                         y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                         amino = plot_data[["amino"]],
                         charge = plot_data[["charge"]],
                         is_hydrophobic = plot_data[["is_hydrophobic"]],
                         count = plot_data[["cnt"]])
    
    tt_df <- filter(hv_dat, abs(x_plot - x) < 0.5) %>%
      filter(abs(x_plot -x ) == min(abs(x_plot - x)))
    
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
        p(HTML(paste0("<br/> Amino acid: ", tt_df[["amino"]],
                      "<br/> Charge: ", tt_df[["charge"]],
                      "<br/> Is hydrophobic? ", tt_df[["is_hydrophobic"]],
                      "<br/> Count: ", tt_df[["count"]])))
      )
    }
  }
})

##

output[["aminoDist_download_button"]] <- downloadHandler("aminoDist.svg",
                                                         content = function(file){
                                                           ggsave(file, aminoDist_out(), device = svg,
                                                                  height = 300, width = 400, units = "mm")
                                                         })
##
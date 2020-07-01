source("ui.R")

#########################################

server <- function(input, output, session) {
  
  ##
  
  observe_helpers(help_dir = "docs", withMathJax = TRUE)
  
  ##
  
  ### TAB: START ###
  
  ##
  
  output[["file_req"]] <- renderTable({
    
    file_req
    
  })
  
  dat_in <- reactive({
    
    inFile <- input[["data_file"]]
    
    if (is.null(inFile)){
      read_hdx('./data/KD_180110_CD160_HVEM.csv')
    } else {
      validate(need(try(read_hdx(inFile[["datapath"]])), "Check file requirements!"))
      read_hdx(inFile[["datapath"]])
    }
    
  })
  
  ##
  
  data_source <- reactive({
    
    attr(dat_in(), "source")
    
  })
  
  ##
  
  exam_protein_name_from_file <- reactive({ unique(dat_in()[["Protein"]]) })
  exam_state_name_from_file <- reactive({ unique(dat_in()[["State"]]) })
  
  observe({
    
    if(data_source() == "HDeXaminer"){
      shinyjs::show("examiner_settings")
    }
    
    updateTextInput(session, 
                    inputId = "exam_protein_name",
                    value = exam_protein_name_from_file())
    
    updateTextInput(session, 
                    inputId = "exam_state_name",
                    value = exam_state_name_from_file())
    
  })
  
  ##
  
  observe({
    
    if(data_source() != "HDeXaminer"){
      shinyjs::hide("examiner_settings")
    }
    
  })
  
  output[["data_file_info"]] <- renderText({
    
    status <- ""
    if (is.null(input[["data_file"]])){
      status <- "Example file: KD_180110_CD160_HVEM.csv."
    } else {
      length(dat_in()[[1]])
      status <- "Supplied file is valid."
    }
    
    if(data_source() == "HDeXaminer"){
      paste0(status, " Detected data source: ", data_source(), ". User action needed below!")
    } else {
      paste0(status, " Detected data source: ", data_source(), ".")
    }
    
    
  })
  
  ##
  
  ### TAB: INPUT DATA
  
  ##
  
  dat_exam <- eventReactive(input[["exam_apply_changes"]], {
    
    tryCatch({
      update_hdexaminer_file(dat_in(),
                             fd_time = input[["examiner_fd_timepoint"]],
                             old_protein_name = exam_protein_name_from_file(),
                             new_protein_name = input[["exam_protein_name"]],
                             old_state_name = exam_state_name_from_file(),
                             new_state_name = strsplit(input[["exam_state_name"]], ",")[[1]],
                             confidence = input[["exam_confidence"]])
    },
    error = function(e){
      validate(need(FALSE, conditionMessage(e)))
    })
    
  })
  
  ##
  #
  # editable tables for later, after cleaning the rest of the code
  #
  # exam_data_proxy <- DT::dataTableProxy("checking_exam_data")
  # 
  # exam_dat_checking_previous <- reactiveValues()
  # 
  # observe({
  #   
  #   exam_dat_checking_previous[["dat"]] <- dat_exam() %>%
  #     select(Protein, State, Sequence,  Start, End, MHP) %>%
  #     unique(.) %>%
  #     arrange(Start, End)
  #   
  # })
  # 
  # observeEvent(input[["checking_exam_data_cell_edit"]], {
  #   
  #   # browser()
  #   
  #   change <- input[["checking_exam_data_cell_edit"]]
  #   row_name <- change[["row"]]
  #   col_name <- change[["col"]]
  #   new_value <- change[["value"]]
  #   
  #   exam_dat_checking_previous[["dat"]][row_name, col_name] <- isolate(DT::coerceValue(new_value, exam_dat_checking_previous[["dat"]][row_name, col_name]))
  #     
  #     #DT::coerceValue(new_value, exam_dat_checking_previous[["dat"]][row_name, col_name])
  #   
  #   replaceData(exam_data_proxy, exam_dat_checking_previous[["dat"]], resetPaging = FALSE)
  #   
  # })
  # 
  # exam_dat_checking_after <- reactive({
  #   
  #   
  #   datatable(exam_dat_checking_previous[["dat"]], editable = TRUE)
  #   
  # })
  
  output[["checking_exam_data"]] <- DT::renderDataTable({
    
    # exam_dat_checking_after() 
    
    dat_exam() %>%
      select(Protein, State, Sequence,  Start, End, MHP) %>%
      unique(.) %>%
      arrange(Start, End)
    
  })
  
  ##
  
  dat_tmp <- reactive({
    
    if(data_source() == "HDeXaminer"){
      validate(need(input[["exam_apply_changes"]][[1]] != 0, "Apply changes in `Input Data` tab."))
      dat_curr <- dat_exam()
    } else {
      dat_curr <- dat_in()
    }
    
    dat_curr %>%
      mutate(Start = Start + input[["sequence_start_shift"]] -1,
             End = End + input[["sequence_start_shift"]] -1)
    
  })
  
  ## mark for modifications
  
  has_modifications <- reactive({
    
    any(!is.na(dat_in()[["Modification"]]))
    
  })
  
  ##
  
  observe({
    
    if(has_modifications()){
      hide("chosen_control")
    }
    
  })
  
  ##
  
  observe({
    
    if(!has_modifications()){
      show("chosen_control")
    }
    
  })
  
  ##
  
  proteins_from_file <- reactive({
    
    unique(dat_tmp()[["Protein"]])
    
  })
  
  ##
  
  max_range <- reactive({
    
    max(filter(dat_tmp(), Protein == input[["chosen_protein"]])[['End']])
    
  })
  
  ##
  
  observe({
    
    updateSelectInput(session,
                      inputId = "chosen_protein",
                      choices = proteins_from_file(),
                      selected = proteins_from_file()[1])
    
  })
  
  ##
  
  options_for_control <- reactive({
    
    dat_tmp() %>%
      filter(Protein == input[["chosen_protein"]]) %>%
      mutate(Exposure = round(Exposure, 4)) %>%
      select(Protein, State, Exposure) %>%
      arrange(State, desc(Exposure)) %>%
      unique(.) %>%
      mutate(control = paste0(Protein, " | ", State, " | ", Exposure)) %>%
      select(control) 
    
  })
  
  ##
  
  observe({
    
    tryCatch({
      if(input[["deut_concentration"]] < 0)
        updateNumericInput(session,
                           inputId = "deut_concentration",
                           value = 0)
    }, error = function(e){
      updateNumericInput(session,
                         inputId = "deut_concentration",
                         value = 0)
    })
  })
  
  observe({
    
    tryCatch({
      if(input[["deut_concentration"]] > 100)
        updateNumericInput(session,
                           inputId = "deut_concentration",
                           value = 100)
    },
    error = function(e){
      updateNumericInput(session,
                         inputId = "deut_concentration",
                         value = 100)
    })
    
  })
  
  observe({
    
    tryCatch({
      if(input[["sequence_length"]] < max_range())
        updateNumericInput(session,
                           inputId = "sequence_length",
                           value = max_range())
    },
    error = function(e){
      updateNumericInput(session,
                         inputId = "sequence_length",
                         value = max_range())
    })
    
  })
  
  observe({
    
    tryCatch(
      { if(input[["sequence_start_shift"]] < 0)
        updateNumericInput(session,
                           inputId = "sequence_start_shift",
                           value = 1) },
      error = function(e) {
        message("You cannot shift it to minus values!")
        updateNumericInput(session,
                           inputId = "sequence_start_shift",
                           value = 1) })
  })
  
  ##
  
  observe({
    
    updateSelectInput(session, 
                      inputId = "chosen_control", 
                      choices = options_for_control())
    
    updateNumericInput(session, 
                       inputId = "sequence_length",
                       value = max_range())
    
  })
  
  ##
  
  ## create dat based on control
  
  dat <- reactive({
    
    generate_general_data_set(dat = dat_tmp(),
                              control_protein = input[["chosen_protein"]], 
                              control_state = strsplit(input[["chosen_control"]], " \\| ")[[1]][2],
                              control_exposure = strsplit(input[["chosen_control"]], " \\| ")[[1]][3])
  })
  
  
  
  ### TAB: SEQUENCE DATA ###
  
  ##
  
  observe({
    
    possible_states <- unique(dat()[["State"]])
    
    updateRadioButtons(session,
                       inputId = "chosen_state",
                       choices = possible_states)
    
  })
  
  ##
  
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
    
    reconstruct_sequence(filter(dat(), Protein == input[["chosen_protein"]]))
    
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
                             paste0(100*round((max_range()-str_count(protein_sequence(), 'x'))/input[["sequence_length"]], 4), '%'),
                             str_count(protein_sequence(), 'C'))),
      stringsAsFactors = FALSE
    )
    
  })
  
  ##
  
  output[["sequence_length_exp_info"]] <- renderText({
    
    paste("Sequence length from the file is ", max_range(), ".")
    
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
  
  ##
  
  aminoDist_out <- reactive({
    
    generate_amino_distribution(position_in_sequence = position_in_sequence(),
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
  
  ### TAB: COVERAGE ###
  
  ##
  
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
  
  ##
  
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
  
  ##
  
  output[["stateOverlap_download_button"]] <- downloadHandler("stateOverlap.svg",
                                                              content = function(file){
                                                                ggsave(file, stateOverlap_out(), device = svg, 
                                                                       height = 300, width = 400, units = "mm")
                                                              })
  
  ##
  
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
  
  ##
  
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
  
  ### TAB: WOODS PLOT ###
  
  ##
  
  output[["protein_length"]] <- renderText({
    
    max_range()
    
  })
  
  ##
  
  states_from_file <- reactive({
    
    unique(dat()[["State"]])
    
  })
  
  ## modification actions
  
  observe({
    
    times_from_file <- unique(round(dat()[["Exposure"]], 3))
    times_from_file <- times_from_file[order(times_from_file)]
    
    tmp <- sort(unique(round(dat()[["Exposure"]], 3)))
    choose_time_out <- setNames(tmp, c(head(tmp, -1), "chosen control"))
    
    if(has_modifications()){
      
      updateSelectInput(session, 
                        inputId = "out_time",
                        choices = times_from_file[times_from_file < 99999],
                        selected = max(times_from_file[times_from_file < 99999]))
      
    }
    
    if(!has_modifications()){
      
      updateSelectInput(session, 
                        inputId = "out_time",
                        choices = choose_time_out,
                        selected = choose_time_out["chosen control"])
    }
  })
  
  ##
  
  # times from file defined as reactive below!
  
  observe({
    
    updateSelectInput(session, 
                      inputId = "chosen_time",
                      choices = times_from_file()[times_from_file() < 99999],
                      selected = min(times_from_file()[times_from_file() > input[["in_time"]]]))
  })
  
  observe({
    
    updateSelectInput(session, 
                      inputId = "in_time",
                      choices = times_from_file()[times_from_file() < 99999],
                      selected = min(times_from_file()[times_from_file() > 0]))
    
    
    updateSelectInput(session,
                      inputId = "state_first",
                      choices = states_from_file(),
                      selected = states_from_file()[1])
    
    updateSelectInput(session,
                      inputId = "state_second",
                      choices = states_from_file(),
                      selected = states_from_file()[length(states_from_file())])
    
    updateCheckboxGroupInput(session,
                             inputId = "compare_states",
                             choices = states_from_file(),
                             selected = states_from_file())
    
    updateSelectInput(session,
                      inputId = "confidence_limit_2",
                      choices = confidence_limit_choices[confidence_limit_choices >= input[["confidence_limit"]]],
                      selected = confidence_limit_choices[confidence_limit_choices > input[["confidence_limit"]]][1])
    
    updateSliderInput(session, 
                      inputId = "plot_range",
                      max = max_range(),
                      value = c(1, max_range()))
    
    updateSliderInput(session, 
                      inputId = "plot_x_range",
                      max = max_range(),
                      value = c(1, max_range()))
    
  })
  
  ##
  
  observe({
    
    if (input[["calc_type"]] == "absolute") {
      
      min_comparison_abs <- round_any(min(prep_dat()[c("abs_frac_exch_state", "abs_avg_theo_in_time")], na.rm = TRUE), 5, floor)
      max_comparison_abs <- round_any(max(prep_dat()[c("abs_frac_exch_state", "abs_avg_theo_in_time")], na.rm = TRUE), 5, ceiling)
      
      updateSliderInput(session,
                        inputId = "comp_plot_y_range",
                        min = min_comparison_abs - 5,
                        max = max_comparison_abs + 5,
                        value = c(min_comparison_abs, max_comparison_abs),
                        step = 1)
      
      min_woods_abs <- round_any(min(woods_plot_dat()[c("abs_diff_frac_exch", "abs_diff_theo_frac_exch")], na.rm = TRUE), 2, floor)
      max_woods_abs <- round_any(max(woods_plot_dat()[c("abs_diff_frac_exch", "abs_diff_theo_frac_exch")], na.rm = TRUE), 2, ceiling)
      
      updateSliderInput(session,
                        inputId = "woods_plot_y_range",
                        min = min_woods_abs - 2, 
                        max = max_woods_abs + 2, 
                        value = c(min_woods_abs, max_woods_abs),
                        step = 0.5)
      
    } else {
      
      updateSliderInput(session,
                        inputId = "comp_plot_y_range",
                        min = -200,
                        max = 200,
                        value = c(0, 120),
                        step = 10)
      
      updateSliderInput(session,
                        inputId = "woods_plot_y_range",
                        min = -200, 
                        max = 200, 
                        value = c(-50, 50),
                        step = 10)
    }
    
  })
  
  ##
  
  observe({
    
    updateTextInput(session, 
                    inputId = "comparison_plot_title",
                    value = case_when(
                      input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Theoretical fraction exchanged in state comparison in ", input[["chosen_time"]], " min for ", input[["chosen_protein"]]),
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Theoretical absolute value exchanged in state comparison in ", input[["chosen_time"]], " min for ", input[["chosen_protein"]]),
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Fraction exchanged in state comparison in ", input[["chosen_time"]], " min for ", input[["chosen_protein"]]),
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Absolute value exchanged in state comparison in ", input[["chosen_time"]], " min for ", input[["chosen_protein"]])
                    ))
    
    updateTextInput(session, 
                    inputId = "woods_plot_title",
                    value = case_when(
                      input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Delta Theoretical fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]]), " for ", input[["chosen_protein"]]),
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Delta Theoretical fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]]), " for ", input[["chosen_protein"]]),
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Delta Fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]]), " for ", input[["chosen_protein"]]),
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Delta Fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]]), " for ", input[["chosen_protein"]])
                    ))
    
    updateTextInput(session, 
                    inputId = "comparison_plot_y_label", 
                    value = case_when(
                      input[["theory"]] & input[["calc_type"]] == "relative" ~ "Theoretical fraction exchanged [%]",
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ "Theoretical absolute value exchanged [Da]",
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ "Fraction exchanged [%]",
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ "Absolute value exchanged [Da]"
                    ))
    
    updateTextInput(session, 
                    inputId = "woods_plot_y_label", 
                    value = case_when(
                      input[["theory"]] & input[["calc_type"]] == "relative" ~ "Delta Theoretical fraction exchanged between states [%]",
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ "Delta Theoretical absolute value exchanged between states [Da]",
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ "Delta Fraction exchanged between states [%]",
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ "Delta Absolute value exchanged between states [Da]"
                    ))
    
  })
  
  ##
  
  observe({
    
    if(input[["theory"]]){
      hide(id = "in_time_part")
      hide(id = "out_time_part")
    }
    
  })
  
  observe({
    
    if(!input[["theory"]]){
      show(id = "in_time_part")
      show(id = "out_time_part")
    }
    
  })
  
  ##
  
  observe({
    
    if(input[["calc_type"]] == "absolute"){
      hide(id = "out_time_part")
    } 
    
  })
  
  ##
  
  observe({
    
    if(input[["calc_type"]] == "relative" & !input[["theory"]]){
      show(id = "out_time_part")
    }
    
  })
  
  ##
  
  comparison_plot_colors <- reactive({
    
    hcl.colors(length(states_from_file()), palette = "Set 2", alpha = NULL, rev = FALSE, fixup = TRUE)
    
  })
  
  ##
  
  output[["states_colors"]] <- renderUI({
    
    # colorInput <- function(inputId, label, value = "", width = NULL, placeholder = NULL) {
    #   '%BAND%' <- function (x, y) {
    #     if (!is.null(x) && !is.na(x)) 
    #       if (!is.null(y) && !is.na(y)) 
    #         return(y)
    #     return(NULL)
    #   }
    #   value <- restoreInput(id = inputId, default = value)
    #   if(!is.null(value))
    #     div(class = "form-group shiny-input-container", 
    #         style = paste(if (value != "") paste0("background-color=: ", validateCssUnit(width), ";"),
    #                       if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";")
    #         ), 
    #         label %BAND%  
    #         tags$label(label, `for` = inputId), tags$input(id = inputId, 
    #                                                        type = "text", class = "form-control", value = value, 
    #                                                        placeholder = placeholder))
    # }
    
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
  
  ##
  
  ## COMPARISON PLOT + DATA
  
  ##
  
  all_dat <- reactive({
    
    if(!input[["theory"]]){
      validate(need(as.numeric(input[["in_time"]]) < as.numeric(input[["chosen_time"]]), "In time must be smaller than chosen time."))
      validate(need(as.numeric(input[["chosen_time"]]) < as.numeric(input[["out_time"]]), "Out time must be bigger than chosen time."))
    }

    bind_rows(lapply(states_from_file(), function(i) calculate_state_deuteration(dat(), 
                                                                                 protein = input[["chosen_protein"]], 
                                                                                 state = i, 
                                                                                 time_in = input[["in_time"]],
                                                                                 time_chosen = input[["chosen_time"]], 
                                                                                 time_out = input[["out_time"]],
                                                                                 deut_part = 0.01*as.integer(input[["deut_concentration"]]))))
  })
  
  ##
  
  prep_dat <- reactive({
    
    validate(need(input[["compare_states"]], "Please select at least one state."))
    
    filter(all_dat(), State %in% input[["compare_states"]])
    
  })
  
  
  ##
  
  comparison_plot_theo <- reactive({
    
    generate_comparison_plot(dat = prep_dat(),
                             theoretical = TRUE, 
                             relative = TRUE)
  })
  
  ##
  
  comparison_plot_theo_abs <- reactive({
    
    generate_comparison_plot(dat = prep_dat(),
                             theoretical = TRUE, 
                             relative = FALSE)
  })
  
  ##
  
  comparison_plot_exp <- reactive({
    
    generate_comparison_plot(dat = prep_dat(),
                             theoretical = FALSE, 
                             relative = TRUE)
  })
  
  ##
  
  comparison_plot_exp_abs <- reactive({
    
    generate_comparison_plot(dat = prep_dat(),
                             theoretical = FALSE, 
                             relative = FALSE)
  })
  
  ##
  
  cp_out <- reactive({
    
    comparison_plot_colors_chosen()
    
    if (input[["theory"]]) {
      
      if (input[["calc_type"]] == "relative") {
        
        cp <- comparison_plot_theo() 
        
      } else {
        
        cp <- comparison_plot_theo_abs() 
      }
      
    } else {
      
      if (input[["calc_type"]] == "relative") {
        
        cp <- comparison_plot_exp() 
        
      } else {
        
        cp <- comparison_plot_exp_abs()
        
      }
      
    }
    
    cp + coord_cartesian(xlim = c(input[["plot_x_range"]][[1]], input[["plot_x_range"]][[2]]),
                         ylim = c(input[["comp_plot_y_range"]][[1]], input[["comp_plot_y_range"]][[2]])) +
      labs(title = input[["comparison_plot_title"]], 
           x = input[["comparison_plot_x_label"]],
           y = input[["comparison_plot_y_label"]]) +
      scale_color_manual(values = comparison_plot_colors_chosen())
    
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
  
  ##
  
  comparison_plot_data_theo <- reactive({
    
    generate_comparison_data(dat = prep_dat(), 
                             theoretical = TRUE,
                             relative = TRUE)
  })
  
  ##
  
  comparison_plot_data_theo_abs <- reactive({
    
    generate_comparison_data(dat = prep_dat(), 
                             theoretical = TRUE,
                             relative = FALSE)
  })
  
  ## 
  
  comparison_plot_data_exp <- reactive({
    
    generate_comparison_data(dat = prep_dat(), 
                             theoretical = FALSE,
                             relative = TRUE)
  })
  
  ##
  
  comparison_plot_data_exp_abs <- reactive({
    
    generate_comparison_data(dat = prep_dat(), 
                             theoretical = FALSE,
                             relative = FALSE)
  })
  
  ##
  
  output[["comparisonPlot_data"]] <- DT::renderDataTable(server = FALSE, {
    
    if (input[["theory"]]) {
      
      if (input[["calc_type"]] == "relative") {
        cp_data <- comparison_plot_data_theo()  
      } else {
        cp_data <- comparison_plot_data_theo_abs()
      }
      
    } else {
      
      if (input[["calc_type"]] == "absolute") {
        cp_data <- comparison_plot_data_exp_abs()
      } else {
        cp_data <- comparison_plot_data_exp()
      }
      
    }
    
    cp_data %>%
      filter(Protein == input[["chosen_protein"]],
            Start >= input[["plot_x_range"]][[1]],
                End <= input[["plot_x_range"]][[2]]) %>%
      dt_format()
  })
  
  ##
  
  ## WOODS PLOT + DATA
  
  ##
  
  woods_plot_dat <- reactive({
    
    validate(need(input[["compare_states"]], "Please select at least one state."))
    validate(need(length(unique(filter(dat(), !is.na("Modification"), Protein == input[["chosen_protein"]])[["State"]])) > 1, "Not sufficient number of states without modifications."))
    
    if(!input[["theory"]]){
      validate(need(as.numeric(input[["in_time"]]) < as.numeric(input[["chosen_time"]]), "In time must be smaller than chosen time."))
      validate(need(as.numeric(input[["chosen_time"]]) < as.numeric(input[["out_time"]]), "Out time must be bigger than chosen time."))
    }
    
    tryCatch({
      generate_differential_data_set(dat = dat(),
                                    states = c(input[["state_first"]], input[["state_second"]]),
                                    protein = input[["chosen_protein"]],
                                    time_in = input[["in_time"]],
                                    time_chosen = input[["chosen_time"]],
                                    time_out = input[["out_time"]],
                                    deut_part = 0.01*as.integer(input[["deut_concentration"]]))
    },
    error = function(e){
      validate(need(FALSE), "Check chosen parameters - not sufficient data.")
    })
    
  })
  
  ##
  
  differential_plot_theo <- reactive({
    
    generate_differential_plot(dat = woods_plot_dat(),
                               theoretical = TRUE,
                               relative = TRUE,
                               confidence_limit = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  differential_plot_theo_abs <- reactive({
    
    generate_differential_plot(dat = woods_plot_dat(),
                               theoretical = TRUE,
                               relative = FALSE,
                               confidence_limit = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  differential_plot_exp <- reactive({
    
    generate_differential_plot(dat = woods_plot_dat(),
                               theoretical = FALSE,
                               relative = TRUE,
                               confidence_limit = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  differential_plot_exp_abs <- reactive({
    
    generate_differential_plot(dat = woods_plot_dat(),
                               theoretical = FALSE,
                               relative = FALSE,
                               confidence_limit = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  wp_out <- reactive({
    
    validate(need(!(input[["state_first"]] == input[["state_second"]]), "Please select two different states."))
    
    if (input[["theory"]]) {
      
      if (input[["calc_type"]] == "relative") {
        wp <- differential_plot_theo() 
      } else {
        wp <- differential_plot_theo_abs() 
      }
      
    } else {
      
      if (input[["calc_type"]] == "relative") {
        wp <- differential_plot_exp() 
      } else {
        wp <- differential_plot_exp_abs() 
      }
      
    }
    
    wp + coord_cartesian(xlim = c(input[["plot_x_range"]][[1]], input[["plot_x_range"]][[2]]),
                         ylim = c(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]])) +
      labs(title = input[["woods_plot_title"]],
           x = input[["woods_plot_x_label"]],
           y = input[["woods_plot_y_label"]])
    
  })
  
  ##
  
  output[["differentialPlot"]] <- renderPlot({
    
    wp_out()
    
  })
  
  ##
  
  output[["differentialPlot_debug"]] <- renderUI({
    
    if(!is.null(input[["differentialPlot_hover"]])) {
      
      wp_plot_data <- wp_out()[["data"]]
      wp_hv <- input[["differentialPlot_hover"]]
      
      wp_hv_dat <- data.frame(x = wp_hv[["x"]],
                              y = wp_hv[["y"]],
                              Start = wp_plot_data[[wp_hv[["mapping"]][["x"]]]],
                              End = wp_plot_data[["End"]],
                              y_plot = wp_plot_data[[wp_hv[["mapping"]][["y"]]]],
                              Sequence = wp_plot_data[["Sequence"]])
      
      wp_tt_df <- filter(wp_hv_dat, Start < x, End > x) %>% 
        filter(abs(y_plot - y) < 10) %>%
        filter(abs(y_plot - y) == min(abs(y_plot - y)))
      
      if(nrow(wp_tt_df) != 0) {
        
        wp_tt_pos_adj <- ifelse(wp_hv[["coords_img"]][["x"]]/wp_hv[["range"]][["right"]] < 0.5,
                                "left", "right")
        
        wp_tt_pos <- ifelse(wp_hv[["coords_img"]][["x"]]/wp_hv[["range"]][["right"]] < 0.5,
                            wp_hv[["coords_css"]][["x"]], 
                            wp_hv[["range"]][["right"]]/wp_hv[["img_css_ratio"]][["x"]] - wp_hv[["coords_css"]][["x"]])
        
        
        style <- paste0("position:absolute; z-index:1072; background-color: rgba(245, 245, 245, 1); pointer-events: none; ",
                        wp_tt_pos_adj, ":", wp_tt_pos, "px; padding: 0px;",
                        "top:", wp_hv[["coords_css"]][["y"]] , "px; ") 
        
        wellPanel(
          style = style,
          p(HTML(paste0(wp_tt_df[["Sequence"]],
                        "<br/> Position: ", wp_tt_df[["Start"]], "-", wp_tt_df[["End"]],
                        "<br/> Value: ", round(wp_tt_df[["y_plot"]], 2))))
        )
        
      }
    }
  })
  
  ##
  
  output[["differentialPlot_download_button"]] <- downloadHandler("differentialPlot.svg",
                                                                  content = function(file) {
                                                                    ggsave(file, wp_out(), device = svg,
                                                                           height = 300, width = 400, units = "mm")
                                                                  })
  
  ##
  
  differential_plot_data_theo <- reactive({
    
    generate_differential_data(dat = woods_plot_dat(),
                               theoretical = TRUE,
                               relative = TRUE,
                               confidence_limit_1 = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  differential_plot_data_theo_abs <- reactive({
    
    generate_differential_data(dat = woods_plot_dat(),
                               theoretical = TRUE,
                               relative = FALSE,
                               confidence_limit_1 = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  differential_plot_data_exp <- reactive({
    
    generate_differential_data(dat = woods_plot_dat(),
                               theoretical = FALSE,
                               relative = TRUE,
                               confidence_limit_1 = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  differential_plot_data_exp_abs <- reactive({
    
    generate_differential_data(dat = woods_plot_dat(),
                               theoretical = FALSE,
                               relative = FALSE,
                               confidence_limit_1 = as.double(input[["confidence_limit"]]),
                               confidence_limit_2 = as.double(input[["confidence_limit_2"]]))
  })
  
  ##
  
  output[["differentialPlot_data"]] <- DT::renderDataTable(server = FALSE, {
    
    if (input[["theory"]]) {
      
      if(input[["calc_type"]] == "relative") {
        dp_data <- differential_plot_data_theo()  
      } else {
        dp_data <- differential_plot_data_theo_abs()
      }
      
    } else {
      
      if (input[["calc_type"]] == "relative") {
        dp_data <- differential_plot_data_exp()  
      } else {
        dp_data <- differential_plot_data_exp_abs()
      }
      
    }
    
    dp_data %>%
      filter(Protein == input[["chosen_protein"]],
             Start >= input[["plot_x_range"]][[1]],
             End <= input[["plot_x_range"]][[2]]) %>%
      dt_format()
    
  })
  
  ##
  
  ### TAB: KINETICS ###
  
  ##
  
  observe({
    
    times_from_file <- round(unique(dat()[["Exposure"]]), 3)
    times_from_file <- times_from_file[order(times_from_file)]
    
    tmp <- sort(unique(round(dat()[["Exposure"]], 3)))
    choose_time_out <- setNames(tmp, c(head(tmp, -1), "chosen control"))
    
    updateSelectInput(session, 
                      inputId = "kin_in_time",
                      choices = times_from_file[times_from_file < 99999],
                      selected = min(times_from_file[times_from_file > 0]))
    
    if(!has_modifications()){
      
      updateSelectInput(session, 
                        inputId = "kin_out_time",
                        choices =  choose_time_out,
                        selected = choose_time_out["chosen control"])
    }
    
    if(has_modifications()){
      
      updateSelectInput(session, 
                        inputId = "kin_out_time",
                        choices =  times_from_file[times_from_file < 99999],
                        selected = max(times_from_file[times_from_file < 99999]))
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
                      input[["kin_theory"]] & input[["kin_calc_type"]] == "relative" ~ "Theoretical deuteration [%]",
                      input[["kin_theory"]] & input[["kin_calc_type"]] == "absolute" ~ "Theoretical deuteration [Da]",
                      !input[["kin_theory"]] & input[["kin_calc_type"]] == "relative" ~ "Deuteration [%]",
                      !input[["kin_theory"]] & input[["kin_calc_type"]] == "absolute" ~ "Deuteration [Da]"
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
    
    if(input[["kin_calc_type"]] == "absolute"){
      hide(id = "kin_out_time_part")
    } 
    
  })
  
  ##
  
  observe({
    
    if(input[["kin_calc_type"]] == "relative"){
      show(id = "kin_out_time_part")
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
  
  ##
  
  kin_dat <- reactive({
    
    validate(need(input[["peptide_list_data_rows_selected"]], "Please select at least one peptide from the table on the left."))
    
    times_from_file <- unique(round(dat()[["Exposure"]], 3))
    
    if(input[["kin_theory"]]){
      
      v_time_in <- min(times_from_file[times_from_file > 0])
      v_time_out <- max(times_from_file)
      
    } else {
      
      validate(need(as.numeric(input[["kin_out_time"]]) > as.numeric(input[["kin_in_time"]]), "Out time must be bigger than in time. "))
      
      validate(need(sum(times_from_file < as.numeric(input[["kin_out_time"]]) & times_from_file > as.numeric(input[["kin_in_time"]])) > 1, "Not enough time points between in and out time. "))
      
      v_time_in <- as.numeric(input[["kin_in_time"]])
      v_time_out <- as.numeric(input[["kin_out_time"]])
      
    }
    
    generate_kinetic_data_set(dat = dat(),
                              peptide_list = peptide_list()[input[["peptide_list_data_rows_selected"]], ],
                              protein = input[["chosen_protein"]],
                              deut_concentration = input[["deut_concentration"]],
                              time_in = v_time_in,
                              time_out = v_time_out)
  })
  
  ##
  
  observe({
    
    if (input[["kin_calc_type"]] == "absolute") {
      
      min_kin_abs <- round_any(min(kin_dat()[c("abs_frac_exch_state", "abs_avg_theo_in_time")], na.rm = TRUE), 5, floor)
      max_kin_abs <- round_any(max(kin_dat()[c("abs_frac_exch_state", "abs_avg_theo_in_time")], na.rm = TRUE), 5, ceiling)
      
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
  
  ##  KINETIC PLOT +  DATA
  
  ##
  
  kin_plot_theo <- reactive({
    
    generate_kinetic_plot(dat = kin_dat(),
                          theoretical = TRUE,
                          relative = TRUE)
  })
  
  ##
  
  kin_plot_theo_abs <- reactive({

    generate_kinetic_plot(dat = kin_dat(),
                          theoretical = TRUE,
                          relative = FALSE)
  })
  
  ##
  
  kin_plot_exp <- reactive({
    
    generate_kinetic_plot(dat = kin_dat(),
                          theoretical = FALSE,
                          relative = TRUE)
  })
  
  ##
  
  kin_plot_exp_abs <- reactive({
    
    generate_kinetic_plot(dat = kin_dat(),
                          theoretical = FALSE,
                          relative = FALSE)
  })
  
  ##
  
  kp_out <- reactive({
    
    if (input[["kin_theory"]]) {
      
      if (input[["kin_calc_type"]] == "relative") {
        
        kp <- kin_plot_theo()
        
      } else {
        
        kp <- kin_plot_theo_abs()
        
      }
      
    } else {
      
      if (input[["kin_calc_type"]] == "relative"){
        
        kp <- kin_plot_exp()
        
      } else {
        
        kp <- kin_plot_exp_abs()
        
      }
      
    }
    
    kp + 
      geom_point(size = 3) +
      labs(title = input[["kin_plot_title"]],
           x = input[["kin_plot_x_label"]],
           y = input[["kin_plot_y_label"]]) +
      coord_cartesian(ylim = c(input[["kin_plot_y_range"]][1], input[["kin_plot_y_range"]][2])) +
      scale_x_log10() + 
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
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
  ##
  
  kin_plot_theo_data <- reactive({
    
    generate_kinetic_data(dat = kin_dat(),
                          theoretical = TRUE, 
                          relative = TRUE)
  })
  
  ##
  
  kin_plot_theo_abs_data <- reactive({
    
    generate_kinetic_data(dat = kin_dat(),
                          theoretical = TRUE, 
                          relative = FALSE)
  })
  
  ##
  
  kin_plot_exp_data <- reactive({
    
    generate_kinetic_data(dat = kin_dat(),
                          theoretical = FALSE, 
                          relative = TRUE)
  })
  
  ##
  
  kin_plot_exp_abs_data <- reactive({
    
    generate_kinetic_data(dat = kin_dat(),
                          theoretical = FALSE, 
                          relative = FALSE)
  })
  
  ##
  
  output[["kin_plot_data"]] <- DT::renderDataTable(server = FALSE, {
    
    if (input[["kin_theory"]]) {
      
      if (input[["kin_calc_type"]] == "relative") {
        kp_data <- kin_plot_theo_data()
      } else {
        kp_data <- kin_plot_theo_abs_data()
      } 
      
    } else {
      
      if (input[["kin_calc_type"]] == "relative") {
        kp_data <- kin_plot_exp_data()
      } else {
        kp_data <- kin_plot_exp_abs_data()
      } 
      
    }
    
    kp_data %>%
      dt_format()
    
  })
  
  ### TAB: QUALITY CONTROL  
  
  ##
  
  ## TODO: propagate it!
  
  times_from_file <- reactive({
    
    times_from_file <- round(unique(dat()[["Exposure"]]), 3)
    times_from_file[order(times_from_file)]
    
  })
  
  
  observe({
    
    updateSelectInput(session, 
                      inputId = "qc_chosen_time",
                      choices = times_from_file()[times_from_file() < 99999],
                      selected = min(times_from_file()[times_from_file() > input[["qc_in_time"]]]))
  })
  
  observe({
    
    updateSelectInput(session, 
                      inputId = "qc_in_time",
                      choices = times_from_file()[times_from_file() < 99999],
                      selected = min(times_from_file()[times_from_file() > 0]))
    
    updateSelectInput(session,
                      inputId = "qc_state_first",
                      choices = states_from_file(),
                      selected = states_from_file()[1])
    
    updateSelectInput(session,
                      inputId = "qc_state_second",
                      choices = states_from_file(),
                      selected = states_from_file()[length(states_from_file())])
    
  })
  
  ##
  
  quality_control_dat <- reactive({
    
    qc_dat <- dat() %>%
      filter(Exposure < 99999)
    
    validate(need(as.numeric(input[["qc_chosen_time"]]) > as.numeric(input[["qc_in_time"]]), "Chosen time must be bigger than in time. "))
    validate(need(sum(unique(qc_dat[["Exposure"]]) > as.numeric(input[["qc_chosen_time"]])) > 1, "Not enough time points (bigger than chosen time) to generate a plot. ")) 
    
    result <- quality_control(dat = qc_dat,
                              state_first = input[["qc_state_first"]],
                              state_second = input[["qc_state_second"]], 
                              chosen_time = as.numeric(input[["qc_chosen_time"]]), 
                              in_time = as.numeric(input[["qc_in_time"]])) %>%
      # to get the percentages in readable form
      mutate(avg_err_state_first = 100 * avg_err_state_first,
             sd_err_state_first = 100 * sd_err_state_first,
             avg_err_state_second = 100 * avg_err_state_second,
             sd_err_state_second = 100 * sd_err_state_second, 
             avg_diff = 100 * avg_diff, 
             sd_diff = 100 * sd_diff)
    
    
  })
  
  ##
  
  qc_out <- reactive({
    
    generate_quality_control_plot(dat = quality_control_dat())
    
  })
  
  output[["quality_control_plot"]] <- renderPlot({
    
    qc_out()
    
  })
  
  ##
  
  quality_control_plot_data_out <- reactive({
    
    generate_quality_control_data(dat = quality_control_dat()) %>%
      dt_format()
    
  })
  
  ##
  
  output[["quality_control_plot_data"]] <- DT::renderDataTable({
    
    quality_control_plot_data_out()
    
  })
  
  ##
  
  output[["quality_control_plot_debug"]] <- renderUI({
    
    if(!is.null(input[["quality_control_plot_hover"]])) {
      
      plot_data <- qc_out()[["data"]]
      hv <- input[["quality_control_plot_hover"]]
      
      hv_dat <- data.frame(x = hv[["x"]],
                           y = hv[["y"]],
                           x_plot = plot_data[[hv[["mapping"]][["x"]]]],
                           y_plot = plot_data[[hv[["mapping"]][["y"]]]])
      
      tt_df <- hv_dat %>%
        filter(abs(y_plot - y) == min(abs(y_plot - y)), abs(x_plot - x) < 0.1*x_plot) 
      
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
          p(HTML(paste0("<br/> x: ", round(tt_df[["x_plot"]], 0), " [min]",
                        "<br/> y: ", round(tt_df[["y_plot"]], 2), " [%] ")))
        )
      }
    }
  })
  
  ##
  
  ##
  
  output[["quality_control_plot_download_button"]] <- downloadHandler("qualityControlPlot.svg",
                                                                      content = function(file){
                                                                        ggsave(file, qc_out(), device = svg,
                                                                               height = 300, width = 400, units = "mm")
                                                                      })
  
  ### TAB: SUMMARY
  
  summary_data <- reactive({
    
    generate_summary_table(dat = dat(),
                           confidence_limit_1 = input[["confidence_limit"]],
                           confidence_limit_2 = input[["confidence_limit_2"]],
                           overlap_distribution_data = stateOverlapDist_data())
  })
  
  
  output[["summary_table"]] <- DT::renderDataTable(server = FALSE, {
    
    datatable(data = summary_data(),
              class = "table-bordered table-condensed",
              extensions = "Buttons",
              options = list(pageLength = 10, dom = "tBi", autoWidth = TRUE, buttons = c("excel", "pdf")),
              filter = "bottom",
              rownames = FALSE)
    
  })
  
  ### TAB: REPORT ###
  
  ##
  
  output[["export_action"]] <- downloadHandler(
    
    filename <- "HaDeX_Report.html",
    
    content <- function(file) {
      
      rmarkdown::render(input = "report_template.Rmd", 
                        output_file = file, quiet = TRUE)
      
    })
  
  ##
  
}

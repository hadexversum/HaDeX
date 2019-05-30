source("ui.R")

#########################################

server <- function(input, output, session) {
  
  ##
  
  ### TAB: START ###
  
  ##
  
  output[["file_req"]] <- renderTable({
    
    file_req
    
  })
  
  dat <- reactive({
    
    inFile <- input[["data_file"]]
    
    if (is.null(inFile)){
      read_hdx('./data/KD_180110_CD160_HVEM.csv')
    } else {
      validate(need(try(read_hdx(inFile[["datapath"]])), "Check file requirements!"))
      read_hdx(inFile[["datapath"]])
    }
    
  })
  
  output[["data_file_info"]] <- renderText({
    
    if (is.null(input[["data_file"]])){
      "Example file: KD_180110_CD160_HVEM.csv"
    } else {
      length(dat()[[1]])
      "Supplied file is valid."
    }
    
  })
  
  ##
  
  ### TAB: SEQUENCE DATA ###
  
  ##
  
  observe({
    
    possible_states <- unique(dat()[["State"]])
    
    updateRadioButtons(session,
                       inputId = "chosen_state",
                       choices = possible_states)
    
    updateNumericInput(session, 
                       inputId = "sequence_length",
                       value = max_range())
    
  })
  
  ##
  
  output[["protein_name"]] <- renderText({
    
    as.character(unique(dat()[["Protein"]]))
    
  })
  
  ##
  
  position_in_sequence_tmp <- reactive({
    
    dat() %>%
      select(Start, End, Sequence) %>%
      unique(.) %>%
      apply(1, function(x) data.frame(position = x[1]:x[2], amino = strsplit(x[3], '')[[1]], stringsAsFactors = FALSE)) %>%
      bind_rows() %>%
      unique(.) 
    
  })
  
  ##
  
  protein_sequence <- reactive({
    
    reconstruct_sequence(dat())
    
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
  
  protein_sequece_colored <- reactive({
    
    paste0("<span>", 
           gsubfn(pattern = 'C', replacement = function(x) paste0('<font color = "red">', x, "</font>"), x = protein_sequence()),
           "</span>")
    
  })
  
  ##
  
  output[["sequenceName"]] <- renderText({
    
    protein_sequece_colored()
    
  })
  
  ##
  
  aminoDist_out <- reactive({
    
    charge_colors <- c("-1" = "#E41A1C", "0" = "#377EB8", "1" = "#4DAF4A")
    
    position_in_sequence() %>%
      mutate(affinity = ifelse(is_hydrophobic, "phobic", "philic")) %>% 
      filter(affinity %in% input[["hydro_prop"]]) %>%
      mutate(amino = factor(amino, levels = amino_groups)) %>%
      ggplot(aes(x = amino, fill = charge)) + 
      geom_bar() +
      scale_fill_manual("Charge", values = charge_colors) + 
      labs(title = 'Amino acid composition',
           x = 'Amino acid',
           y = 'Count')
    
  })
  
  ##
  
  output[["aminoDist"]] <- renderPlot({
    
    aminoDist_out()
    
  })
  
  ##
  
  output[["aminoDist_download_button"]] <- downloadHandler("aminoDist.svg",
                                                           content = function(file){
                                                             ggsave(file, aminoDist_out(), device = svg, 
                                                                    height = 300, width = 400, units = "mm")
                                                           })
  ##
  
  ### TAB: OVERLAPPING ###
  
  ##
  
  stateOverlap_data <- reactive({
    
    dat() %>%
      select(Sequence, Start, End, State) %>% 
      filter(State == input[["chosen_state"]]) %>%
      filter(Start >= input[["plot_range"]][[1]], End <= input[["plot_range"]][[2]]) %>%
      filter(!duplicated(.)) %>%
      select(-State) %>%
      dt_format(cols = c("Sequence", "Start", "End"))
    
  })
  
  output[["stateOverlap_data"]] <- DT::renderDataTable({
    
    stateOverlap_data()
    
  })
  
  ##
  
  stateOverlap <- reactive({
    
    graphic_overlapping(dat = dat(),
                        chosen_state = input[["chosen_state"]])
    
  })
  
  ##
  
  stateOverlap_out <- reactive({
    
    stateOverlap() + 
      coord_cartesian(xlim = c(input[["plot_range"]][[1]], input[["plot_range"]][[2]]))
    
  })
  
  ##
  
  output[["stateOverlap"]] <- renderPlot({
    
    stateOverlap_out()
    
  })
  
  ##
  
  output[["stateOverlap_download_button"]] <- downloadHandler("stateOverlap.svg",
                                                              content = function(file){
                                                                ggsave(file, stateOverlap_out(), device = svg, 
                                                                       height = 300, width = 400, units = "mm")
                                                              })

  ##
  
  stateOverlapDist_data <- reactive({
    
    dat() %>%
      select(Start, End, State, Sequence) %>%
      filter(State == input[["chosen_state"]]) %>%
      filter(Start >= input[["plot_range"]][[1]], End <= input[["plot_range"]][[2]]) %>%
      filter(!duplicated(.)) %>%
      select(-State) %>%
      apply(1, function(i) i[1]:i[2]) %>%
      unlist %>%
      data.frame(pos = .) %>%
      group_by(pos) %>%
      summarise(coverage = length(pos)) %>%
      right_join(data.frame(pos = seq(from = input[["plot_range"]][[1]], to = input[["plot_range"]][[2]]))) %>%
      replace_na(list(coverage = 0)) %>%
      dt_format(cols = c("Position", "Times Covered"))
    
  })
  
  ##
  
  output[["stateOverlapDist_data"]] <- DT::renderDataTable({
    
    stateOverlapDist_data()
    
  })
  
  ##
  
  stateOverlapDist <- reactive({
    
    tmp <- dat() %>%
      select(Start, End, State) %>% 
      filter(State == input[["chosen_state"]]) %>% 
      filter(Start >= input[["plot_range"]][[1]], End <= input[["plot_range"]][[2]]) %>%
      filter(!duplicated(.)) %>% 
      select(-State) %>% 
      apply(1, function(i) i[1]:i[2]) %>% 
      unlist %>% 
      data.frame(x = .) %>% 
      group_by(x) %>% 
      summarise(coverage = length(x)) 
    
    mean_coverage <- round(mean(tmp[["coverage"]], na.rm = TRUE), 2)
    
    display_position <- (input[["plot_range"]][[1]] + input[["plot_range"]][[2]])/2
    
    tmp %>%
      ggplot(aes(x = x, y = coverage)) +
      geom_col(width = 1) +
      geom_hline(yintercept = mean_coverage, linetype = 'dashed', color = 'red') +
      geom_text(aes(x = display_position, y = mean_coverage, label = 'Average', color = 'red', vjust = -.5)) +
      geom_text(aes(x = display_position, y = mean_coverage, label = mean_coverage, color = 'red', vjust = 1.5)) +
      labs(#title = 'How much a position in sequence is covered?',
        x = 'Position in sequence',
        y = 'Position frequency in peptide') +
      theme(legend.position = "none")
    
  })
  
  ##
  
  output[["stateOverlapDist"]] <- renderPlot({
    
    stateOverlapDist()
    
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
  
  max_range <- reactive({
    
    max(dat()[['End']])
    
  })
  
  ##
  
  output[["protein_length"]] <- renderText({
    
    max_range()
    
  })
  
  ##
  
  states_from_file <- reactive({
    
    unique(dat()[["State"]])
    
  })
  
  ##
  
  observe({
    
    times_from_file <- round(unique(dat()["Exposure"]), 3)
    
    updateSelectInput(session, 
                      inputId = "chosen_time",
                      choices = times_from_file,
                      selected = min(times_from_file[times_from_file["Exposure"] >= 1, ]))
    
    updateSelectInput(session, 
                      inputId = "in_time",
                      choices = times_from_file,
                      selected = min(times_from_file[times_from_file["Exposure"] > 0, ]))
    
    updateSelectInput(session, 
                      inputId = "out_time",
                      choices = times_from_file,
                      selected = max(times_from_file))
    
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
                        min = -2,
                        max = 2,
                        value = c(0, 1.2),
                        step = 0.1)
      
      updateSliderInput(session,
                        inputId = "woods_plot_y_range",
                        min = -2, 
                        max = 2, 
                        value = c(-.5, .5),
                        step = 0.1)
    }
    
  })
  
  ##
  
  observe({
    
    updateTextInput(session, 
                    inputId = "comparison_plot_title",
                    value = case_when(
                      input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Theoretical fraction exchanged in state comparison in ", input[["chosen_time"]], " min"),
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Theoretical absolute value exchanged in state comparison in ", input[["chosen_time"]], " min"),
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Fraction exchanged in state comparison in ", input[["chosen_time"]], " min"),
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Absolute value exchanged in state comparison in ", input[["chosen_time"]], " min")
                    ))
    
    updateTextInput(session, 
                    inputId = "woods_plot_title",
                    value = case_when(
                      input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Delta Theoretical fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]])),
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Delta Theoretical fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]])),
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ paste0("Delta Fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]])),
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ paste0("Delta Fraction exchanged in ", input[["chosen_time"]], " min between ", gsub("_", " ", input[["state_first"]]), " and ", gsub("_", " ", input[["state_second"]]))
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
                      input[["theory"]] & input[["calc_type"]] == "absolute" ~ "Delta Theoretical absoute value exchanged between states [Da]",
                      !input[["theory"]] & input[["calc_type"]] == "relative" ~ "Delta Fraction exchanged between states [%]",
                      !input[["theory"]] & input[["calc_type"]] == "absolute" ~ "Delta Absolute value exchanged between states [Da]"
                    ))
    
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
      
      bind_rows(lapply(states_from_file(), function(i) calculate_state_deuteration(dat(), 
                                                                                   protein = dat()[["Protein"]][1], 
                                                                                   state = i, 
                                                                                   time_in = input[["in_time"]],
                                                                                   time_chosen = input[["chosen_time"]], 
                                                                                   time_out = input[["out_time"]])))
    })
    
    ##
    
    prep_dat <- reactive({
      
      validate(need(input[["compare_states"]], "Please select at least one state."))
      
      filter(all_dat(), State %in% input[["compare_states"]])
      
    })
    
    
    ##
    
    comparison_plot_theo <- reactive({
      
      ggplot() +
        geom_segment(data = prep_dat(), aes(x = Start, y = avg_theo_in_time, xend = End, yend = avg_theo_in_time, color = State)) +
        geom_errorbar(data = prep_dat(), aes(x = Med_Sequence, ymin = avg_theo_in_time - err_avg_theo_in_time, ymax = avg_theo_in_time + err_avg_theo_in_time, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(breaks = seq(-2, 2, 0.2), expand = c(0, 0))
      
    })
    
    ##
    
    comparison_plot_theo_abs <- reactive({
      
      ggplot() +
        geom_segment(data = prep_dat(), aes(x = Start, y = abs_avg_theo_in_time, xend = End, yend = abs_avg_theo_in_time, color = State)) +
        geom_errorbar(data = prep_dat(), aes(x = Med_Sequence, ymin = abs_avg_theo_in_time - err_abs_avg_theo_in_time, ymax = abs_avg_theo_in_time + err_abs_avg_theo_in_time, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
      
    })
    
    ##
    
    comparison_plot_exp <- reactive({
      
      ggplot() +
        geom_segment(data = prep_dat(), aes(x = Start, y = frac_exch_state, xend = End, yend = frac_exch_state, color = State)) +
        geom_errorbar(data = prep_dat(), aes(x = Med_Sequence, ymin = frac_exch_state - err_frac_exch_state, ymax = frac_exch_state + err_frac_exch_state, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(breaks = seq(-2, 2, 0.2), expand = c(0, 0))
      
    })
    
    ##
    
    comparison_plot_exp_abs <- reactive({
      
      ggplot() +
        geom_segment(data = prep_dat(), aes(x = Start, y = abs_frac_exch_state, xend = End, yend = abs_frac_exch_state, color = State)) +
        geom_errorbar(data = prep_dat(), aes(x = Med_Sequence, ymin = abs_frac_exch_state - err_abs_frac_exch_state, ymax = abs_frac_exch_state + err_abs_frac_exch_state, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
      
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
    
    output[["comparisonPlot_download_button"]] <- downloadHandler("comparisonPlot.svg",
                                                                   content = function(file) {
                                                                      ggsave(file, cp_out(), device = svg,
                                                                             height = 300, width = 400, units = "mm")
                                                                    })

    ##
    
    comparison_plot_data_theo <- reactive({
      
      prep_dat() %>%
        select(Sequence, State, Start, End, avg_theo_in_time, err_avg_theo_in_time) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(avg_theo_in_time = round(avg_theo_in_time, 4),
               err_avg_theo_in_time = round(err_avg_theo_in_time, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = c("Sequence", "State", "Start", "End", "Theo Frac Exch", "Err Theo Frac Exch"))
      
    })
    
    ##
    
    comparison_plot_data_theo_abs <- reactive({
      
      prep_dat() %>%
        select(Sequence, State, Start, End, abs_avg_theo_in_time, err_abs_avg_theo_in_time) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(abs_avg_theo_in_time = round(abs_avg_theo_in_time, 4),
               err_abs_avg_theo_in_time = round(abs_avg_theo_in_time, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = c("Sequence", "State", "Start", "End", "Theo Abs Val Exch", "Err Theo Abs Val Exch"))
    })
    
    ## 
    
    comparison_plot_data_exp <- reactive({
      
      prep_dat() %>%
        select(Sequence, State, Start, End, frac_exch_state, err_frac_exch_state) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(frac_exch_state = round(frac_exch_state, 4),
               err_frac_exch_state = round(err_frac_exch_state, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = c("Sequence", "State", "Start", "End", "Frac Exch", "Err Frac Exch"))
      
    })
    
    ##
    
    comparison_plot_data_exp_abs <- reactive({
      
      prep_dat() %>%
        select(Sequence, State, Start, End, abs_frac_exch_state, err_abs_frac_exch_state) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(abs_frac_exch_state = round(abs_frac_exch_state, 4),
               err_abs_frac_exch_state = round(abs_frac_exch_state, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = c("Sequence", "State", "Start", "End", "Abs Val Exch", "Err Abs Val Exch"))
      
    })
    
    ##
    
    output[["comparisonPlot_data"]] <- DT::renderDataTable({
      
      if (input[["theory"]]) {
        
        if (input[["calc_type"]] == "relative") {
          comparison_plot_data_theo()  
        } else {
          comparison_plot_data_theo_abs()
        }
        
      } else {
        
        if (input[["calc_type"]] == "absolute") {
          comparison_plot_data_exp_abs()
        } else {
          comparison_plot_data_exp()
        }
        
      }
      
    })
    
    ##
    
    ## WOODS PLOT + DATA
    
    ##
    
    woods_plot_dat <- reactive({
      
      validate(need(input[["compare_states"]], "Please select at least one state."))
      
      bind_rows(lapply(c(input[["state_first"]], input[["state_second"]]), function(i) calculate_state_deuteration(dat(), 
                                                                                                                   protein = dat()[["Protein"]][1], 
                                                                                                                   state = i, 
                                                                                                                   time_in = input[["in_time"]],
                                                                                                                   time_chosen = input[["chosen_time"]], 
                                                                                                                   time_out = input[["out_time"]]))) %>%
        droplevels() %>% 
        mutate(State = factor(State, levels = c(input[["state_first"]], input[["state_second"]]), labels = c("1", "2"))) %>%
        gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
        unite(tmp, variable, State) %>%
        spread(tmp, value) %>%
        mutate(diff_frac_exch = frac_exch_state_1 - frac_exch_state_2,
               err_frac_exch = sqrt(err_frac_exch_state_1^2 + err_frac_exch_state_2^2),
               abs_diff_frac_exch = abs_frac_exch_state_1 - abs_frac_exch_state_2,
               err_abs_diff_frac_exch = sqrt(err_abs_frac_exch_state_1^2 + err_abs_frac_exch_state_2^2),
               diff_theo_frac_exch = avg_theo_in_time_1 - avg_theo_in_time_2, 
               err_diff_theo_frac_exch = sqrt(err_avg_theo_in_time_1^2 + err_avg_theo_in_time_2^2),
               abs_diff_theo_frac_exch = abs_avg_theo_in_time_1 - abs_avg_theo_in_time_2,
               err_abs_diff_theo_frac_exch = sqrt(err_abs_avg_theo_in_time_1^2 + err_abs_avg_theo_in_time_2^2)) %>%
        select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2"))
      
    })
    
    ##
    
    differential_plot_theo <- reactive({
      
      confidence_limit <- as.double(input[["confidence_limit"]])
      confidence_limit_2 <- as.double(input[["confidence_limit_2"]])
      
      interval <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    relative = TRUE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      relative = TRUE)
      
      mutate(woods_plot_dat(), colour = case_when(
        woods_plot_dat()[["diff_theo_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        woods_plot_dat()[["diff_theo_frac_exch"]] < interval[1] ~ "deepskyblue1",
        woods_plot_dat()[["diff_theo_frac_exch"]] > interval_2[2] ~ "firebrick3",
        woods_plot_dat()[["diff_theo_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_diff_theo_frac_exch, ymax = diff_theo_frac_exch + err_diff_theo_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
    })
    
    ##
    
    differential_plot_theo_abs <- reactive({
      
      confidence_limit <- as.double(input[["confidence_limit"]])
      confidence_limit_2 <- as.double(input[["confidence_limit_2"]])
      
      interval <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    relative = FALSE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      relative = FALSE)
      
      mutate(woods_plot_dat(), colour = case_when(
        woods_plot_dat()[["abs_diff_theo_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        woods_plot_dat()[["abs_diff_theo_frac_exch"]] < interval[1] ~ "deepskyblue1",
        woods_plot_dat()[["abs_diff_theo_frac_exch"]] > interval_2[2] ~ "firebrick3",
        woods_plot_dat()[["abs_diff_theo_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = abs_diff_theo_frac_exch, xend = End, yend = abs_diff_theo_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = abs_diff_theo_frac_exch - err_abs_diff_theo_frac_exch, ymax = abs_diff_theo_frac_exch + err_abs_diff_theo_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
    })
    
    ##
    
    differential_plot_exp <- reactive({
      
      confidence_limit <- as.double(input[["confidence_limit"]])
      confidence_limit_2 <- as.double(input[["confidence_limit_2"]])
      
      interval <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    relative = TRUE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      relative = TRUE)
      
      mutate(woods_plot_dat(), colour = case_when(
        woods_plot_dat()[["diff_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        woods_plot_dat()[["diff_frac_exch"]] < interval[1] ~ "deepskyblue1",
        woods_plot_dat()[["diff_frac_exch"]] > interval_2[2] ~ "firebrick3",
        woods_plot_dat()[["diff_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
    })
    
    ##
    
    differential_plot_exp_abs <- reactive({
      
      confidence_limit <- as.double(input[["confidence_limit"]])
      confidence_limit_2 <- as.double(input[["confidence_limit_2"]])
      
      interval <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    relative = FALSE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = woods_plot_dat(),
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      relative = FALSE)
      
      mutate(woods_plot_dat(), colour = case_when(
        woods_plot_dat()[["abs_diff_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        woods_plot_dat()[["abs_diff_frac_exch"]] < interval[1] ~ "deepskyblue1",
        woods_plot_dat()[["abs_diff_frac_exch"]] > interval_2[2] ~ "firebrick3",
        woods_plot_dat()[["abs_diff_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = abs_diff_frac_exch, xend = End, yend = abs_diff_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = abs_diff_frac_exch - err_abs_diff_frac_exch, ymax = abs_diff_frac_exch + err_abs_diff_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
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
    
    output[["differentialPlot_download_button"]] <- downloadHandler("differentialPlot.svg",
                                                                    content = function(file) {
                                                                      ggsave(file, wp_out(), device = svg,
                                                                             height = 300, width = 400, units = "mm")
                                                                    })
    
    ##
    
    differential_plot_data_theo <- reactive({
      
      woods_plot_dat() %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit"]]),
                            theoretical = TRUE, 
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit_2"]]),
                            theoretical = TRUE, 
                            relative = TRUE) %>%
        select(Sequence, Start, End, diff_theo_frac_exch, err_diff_theo_frac_exch, paste0("valid_at_", input[["confidence_limit"]]), paste0("valid_at_", input[["confidence_limit_2"]])) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(diff_theo_frac_exch = round(diff_theo_frac_exch, 4),
               err_diff_theo_frac_exch = round(err_diff_theo_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = unique(c("Sequence", "Start", "End", "Theo Diff Frac Exch", "Err Theo Diff Frac Exch", paste0("Valid At ", input[["confidence_limit"]]), paste0("Valid At ", input[["confidence_limit_2"]]))))
      
    })
    
    ##
    
    differential_plot_data_theo_abs <- reactive({
      
      woods_plot_dat() %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit"]]),
                            theoretical = TRUE, 
                            relative = FALSE) %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit_2"]]),
                            theoretical = TRUE, 
                            relative = FALSE) %>%
        select(Sequence, Start, End, abs_diff_theo_frac_exch, err_abs_diff_theo_frac_exch, paste0("valid_at_", input[["confidence_limit"]]), paste0("valid_at_", input[["confidence_limit_2"]])) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(abs_diff_theo_frac_exch = round(abs_diff_theo_frac_exch, 4),
               err_abs_diff_theo_frac_exch = round(err_abs_diff_theo_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = unique(c("Sequence", "Start", "End", "Theo Abs Value Diff", "Err Theo Abs Value Diff", paste0("Valid At ", input[["confidence_limit"]]), paste0("Valid At ", input[["confidence_limit_2"]]))))
      
    })
    
    ##
    
    differential_plot_data_exp <- reactive({
      
      woods_plot_dat() %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit"]]),
                            theoretical = FALSE, 
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit_2"]]),
                            theoretical = FALSE, 
                            relative = TRUE) %>%
        select(Sequence, Start, End, diff_frac_exch, err_frac_exch, paste0("valid_at_", input[["confidence_limit"]]), paste0("valid_at_", input[["confidence_limit_2"]])) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(diff_frac_exch = round(diff_frac_exch, 4),
               err_frac_exch = round(err_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = unique(c("Sequence", "Start", "End", "Diff Frac Exch", "Err Diff Frac Exch", paste0("Valid At ", input[["confidence_limit"]]), paste0("Valid At ", input[["confidence_limit_2"]]))))
      
    })
    
    ##
    
    differential_plot_data_exp_abs <- reactive({
      
      woods_plot_dat() %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit"]]),
                            theoretical = FALSE,
                            relative = FALSE) %>%
        add_stat_dependency(confidence_limit = as.double(input[["confidence_limit_2"]]),
                            theoretical = FALSE, 
                            relative = FALSE) %>%
        select(Sequence, Start, End, abs_diff_frac_exch, err_abs_diff_frac_exch, paste0("valid_at_", input[["confidence_limit"]]), paste0("valid_at_", input[["confidence_limit_2"]])) %>%
        filter(Start >= input[["plot_x_range"]][[1]],
               End <= input[["plot_x_range"]][[2]]) %>%
        mutate(abs_diff_frac_exch = round(abs_diff_frac_exch, 4),
               err_abs_diff_frac_exch = round(err_abs_diff_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        dt_format(cols = unique(c("Sequence", "Start", "End", "Diff Abs Value Exch", "Err Diff Abs Value Exch", paste0("Valid At ", input[["confidence_limit"]]), paste0("Valid At ", input[["confidence_limit_2"]]))))
    })
    
    ##
    
    output[["differentialPlot_data"]] <- DT::renderDataTable({
      
      if (input[["theory"]]) {
        
        if(input[["calc_type"]] == "relative") {
          differential_plot_data_theo()  
        } else {
          differential_plot_data_theo_abs()
        }
        
      } else {
        
        if (input[["calc_type"]] == "relative") {
          differential_plot_data_exp()  
        } else {
          differential_plot_data_exp_abs()
        }
        
      }
      
    })
    
    ##
    
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

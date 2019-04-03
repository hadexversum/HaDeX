source("ui.R")

#########################################

server <- function(input, output, session) {
  
  output[["file_req"]] <- renderTable({
    
    file_req
    
  })
  
  dat <- reactive({
    
    inFile <- input[["data_file"]]
    
    if (is.null(inFile)){
      read_hdx('./data/KD_180110_CD160_HVEM.csv')
    } else {
      read_hdx(inFile[["datapath"]])
    }
    
  })
  
  ##
  
  observe({
    
    times_from_file <- round(unique(dat()["Exposure"]), 3)
    
    states_from_file <- unique(dat()[["State"]])
    
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
                      choices = states_from_file,
                      selected = states_from_file[1])
    
    updateSelectInput(session,
                      inputId = "state_second",
                      choices = states_from_file,
                      selected = states_from_file[length(states_from_file)])
    
  })
  
  ##
  # TODO : change name for more informative one
  dat_new <- reactive({

    prepare_dataset(dat = dat(),
                    in_state_first = paste0(input[["state_first"]], "_", input[["in_time"]]),
                    chosen_state_first = paste0(input[["state_first"]], "_", input[["chosen_time"]]),
                    out_state_first = paste0(input[["state_first"]], "_", input[["out_time"]]),
                    in_state_second = paste0(input[["state_second"]], "_", input[["in_time"]]),
                    chosen_state_second = paste0(input[["state_second"]], "_", input[["chosen_time"]]),
                    out_state_second = paste0(input[["state_second"]], "_", input[["out_time"]]))
    
  })
  
  ##
  
  comparison_plot_data_theo <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, avg_theo_in_time_1, err_avg_theo_in_time_1, avg_theo_in_time_2, err_avg_theo_in_time_2) %>%
      mutate(avg_theo_in_time_1 = round(avg_theo_in_time_1, 4),
             err_avg_theo_in_time_1 = round(err_avg_theo_in_time_1, 4),
             avg_theo_in_time_2 = round(avg_theo_in_time_2, 4),
             err_avg_theo_in_time_2 = round(err_avg_theo_in_time_2, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Theo Frac Exch 1", "Err Theo Frac Exch 1", "Theo Frac Exch 2", "Err Theo Frac Exch 2"))
    
  })
  
  ##
  
  comparison_plot_data_theo_abs <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, abs_avg_theo_in_time_1, err_abs_avg_theo_in_time_1, abs_avg_theo_in_time_2, err_abs_avg_theo_in_time_2) %>%
      mutate(abs_avg_theo_in_time_1 = round(abs_avg_theo_in_time_1, 4),
             err_abs_avg_theo_in_time_1 = round(err_abs_avg_theo_in_time_1, 4),
             abs_avg_theo_in_time_2 = round(abs_avg_theo_in_time_2, 4),
             err_abs_avg_theo_in_time_2 = round(abs_avg_theo_in_time_2, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Theo Abs Val Exch 1", "Err Theo Abs Val Exch 1", "Theo Abs Val Exch 2", "Err Theo Abs Val Exch 2"))
  })
  
  ## 
  
  comparison_plot_data_exp <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, frac_exch_state_1, err_frac_exch_state_1, frac_exch_state_2, err_frac_exch_state_2) %>%
      mutate(frac_exch_state_1 = round(frac_exch_state_1, 4),
             err_frac_exch_state_1 = round(err_frac_exch_state_1, 4),
             frac_exch_state_2 = round(frac_exch_state_2, 4),
             err_frac_exch_state_2 = round(err_frac_exch_state_2, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Frac Exch 1", "Err Frac Exch 1", "Frac Exch 2", "Err Frac Exch 2"))
    
  })
  
  ##
  
  comparison_plot_data_exp_abs <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, abs_frac_exch_state_1, err_abs_frac_exch_state_1, abs_frac_exch_state_2, err_abs_frac_exch_state_2) %>%
      mutate(abs_frac_exch_state_1 = round(abs_frac_exch_state_1, 4),
             err_abs_frac_exch_state_1 = round(err_abs_frac_exch_state_1, 4),
             abs_frac_exch_state_2 = round(abs_frac_exch_state_2, 4),
             err_abs_frac_exch_state_2 = round(abs_frac_exch_state_2, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Abs Val Exch 1", "Err Abs Val Exch 1", "Abs Val Exch 2", "Err Abs Val Exch 2"))
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
        comparison_plot_data_exp()
      } else {
        comparison_plot_data_exp_abs()
      }
      
    }
    
  })
  
  ##
  
  comparison_plot_theo <- reactive({
    
    comparison_plot(calc_dat = dat_new(),
                    theoretical = TRUE,
                    relative = TRUE,
                    state_first = input[["state_first"]],
                    state_second = input[["state_second"]]) +
      labs(title = paste0("Theoretical fraction exchanged in state comparison in ", input[["chosen_time"]], " min"))
    
  })
  
  ##
  
  comparison_plot_theo_abs <- reactive({
    
    comparison_plot(calc_dat = dat_new(),
                    theoretical = TRUE,
                    relative = FALSE,
                    state_first = input[["state_first"]],
                    state_second = input[["state_second"]]) +
      labs(title = paste0("Theoretical absolute value exchanged in state comparison in ", input[["chosen_time"]], " min"))
    
  })
  
  ##
  
  comparison_plot_exp <- reactive({
    
    comparison_plot(calc_dat = dat_new(),
                    theoretical = FALSE,
                    relative = TRUE,
                    state_first = input[["state_first"]],
                    state_second = input[["state_second"]]) +
      labs(title = paste0("Fraction exchanged in state comparison in ", input[["chosen_time"]], " min"))
    
  })
  
  ##
  
  comparison_plot_exp_abs <- reactive({
    
    comparison_plot(calc_dat = dat_new(),
                    theoretical = FALSE,
                    relative = FALSE,
                    state_first = input[["state_first"]],
                    state_second = input[["state_second"]]) +
      labs(title = paste0("Absolute value exchanged in state comparison in ", input[["chosen_time"]], " min"))
    
  })
  
  ##
  
  output[["comparisonPlot"]] <- renderPlot({

    if (input[["theory"]]) {
      
      if (input[["calc_type"]] == "relative") {
        comparison_plot_theo() +
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["comp_plot_y_range"]][[1]], input[["comp_plot_y_range"]][[2]]) 
      } else {
        comparison_plot_theo_abs() +
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["comp_plot_y_range"]][[1]], input[["comp_plot_y_range"]][[2]]) 
      }
      
    } else {
        
      if (input[["calc_type"]] == "relative") {
        comparison_plot_exp() +
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["comp_plot_y_range"]][[1]], input[["comp_plot_y_range"]][[2]]) 
      } else {
        comparison_plot_exp_abs() +
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["comp_plot_y_range"]][[1]], input[["comp_plot_y_range"]][[2]]) 
      }
      
    }

  })
  
  ##
  
  differential_plot_data_theo <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, diff_theo_frac_exch, err_diff_theo_frac_exch) %>%
      mutate(diff_theo_frac_exch = round(diff_theo_frac_exch, 4),
             err_diff_theo_frac_exch = round(err_diff_theo_frac_exch, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Theo Diff Frac Exch", "Err Theo Diff Frac Exch"))
    
  })
  
  ##
  
  differential_plot_data_theo_abs <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, abs_diff_theo_frac_exch, err_abs_diff_theo_frac_exch) %>%
      mutate(abs_diff_theo_frac_exch = round(abs_diff_theo_frac_exch, 4),
             err_abs_diff_theo_frac_exch = round(err_abs_diff_theo_frac_exch, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Theo Abs Value Diff", "Err Theo Abs Value Diff"))
    
  })
  
  ##
  
  differential_plot_data_exp <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, diff_frac_exch, err_frac_exch) %>%
      mutate(diff_frac_exch = round(diff_frac_exch, 4),
             err_frac_exch = round(err_frac_exch, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Diff Frac Exch", "Err Diff Frac Exch"))
    
  })
  
  ##
  
  differential_plot_data_exp_abs <- reactive({
    
    dat_new() %>%
      select(Sequence, Start, End, abs_diff_frac_exch, err_abs_diff_frac_exch) %>%
      mutate(abs_diff_frac_exch = round(abs_diff_frac_exch, 4),
             err_abs_diff_frac_exch = round(err_abs_diff_frac_exch, 4)) %>%
      dt_format(cols = c("Sequence", "Start", "End", "Diff Abs Value Exch", "Err Diff Abs Value Exch"))
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
  
  differential_plot_theo <- reactive({
    
    woods_plot(calc_dat = dat_new(),
               theoretical = TRUE,
               relative = TRUE) +
      labs(title = TeX(paste0("$\\Delta$ theoretical fraction exchanged between states in ", input[["chosen_time"]], " min")))
    
  })
  
  ##
  
  differential_plot_theo_abs <- reactive({
    
    woods_plot(calc_dat = dat_new(),
               theoretical = TRUE,
               relative = FALSE) +
      labs(title = TeX(paste0("$\\Delta$ theoretical absolute value exchanged between states in ", input[["chosen_time"]], " min")))
    
  })
  
  
  ##
  
  differential_plot_exp <- reactive({
    
    woods_plot(calc_dat = dat_new(),
               theoretical = FALSE, 
               relative = TRUE) +
      labs(title = TeX(paste0("$\\Delta$ fraction exchanged between states in ", input[["chosen_time"]], " min")))
    
  })
  
  ##
  
  differential_plot_exp_abs <- reactive({
    
    woods_plot(calc_dat = dat_new(),
               theoretical = FALSE,
               relative = FALSE) +
      labs(title = TeX(paste0("$\\Delta$ absolute value exchanged between states in ", input[["chosen_time"]], " min")))
    
  })
  
  ##
  
  output[["differentialPlot"]] <- renderPlot({
    
    if (input[["theory"]]) {
      
      if (input[["calc_type"]] == "relative") {
        
        differential_plot_theo()+
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]])   
        
      } else {
        
        differential_plot_theo_abs()+
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]]) 
      }
      
    } else {
      
      if (input[["calc_type"]] == "relative") {
        
        differential_plot_exp()+
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]]) 
        
      } else {
        
        differential_plot_exp_abs()+
          xlim(input[["woods_plot_x_range"]][[1]], input[["woods_plot_x_range"]][[2]]) +
          ylim(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]]) 
        
      }
        
    }
    
  })

  ##
  # left as reactive, used more than once
  
  max_range <- reactive({

    max(dat()[['End']])

  })
  
  ##
  
  output[["protein_length"]] <- renderText({
    
    max_range()
    
  })
  
  ##
  
  observe({
    
    updateSliderInput(session, 
                      inputId = "plot_range",
                      max = max_range(),
                      value = c(0, max_range()))
    
    updateSliderInput(session, 
                      inputId = "woods_plot_x_range",
                      max = max_range(),
                      value = c(0, max_range()))
    
  })

  ##
  
  observe({
    
    if (input[["calc_type"]] == "absolute") {
      
      updateSliderInput(session,
                        inputId = "comp_plot_y_range",
                        min = -10,
                        max = 50,
                        value = c(0, 20))
    } else {
      
      updateSliderInput(session,
                        inputId = "comp_plot_y_range",
                        min = -2,
                        max = 2,
                        value = c(0, 1.5))
      
    }
    
  })
  ##
  
  ### TAB : GENERAL DATA ###
  
  ##
  
  observe({
    
    possible_states <- unique(dat()[["State"]])
    
    updateRadioButtons(session,
                       inputId = "chosen_state",
                       choices = possible_states)
    
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
      Name = c("Length", "Covered", "Cys"),
      Value = as.character(c(max_range(), 
                             paste0(round(100-100*str_count(protein_sequence(), 'x')/max_range(), 2), '%'),
                             str_count(protein_sequence(), 'C'))),
      stringsAsFactors = FALSE
    )
    
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
  
  output[["aminoDist"]] <- renderPlot({
    
    if (length(input[["hydro_prop"]]) == 0) {
      position_in_sequence() %>%
        ggplot(aes(x = amino, col = charge)) 
    } else if (length(input[["hydro_prop"]]) == 2){
      position_in_sequence() %>%
        ggplot(aes(x = amino, fill = charge)) + 
        geom_bar() +
        ylim(0, NA) + 
        labs(title = 'Amino distribution',
             x = 'Amino',
             y = 'Count')
    } else if (length(input[["hydro_prop"]]) == 1 & input[["hydro_prop"]] == "phobic") {
      position_in_sequence() %>%
        filter(is_hydrophobic) %>%
        ggplot(aes(x = amino, fill = charge)) + 
        geom_bar() +
        ylim(0, NA) + 
        labs(title = 'Amino distribution',
             x = 'Amino',
             y = 'Count')
    } else if (length(input[["hydro_prop"]]) == 1 & input[["hydro_prop"]] == "philic"){
      position_in_sequence() %>%
        filter(!is_hydrophobic) %>%
        ggplot(aes(x = amino, fill = charge)) + 
        geom_bar() +
        ylim(0, NA) + 
        labs(title = 'Amino distribution',
             x = 'Amino',
             y = 'Count')
    } 
    
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
    
    dat() %>%
      select(Start, End, State) %>%
      filter(State == input[["chosen_state"]]) %>%
      filter(Start >= input[["plot_range"]][[1]], End <= input[["plot_range"]][[2]]) %>%
      filter(!duplicated(.)) %>%
      select(-State) %>%
      mutate(ID = 1L:nrow(.)) %>%
      melt(id.vars = "ID") %>%
      ggplot(aes(x = value, y = ID, group = ID)) +
      geom_line() +
      labs(title = 'Peptyds positions compared to whole protein sequence',
           x = 'Position in sequence',
           y = '') +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
    
  })
  
  ##
  
  output[["stateOverlap"]] <- renderPlot({
    
    stateOverlap()
    
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
      labs(title = 'How much a position in sequence is covered?',
           x = 'Position in amino',
           y = 'Coverage') +
      theme(legend.position = "none")
    
  })
  
  ##
  
  output[["stateOverlapDist"]] <- renderPlot({
    
    stateOverlapDist()
    
  })
  
  ##
  
  ### TAB : REPORT ###

  ##
  
  output[["export_action"]] <- downloadHandler(
    
    filename <- "HaDeX_Report.html",
    
    content <- function(file) {
      
      rmarkdown::render(input = "report_template.Rmd", 
                        output_file = file, quiet = TRUE)
      

  })
  
  ##
  
}

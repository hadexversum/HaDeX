source("ui.R")

#########################################

server <- function(input, output, session) {
  
  output[["file_req"]] <- renderTable({
    
    file_req
    
  })
  
  dat <- reactive({
    
    inFile <- input[["data_file"]]
    
    if (is.null(inFile)){
      read.csv('./data/161114_BETA_alpha_gamma.csv')
    } else {
      read.csv(inFile[["datapath"]])
    }
    
  })
  
  ##
  
  observe({
    
    times_from_file <- round(unique(dat()["Exposure"]), 3)
    
    states_from_file <- unique(dat()["State"])
    
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
                      selected = states_from_file[1,])
    
    updateSelectInput(session,
                      inputId = "state_second",
                      choices = states_from_file,
                      selected = states_from_file[2,])
    
  })
  
  ##
  
  output[["plotParametersKrzys"]] <- renderTable({
    
    data.frame(
      Name = c("Theoretical?", "First state", "Second state", "Time in", "Chosen time", "Time out"),
      Value = c(input[["theory"]], input[["state_first"]], input[["state_second"]], input[["in_time"]], input[["chosen_time"]], input[["out_time"]]),
      stringsAsFactors = TRUE
    )
    
  })
  
  ##
  # TODO : change name for better one
  # Krzysiowe
  dat_1 <- reactive({
    
    chosen_state_first <- paste0(input[["state_first"]], "_", input[["chosen_time"]])
    chosen_state_second <- paste0(input[["state_second"]], "_", input[["chosen_time"]])
    
    in_state_first <- paste0(input[["state_first"]], "_", input[["in_time"]])
    out_state_first <- paste0(input[["state_first"]], "_", input[["out_time"]])
    zero_state_first <- paste0(input[["state_first"]], "_0")
      
    dat() %>%
      mutate(exp_mass = Center*z - z,
             Exposure = round(Exposure, 3)) %>%
      select(-Center, -z, -Protein) %>%
      group_by(Sequence, Start, End, MaxUptake, State, Exposure, File) %>%
      summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
      ungroup(.) %>%
      unite(State_Exposure, State, Exposure) %>%
      spread(key = State_Exposure, value = avg_exp_mass) %>%
      group_by(Sequence, Start, End) %>%
      mutate(in_time_mean = mean(.[.[["Sequence"]] == Sequence, ][[in_state_first]], na.rm = TRUE),
             out_time_mean = mean(.[.[["Sequence"]] == Sequence, ][[out_state_first]], na.rm = TRUE),
             zero_time_mean = mean(.[.[["Sequence"]] == Sequence, ][[zero_state_first]], na.rm = TRUE),
             theo_deut = MaxUptake*deuteration_mass) %>%
      ungroup(.) %>%
      mutate(frac_exch_state_1 = (.[[chosen_state_first]] - in_time_mean)/(out_time_mean - in_time_mean),
             frac_exch_state_2 = (.[[chosen_state_second]] - in_time_mean)/(out_time_mean - in_time_mean),
             theo_frac_exch_state_1 = (.[[chosen_state_first]] - zero_time_mean)/theo_deut,
             theo_frac_exch_state_2 = (.[[chosen_state_second]] - zero_time_mean)/theo_deut) %>%
      group_by(Sequence, Start, End, MaxUptake) %>%
      summarize(avg_frac_exch_state_1 = mean(frac_exch_state_1, na.rm = TRUE),
                sd_frac_exch_state_1 = sd(frac_exch_state_1, na.rm = TRUE),
                avg_frac_exch_state_2 = mean(frac_exch_state_2, na.rm = TRUE),
                sd_frac_exch_state_2 = sd(frac_exch_state_2, na.rm = TRUE),
                avg_theo_frac_exch_state_1 = mean(theo_frac_exch_state_1, na.rm = TRUE),
                sd_theo_frac_exch_state_1 = sd(theo_frac_exch_state_1, na.rm = TRUE),
                avg_theo_frac_exch_state_2 = mean(theo_frac_exch_state_2, na.rm = TRUE),
                sd_theo_frac_exch_state_2 = sd(theo_frac_exch_state_2, na.rm = TRUE)) %>%
      ungroup(.) %>%
      mutate(Med_Sequence = Start + (End - Start)/2,
             diff_frac_exch = avg_frac_exch_state_1 - avg_frac_exch_state_2,
             err_frac_exch = sqrt(sd_frac_exch_state_1^2 + sd_frac_exch_state_2^2),
             diff_theo_frac_exch = avg_theo_frac_exch_state_1 - avg_theo_frac_exch_state_2,
             err_theo_frac_exch = sqrt(sd_theo_frac_exch_state_1^2 + sd_theo_frac_exch_state_2^2)) %>%
      arrange(Start, End)
    
  })
  
  ##
  
  output[["comparisonPlotKrzys_data"]] <- DT::renderDataTable({
    
    if (input[["theory"]]) {
      
      dat_1() %>%
        select(Sequence, Start, End, avg_theo_frac_exch_state_1, sd_theo_frac_exch_state_1, avg_theo_frac_exch_state_2, sd_theo_frac_exch_state_2) %>%
        mutate(avg_theo_frac_exch_state_1 = round(avg_theo_frac_exch_state_1, 4),
               sd_theo_frac_exch_state_1 = round(sd_theo_frac_exch_state_1, 4),
               avg_theo_frac_exch_state_2 = round(avg_theo_frac_exch_state_2, 4),
               sd_theo_frac_exch_state_2 = round(sd_theo_frac_exch_state_2, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Theo Frac Exch 1", "Err Theo Frac Exch 1", "Theo Frac Exch 2", "Err Theo Frac Exch 2"))
      
    } else {
      
      dat_1() %>%
        select(Sequence, Start, End, avg_frac_exch_state_1, sd_frac_exch_state_1, avg_frac_exch_state_2, sd_frac_exch_state_2) %>%
        mutate(avg_frac_exch_state_1 = round(avg_frac_exch_state_1, 4),
               sd_frac_exch_state_1 = round(sd_frac_exch_state_1, 4),
               avg_frac_exch_state_2 = round(avg_frac_exch_state_2, 4),
               sd_frac_exch_state_2 = round(sd_frac_exch_state_2, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Frac Exch 1", "Err Frac Exch 1", "Frac Exch 2", "Err Frac Exch 2"))
      
    }
    
  })
  
  ##
  
  output[["comparisonPlotKrzys"]] <- renderPlot({
    
    if (input[["theory"]]) {

      ggplot() +
        geom_segment(data = dat_1(), aes(x = Start, y = avg_theo_frac_exch_state_1, xend = End, yend = avg_theo_frac_exch_state_1, color = "state_1")) +
        geom_segment(data = dat_1(), aes(x = Start, y = avg_theo_frac_exch_state_2, xend = End, yend = avg_theo_frac_exch_state_2, color = "state_2")) +
        geom_errorbar(data = dat_1(), aes(x = Med_Sequence, ymin = avg_theo_frac_exch_state_1 - sd_theo_frac_exch_state_1, ymax = avg_theo_frac_exch_state_1 + sd_theo_frac_exch_state_1, color = "state_1")) + 
        geom_errorbar(data = dat_1(), aes(x = Med_Sequence, ymin = avg_theo_frac_exch_state_2 - sd_theo_frac_exch_state_2, ymax = avg_theo_frac_exch_state_2 + sd_theo_frac_exch_state_2, color = "state_2")) + 
        labs(x = "Position in sequence", y = "Theoretical fraction Exchanged", title = paste0("Theoretical fraction exchanged in state comparison in ", input[["chosen_time"]], " min")) +
        theme(legend.position="bottom") + 
        scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2)) 

      } else {

        ggplot() +
          geom_segment(data = dat_1(), aes(x = Start, y = avg_frac_exch_state_1, xend = End, yend = avg_frac_exch_state_1, color = "state_1")) +
          geom_segment(data = dat_1(), aes(x = Start, y = avg_frac_exch_state_2, xend = End, yend = avg_frac_exch_state_2, color = "state_2")) +
          geom_errorbar(data = dat_1(), aes(x = Med_Sequence, ymin = avg_frac_exch_state_1 - sd_frac_exch_state_1, ymax = avg_frac_exch_state_1 + sd_frac_exch_state_1, color = "state_1")) + 
          geom_errorbar(data = dat_1(), aes(x = Med_Sequence, ymin = avg_frac_exch_state_2 - sd_frac_exch_state_2, ymax = avg_frac_exch_state_2 + sd_frac_exch_state_2, color = "state_2")) + 
          labs(x = "Position in sequence", y = "Theoretical fraction Exchanged", title = paste0("Theoretical fraction exchanged in state comparison in ", input[["chosen_time"]]," min")) +
          theme(legend.position="bottom") +
          scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2)) 

      }
    
  })
  
  ##
  
  output[["differentailPlotKrzys_data"]] <- DT::renderDataTable({
    
    if (input[["theory"]]) {
      
      dat_1() %>%
        select(Sequence, Start, End, diff_theo_frac_exch, err_theo_frac_exch) %>%
        mutate(diff_theo_frac_exch = round(diff_theo_frac_exch, 4),
               err_theo_frac_exch = round(err_theo_frac_exch, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Theo Diff Frac Exch", "Err Theo Diff Frac Exch"))
    
    } else {
      
      dat_1() %>%
        select(Sequence, Start, End, diff_frac_exch, err_frac_exch) %>%
        mutate(diff_frac_exch = round(diff_frac_exch, 4),
               err_frac_exch = round(err_frac_exch, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Diff Frac Exch", "Err Diff Frac Exch"))
      
    }
    
  })
  
  ##
  
  output[["differentailPlotKrzys"]] <- renderPlot({

    if (input[["theory"]]) {

      ggplot() + 
        geom_segment(data = dat_1(), aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch)) +
        geom_errorbar(data = dat_1(), aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_theo_frac_exch, ymax = diff_theo_frac_exch + err_theo_frac_exch)) +
        scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) + 
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = "Position in sequence", y = TeX("$\\Delta$ Fraction Exchanged"), title = TeX("$\\Delta$ Theoretical fraction exchanged between states in 1 min")) 

    } else {

      ggplot() + 
        geom_segment(data = dat_1(), aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch)) +
        geom_errorbar(data = dat_1(), aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch)) +
        scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) + 
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = "Position in sequence", y = TeX("$\\Delta$ Fraction Exchanged"), title = expression(paste(Delta, " Fraction exchanged between states in chosen time")))

    }


  })
  
  
  ##
  # TODO : change name for more informative one
  dat_new <- reactive({
    
    chosen_state_first <- paste0(input[["state_first"]], "_", input[["chosen_time"]])
    chosen_state_second <- paste0(input[["state_second"]], "_", input[["chosen_time"]])
    
    in_state_first <- paste0(input[["state_first"]], "_", input[["in_time"]])
    out_state_first <- paste0(input[["state_first"]], "_", input[["out_time"]])
    in_state_second <- paste0(input[["state_second"]], "_", input[["in_time"]])
    out_state_second <- paste0(input[["state_second"]], "_", input[["out_time"]])

    dat() %>%
      mutate(exp_mass = Center*z - z*proton_mass,
             Exposure = round(Exposure, 3)) %>%
      select(-Center, -z, -Protein) %>%
      group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, File) %>%
      summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
      ungroup() %>%
      unite(State_Exposure, State, Exposure) %>%
      spread(key = State_Exposure, value = avg_exp_mass) %>%
      mutate(theo_in_time_first = (!!sym(chosen_state_first) - MHP)/ (MaxUptake * proton_mass),
             theo_in_time_second = (!!sym(chosen_state_second) - MHP)/(MaxUptake * proton_mass)) %>%
      group_by(Sequence, Start, End) %>%
      summarize(in_time_mean_first = mean(!!sym(in_state_first), na.rm = TRUE),
                err_in_time_mean_first = coalesce(sd(!!sym(in_state_first), na.rm = TRUE), 0),
                chosen_time_mean_first = mean(!!sym(chosen_state_first), na.rm = TRUE),
                err_chosen_time_mean_first = sd(!!sym(chosen_state_first), na.rm = TRUE),
                out_time_mean_first = mean(!!sym(out_state_first), na.rm = TRUE),
                err_out_time_mean_first = sd(!!sym(out_state_first), na.rm = TRUE),
                in_time_mean_second = mean(!!sym(in_state_second), na.rm = TRUE),
                err_in_time_mean_second = coalesce(sd(!!sym(in_state_second), na.rm = TRUE), 0),
                chosen_time_mean_second = mean(!!sym(chosen_state_second), na.rm = TRUE),
                err_chosen_time_mean_second = sd(!!sym(chosen_state_second), na.rm = TRUE),
                out_time_mean_second = mean(!!sym(out_state_second), na.rm = TRUE),
                err_out_time_mean_second = sd(!!sym(out_state_second), na.rm = TRUE),
                avg_theo_in_time_first = mean(theo_in_time_first, na.rm = TRUE),
                err_avg_theo_in_time_first = sd(theo_in_time_first, na.rm = TRUE),
                avg_theo_in_time_second = mean(theo_in_time_second, na.rm = TRUE),
                err_avg_theo_in_time_second = sd(theo_in_time_second, na.rm = TRUE))  %>%
      mutate(frac_exch_state_1 = (chosen_time_mean_first - in_time_mean_first)/(out_time_mean_first - in_time_mean_first),
             err_frac_exch_state_1 = sqrt(err_chosen_time_mean_first^2 + 2*err_in_time_mean_first^2 + err_in_time_mean_first^2), 
             frac_exch_state_2 = (chosen_time_mean_second - in_time_mean_second)/(out_time_mean_second - in_time_mean_second),
             err_frac_exch_state_2 = sqrt(err_chosen_time_mean_second^2 + 2*err_in_time_mean_second^2 + err_in_time_mean_second^2),
             diff_frac_exch = frac_exch_state_1 - frac_exch_state_2,
             err_frac_exch = sqrt(err_frac_exch_state_1^2 + err_frac_exch_state_2^2),
             diff_theo_frac_exch = avg_theo_in_time_first - avg_theo_in_time_second, 
             err_diff_theo_frac_exch = sqrt(err_avg_theo_in_time_first^2 + err_avg_theo_in_time_second^2),
             Med_Sequence = Start + (End - Start)/2) %>%
      select(Sequence, Start, End, Med_Sequence, frac_exch_state_1, err_frac_exch_state_1, frac_exch_state_2, 
             err_frac_exch_state_2, diff_frac_exch, err_frac_exch, diff_theo_frac_exch, err_diff_theo_frac_exch, 
             avg_theo_in_time_first, avg_theo_in_time_second, err_avg_theo_in_time_first, err_avg_theo_in_time_second) %>%
      arrange(Start, End)
    
    
  })
  
  ##
  
  output[["comparisonPlot_data"]] <- DT::renderDataTable({
    
    if (input[["theory"]]) {
      
      dat_new() %>%
        select(Sequence, Start, End, avg_theo_in_time_first, err_avg_theo_in_time_first, avg_theo_in_time_second, err_avg_theo_in_time_second) %>%
        mutate(avg_theo_in_time_first = round(avg_theo_in_time_first, 4),
               err_avg_theo_in_time_first = round(err_avg_theo_in_time_first, 4),
               avg_theo_in_time_second = round(avg_theo_in_time_second, 4),
               err_avg_theo_in_time_second = round(err_avg_theo_in_time_second, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Theo Frac Exch 1", "Err Theo Frac Exch 1", "Theo Frac Exch 2", "Err Theo Frac Exch 2"))
      
    } else {
      
      dat_new() %>%
        select(Sequence, Start, End, frac_exch_state_1, err_frac_exch_state_1, frac_exch_state_2, err_frac_exch_state_2) %>%
        mutate(frac_exch_state_1 = round(frac_exch_state_1, 4),
               err_frac_exch_state_1 = round(err_frac_exch_state_1, 4),
               frac_exch_state_2 = round(frac_exch_state_2, 4),
               err_frac_exch_state_2 = round(err_frac_exch_state_2, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Frac Exch 1", "Err Frac Exch 1", "Frac Exch 2", "Err Frac Exch 2"))
      
    }
    
    
  })
  
  ##
  
  output[["comparisonPlot"]] <- renderPlot({

    if (input[["theory"]]) {
      
      ggplot()+
        geom_segment(data = dat_new(), aes(x = Start, y = avg_theo_in_time_first, xend = End, yend = avg_theo_in_time_first, color = "state_1")) +
        geom_segment(data = dat_new(), aes(x = Start, y = avg_theo_in_time_second, xend = End, yend = avg_theo_in_time_second, color = "state_2")) +
        geom_errorbar(data = dat_new(), aes(x = Med_Sequence, ymin = avg_theo_in_time_first - err_avg_theo_in_time_first, ymax = avg_theo_in_time_first + err_avg_theo_in_time_first, color = "state_1")) + 
        geom_errorbar(data = dat_new(), aes(x = Med_Sequence, ymin = avg_theo_in_time_second - err_avg_theo_in_time_second, ymax = avg_theo_in_time_second + err_avg_theo_in_time_second, color = "state_2")) + 
        labs(x = "Position in sequence", y = "Theoretical fraction Exchanged", title = paste0("Theoretical fraction exchanged in state comparison in ", input[["chosen_time"]], " min")) +
        theme(legend.position = "bottom") +
        scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2))
  
      } else {   
  
        ggplot() +
          geom_segment(data = dat_new(), aes(x = Start, y = frac_exch_state_1, xend = End, yend = frac_exch_state_1, color = "state_1")) +
          geom_segment(data = dat_new(), aes(x = Start, y = frac_exch_state_2, xend = End, yend = frac_exch_state_2, color = "state_2")) +
          geom_errorbar(data = dat_new(), aes(x = Med_Sequence, ymin = frac_exch_state_1 - err_frac_exch_state_1, ymax = frac_exch_state_1 + err_frac_exch_state_1, color = "state_1")) + 
          geom_errorbar(data = dat_new(), aes(x = Med_Sequence, ymin = frac_exch_state_2 - err_frac_exch_state_2, ymax = frac_exch_state_2 + err_frac_exch_state_2, color = "state_2")) + 
          labs(x = "Position in sequence", y = "Fraction Exchanged", title = paste0("Fraction exchanged in state comparison in ", input[["chosen_time"]], " min")) +
          theme(legend.position = "bottom") + 
          scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2)) 
  
      }

  })
  
  ##
  
  output[["differentialPlot_data"]] <- DT::renderDataTable({
    
    if (input[["theory"]]) {
      
      dat_new() %>%
        select(Sequence, Start, End, diff_theo_frac_exch, err_diff_theo_frac_exch) %>%
        mutate(diff_theo_frac_exch = round(diff_theo_frac_exch, 4),
               err_diff_theo_frac_exch = round(err_diff_theo_frac_exch, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Theo Diff Frac Exch", "Err Theo Diff Frac Exch"))
      
    } else {
      
      dat_new() %>%
        select(Sequence, Start, End, diff_frac_exch, err_frac_exch) %>%
        mutate(diff_frac_exch = round(diff_frac_exch, 4),
               err_frac_exch = round(err_frac_exch, 4)) %>%
        dt_format(cols = c("Sequence", "Start", "End", "Diff Frac Exch", "Err Diff Frac Exch"))
      
    }
    
  })
  
  ##
  
  output[["differentialPlot"]] <- renderPlot({
    
    if (input[["theory"]]) {
      
      ggplot() + 
        geom_segment(data = dat_new(), aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch)) +
        geom_errorbar(data = dat_new(), aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_diff_theo_frac_exch, ymax = diff_theo_frac_exch + err_diff_theo_frac_exch)) +
        scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) + 
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = "Position in sequence", y = TeX("$\\Delta$ Theoretical fraction Exchanged"), title = expression(paste(Delta, " Theoretical fraction exchanged between states in chosen time"))) 

      } else {
        
        ggplot() + 
          geom_segment(data = dat_new(), aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch)) +
          geom_errorbar(data = dat_new(), aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch)) +
          scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) + 
          geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
          labs(x = "Position in sequence", y = TeX("$\\Delta$ Fraction Exchanged"), title = expression(paste(Delta, " Fraction exchanged between states in chosen time"))) 
        
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
    
  })

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
  
  output[["sequenceName"]] <- renderText({
    
    paste0("<span>", 
           gsubfn(pattern = 'C', replacement = function(x) paste0('<font color = "red">', x, "</font>"), x = protein_sequence()),
           "</span>")
    
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
        ylim(0, 30) + 
        labs(title = 'Amino distribution',
             x = 'Amino',
             y = 'Count')
    } else if (length(input[["hydro_prop"]]) == 1 & input[["hydro_prop"]] == "phobic") {
      position_in_sequence() %>%
        filter(is_hydrophobic) %>%
        ggplot(aes(x = amino, fill = charge)) + 
        geom_bar() +
        ylim(0, 30) + 
        labs(title = 'Amino distribution',
             x = 'Amino',
             y = 'Count')
    } else if (length(input[["hydro_prop"]]) == 1 & input[["hydro_prop"]] == "philic"){
      position_in_sequence() %>%
        filter(!is_hydrophobic) %>%
        ggplot(aes(x = amino, fill = charge)) + 
        geom_bar() +
        ylim(0, 30) + 
        labs(title = 'Amino distribution',
             x = 'Amino',
             y = 'Count')
    } 
    
  })
  
  ##
  
  output[["stateOverlap_data"]] <- DT::renderDataTable({
    
    dat() %>%
      select(Sequence, Start, End, State) %>% 
      filter(State == input[["chosen_state"]]) %>%
      filter(Start >= input[["plot_range"]][[1]], End <= input[["plot_range"]][[2]]) %>%
      filter(!duplicated(.)) %>%
      select(-State) %>%
      dt_format(cols = c("Sequence", "Start", "End"))
    
  })
  
  ##
  
  output[["stateOverlap"]] <- renderPlot({
    
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
  
  output[["stateOverlapDist_data"]] <- DT::renderDataTable({
    
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
  
  output[["stateOverlapDist"]] <- renderPlot({
    
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
  
}

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


  ##
  
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

  ##
  
  observe({

    if(data_source() == "HDeXaminer"){
      shinyjs::show("HaDeX-examiner-settings-panel")
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
    
    # browser()

    status <- ""
    if (is.null(input[["data_file"]])){
      status <- "Example file: KD_180110_CD160_HVEM.csv."
    } else {
      length(dat_in()[[1]])
      status <- "Supplied file is valid."
    }

    if(data_source() == "HDeXaminer"){
      paste0(status, "\nDetected data source: ", data_source(), ". User action needed below!")
    } else {
      paste0(status, "\nDetected data source: ", data_source(), ".")
    }


  })

  ##

  ### TAB: INPUT DATA

  ##

  dat_exam <- eventReactive(input[["exam_apply_changes"]], {

    get_internal_messages(update_hdexaminer_file(
                          dat = dat_in(),
                          fd_time = input[["examiner_fd_timepoint"]],
                          old_protein_name = exam_protein_name_from_file(),
                          new_protein_name = input[["exam_protein_name"]],
                          old_state_name = exam_state_name_from_file(),
                          new_state_name = strsplit(input[["exam_state_name"]], ",")[[1]],
                          confidence = input[["exam_confidence"]]))
  })

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

  ##
  
  observe({
    
    updateSelectInput(session,
                      inputId = "chosen_protein",
                      choices = proteins_from_file(),
                      selected = proteins_from_file()[1])
    
  })
  
  observe({
    
    no_deut_times <- times_from_file()[times_from_file() < 0.1] 
    
    updateSelectInput(session, 
                      inputId = "no_deut_control",
                      choices = no_deut_times,
                      selected = max(no_deut_times))
    
  })
  
  ################
  #### VALUES #### 
  ################
  
  # peptides_from_file <- reactive({
  #   
  #   unique(dat()[["Sequence"]])
  #   
  # })
  
  #
  
  states_from_file <- reactive({
    
    unique(dat()[["State"]])
    
  })
  
  states_chosen_protein <- reactive({
    
    dat() %>%
      filter(Protein == input[["chosen_protein"]]) %>%
      select(State) %>%
      unique(.) %>%
      .[[1]]
      
  })
  
  
  ##
  
  times_from_file <- reactive({
    
    times_from_file <- round(unique(dat()[["Exposure"]]), 3)
    
    times_from_file[order(times_from_file)]
    
  })
  
  times_with_control <- reactive({
    
    tmp <- sort(unique(round(dat()[["Exposure"]], 3)))
    choose_time_100 <- setNames(tmp, c(head(tmp, -1), "chosen control"))
    
    choose_time_100
    
  })
  
  times_t <- reactive({
    
    times_from_file()[times_from_file() > 0 & times_from_file()<99999]
    
  })
  
  ## mark for modifications

  has_modifications <- reactive({
    
    attr(dat_in(), "has_modification")

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
  
  max_range_from_file <- reactive({
    
    max(filter(dat_tmp(), Protein == input[["chosen_protein"]])[['End']])
    
  })

  ##
  
  max_range <- reactive({
    
    max(max_range_from_file(), as.numeric(input[["sequence_length"]]), na.rm = T)

  })

  ##
  
  output[["sequence_length_exp_info"]] <- renderText({
    
    paste("Sequence length from the file is ", max_range_from_file(), ".")
    
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

  # observe({
  # 
  #   tryCatch({
  #     if(input[["deut_part"]] < 0)
  #       updateNumericInput(session,
  #                          inputId = "deut_part",
  #                          value = 0)
  #   }, error = function(e){
  #     updateNumericInput(session,
  #                        inputId = "deut_part",
  #                        value = 0)
  #   })
  # })

  observe({

    tryCatch({
      if(input[["deut_part"]] > 100)
        updateNumericInput(session,
                           inputId = "deut_part",
                           value = 100)
    },
    error = function(e){
      updateNumericInput(session,
                         inputId = "deut_part",
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
                       value = max_range_from_file())

  })

  ##

  ## create dat based on control

  dat <- reactive({

    create_control_dataset(dat = dat_tmp(),
                           control_protein = input[["chosen_protein"]],
                           control_state = strsplit(input[["chosen_control"]], " \\| ")[[1]][2],
                           control_exposure = strsplit(input[["chosen_control"]], " \\| ")[[1]][3])
  })

  ### TAB: SEQUENCE DATA ###

  source("server/tab_sequence_data.R", local = TRUE)

  ### TAB: COVERAGE ###

  source("server/tab_coverage.R", local = TRUE)
  
  ### TAB: QUALITY CONTROL

  source("server/tab_quality_control.R", local = TRUE)
  
  ### TAB: SUMMARY
  
  source("server/tab_summary.R", local = TRUE)
  
  ### TAB: REPORT ###
  
  source("server/tab_report.R", local = TRUE)
  
  ##

  ### TAB: WOODS PLOT ###

  source("server/tab_comparison.R", local = TRUE)

  source("server/tab_differential.R", local = TRUE)

  ### TAB: BUTTERFLY ###

  source("server/tab_butterfly.R", local = TRUE) # check

  source("server/tab_butterfly_differential.R", local = TRUE)

  ### TAB: VOLCANO ###

  source("server/tab_volcano.R", local = TRUE)

  ### TAB: REPLICATES ###

  source("server/tab_replicates.R", local = TRUE)

  ### TAB: CHICLET ###

  source("server/tab_chiclet.R", local = TRUE)

  source("server/tab_chiclet_differential.R", local = TRUE)

  ### TAB: KINETICS ###

  source("server/tab_uptake_curve.R", local = TRUE)
  
  source("server/tab_uptake_curve_differential.R", local = TRUE)
  
  ### TAB: MANHATTAN ###
  
  source("server/tab_manhattan.R", local = TRUE)

  


}

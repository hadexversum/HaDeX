tab_input <- function() HaDeX_nonplotTab(
  
  title = "Input data",
  
  p(strong("Upload your file."), "Otherwise, you will see the example data."),
  p("Currently HaDeX is limited to `cluster` files from DynamX 3.0 or 2.0 and `tables` file from  HDeXaminer.",
    "The supplied file should contain at least two repetitions of the measurement for the uncertainty to be calculated.",
    "If the supplied file contains modified peptides, maximal exchange control cannot be applied."),
  p("Please be aware that loading data (including example file) may take a while. Thank you for the patience."),
  
  wellPanel(
    fillRow(
      flex = c(NA, 1),
      fileInput(
        inputId = "data_file",
        label = "Choose file:",
        multiple = FALSE,
        accept = c(".csv", ".xlsx", ".xls"),
        placeholder = "No file selected"
      ),
      div(
        id = "HaDeX-file-status-panel",
        h6("File status:"),
        div(
          id = "HaDeX-file-status-message",
          withHaDeXSpinner(verbatimTextOutput("data_file_info"))
        )
      )
    )
  ),
  
  fillRow(
    id = "HaDeX-file-requirements-section",
    flex = c(1, NA),
    p("For the program to behave correctly, please make sure supplied file fulfills all requirements.",
      "Requirements can be displayed by clicking the button."),
    HaDeX_collapseButton(
      title = "Show requirements",
      target = "#HaDeX-file-requirements-table"
    )
  ),
  
  HaDeX_collapsablePanel(
    id = "HaDeX-file-requirements-table",
    tableOutput("file_req")
  ),
  
  h3("Upload settings"),
  p("Values chosen here are propagated into all of the tabs for coherent results."),
  
  fillRow(
    id = "HaDeX-settings-panel",
    wellPanel(
      id = "HaDeX-standard-settings-panel",
      input_parameters()
    ),
    
    hidden(
      wellPanel(
        id = "HaDeX-examiner-settings-panel",
        h3("File from HDeXaminer detected!"),
        span(
          "Some of the information from the data file requires your confirmation.",
          "For the additional information on how the data from HDeXaminer is processed, check the requirements above.",
          "Keep in mind that the MHP value is generated based on the peptide sequence and therefore, may differ from actual value in case of the modifications."
        ),
        numericInput_h(inputId = "examiner_fd_timepoint",
                       label = "FD timepoint [min]:",
                       value = 1440,
                       min = 0,
                       width = "100%"),
        textInput_h(inputId = "exam_protein_name",
                    label = "Protein name:",
                    width = "100%"),
        textInput_h(inputId = "exam_state_name",
                    label = "States names:",
                    width = "100%"),
        checkboxGroupInput_h(inputId = "exam_confidence",
                             label = "Accepted confidence values:",
                             choices = c("High", "Medium", "Low"),
                             selected = c("Medium", "High")),
        actionButton(inputId = "exam_apply_changes",
                     label = "Apply changes to continue"),
        span(
          "The calculated values of MPH might slightly differ based on data used and its precision."
        ),
        a(
          href = "http://www.matrixscience.com/help/aa_help.html", 
          "Used amino mass data"
        ),
        DT::dataTableOutput("checking_exam_data"),
      )
    ),
    flex = c(NA, 1)
  )
)

input_parameters <- function() HaDeX_plotSettingsSection(
  
  title = "Select the parameters:",
  
  selectInput_h(inputId = "chosen_protein",
                label = "Choose protein: ",
                choices = c("db_CD160"),
                width = "100%"),
  selectInput_h(inputId = "chosen_control",
                label = "Maximal exchange control: ",
                choices = c("not present", "db_CD160 | CD160 | 1440"),
                width = "100%"),
  # selectInput_h(inputId = "no_deut_control",
  #               label = "No deuterated time point:",
  #               choices = c(0, 0.001),
  #               selected = 0.001),
  numericInput_h(inputId = "deut_part",
                 label = "Choose D20 concentration [%]: ",
                 value = 90,
                 min = 0, max = 100, step = 1,
                 width = "100%"),
  
  fluidPage(
    fluidRow(
      column(
        width = 6,
        numericInput_h(inputId = "sequence_start_shift",
                       label = "Sequence start:",
                       value = 1, step = 1,
                       width = "100%"),
      ),
      column(
        width = 6,
        numericInput_h(inputId = "sequence_length",
                       label = "Sequence length:",
                       value = 300, step = 1,
                       width = "100%")
      )
    )
  ),
  textOutput("sequence_length_exp_info")
  
  
)

tab_coverage <- tabPanel(
  
  "Coverage",
  
  br(),
  
  sidebarPanel(
    class = "scrollable",
           
    textOutput("cov_protein_coverage"),
    br(),
    
    ##### SETTINGS ##### 
           
    radioButtons(
      inputId = 'chosen_state',
      label = 'Choose state:',
      choices = c('CD160', 'CD160_HVEM'),
      selected = 'CD160'
    ),
    
    sliderInput(
      inputId = 'plot_range',
      label = 'Choose range:',
      min = 1,
      max = 300,
      value = c(1, 300),
      # ticks = seq(1, 300, 1) # this breaks shiny 1.5
    )
  ),
         
  mainPanel(
    class = "scrollable",
    tabsetPanel(
      
      ##### PEPTIDE COVERAGE ##### 
             
      tabPanel(
        "Peptide Coverage",
        br(),
        plotOutput_h("stateOverlap", hover = hoverOpts("stateOverlap_hover", delay = 10, delayType = "debounce")),
        uiOutput("stateOverlap_debug"),
        downloadButton("stateOverlap_download_button",
                       "Save chart (.svg)")
      ),
             
      tabPanel(
        "Data",
        br(),
        DT::dataTableOutput("stateOverlap_data")
      )
    ),
    br(),
    
    
    tabsetPanel(
      
      ##### POSITION FREQUENCY ##### 
             
      tabPanel(
        "Position Frequency",
        br(),
        div(style = "position:relative;",
            plotOutput_h("stateOverlapDist", hover = hoverOpts("stateOverlapDist_hover", delay = 10, delayType = "debounce")),
            uiOutput("stateOverlapDist_debug")),
            downloadButton("stateOverlapDist_download_button",
                           "Save chart (.svg)"
        )
      ),
      
      tabPanel(
        "Data",
        br(),
        DT::dataTableOutput("stateOverlapDist_data")
      )
    )
           
  )
)
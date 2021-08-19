tab_sequence <- tabPanel("Sequence data",
         h3('Protein name'),
         h4(textOutput("protein_name"), class  = "monospaced"),
         h3('Reconstructed sequence'),
         htmlOutput("sequenceName", container = tags[["span"]], class  = "monospaced"),
         br(),
         wellPanel(
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 column(6,
                        tableOutput("protein_stats"),
                        br(),
                        checkboxGroupInput(
                          inputId = "hydro_prop",
                          label = "Hydro-",
                          choices = c(
                            "Hydrophilic" = "philic",
                            "Hydrophobic" = "phobic"),
                          selected = c("philic", "phobic"))
                 ),
                 column(6,
                        br()
                 )
               )
             ),
             mainPanel(withSpinner(plotOutput("aminoDist", hover = hoverOpts("aminoDist_hover", delay = 10, delayType = "debounce"))),
                       uiOutput("aminoDist_debug"),
                       downloadButton("aminoDist_download_button",
                                      "Save chart (.svg)"),
                       p("Source: Kyte, J., and Doolittle, R.F. (1982). A simple method for displaying the hydropathic character of a protein. J Mol Biol 157, 105–132."))
           )
         )
)
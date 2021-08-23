tab_summary <- tabPanel("Summary",
         br(),
         fluidRow(
           withHaDeXSpinner(DT::dataTableOutput("summary_table"))
         ),
         includeMarkdown("./readmes/summary.md")
)
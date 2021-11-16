tab_summary <- function() HaDeX_nonplotTab(
  title = "Summary",
  withHaDeXSpinner(DT::dataTableOutput("summary_table", width = "60%")),
  includeMarkdown("./readmes/summary.md")
)
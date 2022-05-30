tab_about <- function() HaDeX_nonplotTab(

  title = "About",
  
  fluidPage(
    fluidRow(
      column(
        width = 5, 
        includeMarkdown("readmes/about.md")
      ),
      column(
        width = 6, 
        includeMarkdown("readmes/resources.md")
      )
    )
  )
  
  
  
  
)

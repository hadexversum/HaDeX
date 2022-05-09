tab_start <- function() HaDeX_nonplotTab(
  
  title = "Start",
  
  h1("Welcome to HaDeX!"),
  fluidPage(
    fluidRow(
      column(
        width = 5, 
        h6("Thank you for using our tool."),
        h6("Questions/feature requests/commercial applications: hadex@ibb.waw.pl")
      ),
      column(
        width = 5,
        span("This is the development version of the application and may not be working correctly yet. The official version will be available soon. ", style="color:red")
      )
    )
  ),
  includeMarkdown("readmes/about.md"),
  img(
    id = 'HaDeX-funding-icons',
    src = 'funding_icons.png'
  )
)
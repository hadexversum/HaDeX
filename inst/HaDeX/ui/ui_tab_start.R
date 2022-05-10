tab_start <- function( #mobile = FALSE
  ) HaDeX_nonplotTab(
  
  title = "Start",
  
  h1("Welcome to HaDeX!"),
  
  # if (!mobile) NULL else p(
  #   tags$b(
  #     "For better user experience please use device with wider screen (at least 900px)."
  #   )
  # ),
  
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
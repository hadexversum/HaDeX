tab_start <- function(mobile = FALSE) HaDeX_nonplotTab(
  title = "Start",
  h1("Welcome to HaDeX!"),
  if (!mobile) NULL else p(
    tags$b(
      "For better user experience please use device with wider screen (at least 900px)."
    )
  ),
  p(
    "Thank you for using our tool.",
    "Questions/feature requests/commercial applications: hadex@ibb.waw.pl"
  ),
  includeMarkdown("readmes/about.md"),
  img(
    id = 'HaDeX-funding-icons',
    src = 'funding_icons.png'
  )
)
tab_start <- function() HaDeX_nonplotTab(
  title = "Start",
  h1("Welcome to HaDeX!"),
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
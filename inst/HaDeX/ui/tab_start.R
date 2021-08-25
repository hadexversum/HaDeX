tab_start <- tabPanel(
  "Start",
  div(class = "HaDeX-nonplot-tab-content",
      h1("Welcome to HaDeX!"),
      span("Thank you for using our tool.",
           "Questions/feature requests/commercial applications: hadex@ibb.waw.pl"),
      includeMarkdown("readmes/about.md"),
      img(class='HaDeX-funding-icons',
          src='funding_icons.png'),
      br()
  )
)
tab_start <- tabPanel("Start",
         h3("Welcome to HaDeX!"),
         h4("Thank you for using our tool."),
         h4("Questions/feature requests/commercial applications: hadex@ibb.waw.pl"),
         includeMarkdown("readmes/about.md"),
         img(class='funding-icons',
             src='funding_icons.png'),
         br()
)
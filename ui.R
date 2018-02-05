library(shiny)
library(shinythemes)
shinyUI(
    fluidPage(
          br(),br(),br(), br(), theme = shinytheme("superhero"),
          titlePanel(h2("What's Next?", align="center"), windowTitle = "DelGraph App"),
          h6("Data Specialization Capstone", align="center"), 
          h6("DelGraph - Aug. 2017", align="center"),
          hr(), br(), br(), br(),
          fluidRow(column(6, offset=3, align="center", 
                          textInput("phrase", label = "type in and push go!",
                                    placeholder="Type here ...",
                                    value = ""),
                          actionButton("goButton", "Go!"),
                          h6(textOutput("stats")),
                          h4("suggested next word:"),
                          h3(textOutput("nextword")))), br(), br(), hr(),
                          h3(img(src='logo.png'), align="center")         
    )
)

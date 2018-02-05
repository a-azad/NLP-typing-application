load("alldata.RData", envir=.GlobalEnv)
source("whatisnextword.R")
whatsnext <- function(phrase) return(SBO(phrase, removeProfanity=TRUE))
shinyServer(function(input, output) {
    phraseGo <- eventReactive(input$goButton, {input$phrase})
    output$stats <- renderText({
      numword <- length(strsplit(input$phrase," ")[[1]])
      paste("so far, you have entered  ", numword, "  words")})
    output$nextword <- renderText({paste0(whatsnext(phraseGo()))})
})

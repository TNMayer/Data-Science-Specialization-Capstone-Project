library(shiny)
source('prediction.R')
next_word <- c('hello', 'world')
probability <- c(0.03, 0.0002)
results <- data.frame(next_word = next_word)
results$probability <- format(c(0.002, 0.0003), scientific = F)
names(results) <- c('Next Word', 'Probability')

function(input,output){
  output$inputValue <- renderPrint({input$text})
  output$b <- renderPrint({results})
  output$prediction <- renderPrint({
    backoff(input$text, fDT1, fDT2, fDT3, maxResults = 5)
  })
  output$cleaned <- renderPrint({
    preprocessString(input$text)
  })
}
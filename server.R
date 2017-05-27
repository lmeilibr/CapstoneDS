library(shiny)
library(ggplot2)
source("prediction.R", local = TRUE)

shinyServer(function(input,output){
  #Load the n-grams to memory
  unigram <- readRDS("./Data/unigram.rds")
  bigram <- readRDS("./Data/bigram.rds")
  trigram <- readRDS("./Data/trigram.rds")
  quadgram <- readRDS("./Data/quadgram.rds")
  pentagram <- readRDS("./Data/pentagram.rds")
  
  #plot the top1 predicted word
  output$result <- renderText({
    count <- length(unlist(str_split(input$text, " ")))
    t <- str_trim(input$text, side = "both")
    if(count > 2){
      t <- predict(t, unigram, bigram, trigram, quadgram, pentagram)
      graph <- TRUE
      as.character(t[1,1])
    }
    })
  #plot the top5 predictions
    output$plot1 <- renderPlot({
      count <- length(unlist(str_split(input$text, " ")))
      t <- str_trim(input$text, side = "both")
      if(count > 2){
        t <- predict(t, unigram, bigram, trigram, quadgram, pentagram)
        ggplot(t, aes(x = reorder(NextWord, -score), y = score)) +
          geom_col(fill = "blue", alpha = 0.7) +
          xlab("Top 5 Predictions")
      }
    })
    
}
)
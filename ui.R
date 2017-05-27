library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Word Prediction App - by Leandro Meili"),
  sidebarPanel(
    textInput("text", "Insert your text"),
    tags$head(tags$style("#text{color: black;
                                 font-size: 20px;
                                 }"
    )
    ),
    hr(),
    helpText("Word Prediction using the Stupid Backoff algorithm")
  ),
  mainPanel(
    h3("Prediction"),
    textOutput("result"),
    tags$head(tags$style("#result{color: blue;
                                 font-size: 20px;
                         font-style: italic;
                         }"
    )),
    h3("Scores"),
    plotOutput("plot1")
  )
))
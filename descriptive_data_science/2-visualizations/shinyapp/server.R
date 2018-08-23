
library(shiny)
library(shinydashboard)
library(tidyr)
library(readr)
library(plotly)

data <- read_csv("series_from_imdb.csv")

shinyServer(function(input, output) {
  
  finalInput <- reactive({
    filter(data, series_name %in% c(input$select_series)) %>%
    filter(season <= input$select_seasons[1])
  })
  
  output$lineplot <- renderPlotly({
    
    plot_ly(data=finalInput(), x=~as.numeric(series_ep), y=~UserRating, color=~series_name) %>%
    add_trace(type='scatter', mode='lines') %>%
    add_trace(size=~log(UserVotes/sqrt(UserVotes)), type='scatter', alpha=0.9) %>%
      
    layout(title = "Notas gerais de episódios",
           yaxis = list(title="Avaliação geral do episódio"),
           xaxis = list(title="Número do episódio na série"))
  })
  
  output$scatter <- renderPlotly({
    plot_ly(data=finalInput(), x=~as.numeric(series_ep), size=~log(UserVotes/sqrt(UserVotes))) %>%
    add_trace(y=~UserVotes, color=~series_name, type='scatter', alpha=0.9) %>%
    
    layout(title = "Número de votos de episódios",
           yaxis = list(title="Número de votos do episódio"),
           xaxis = list(title="Número do episódio na série"))
  })
  
})
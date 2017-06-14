
library(shiny)
library(shinydashboard)
library(tidyr)
library(readr)
library(plotly)

data <- read_csv("series_from_imdb.csv")

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    filter(data, series_name %in% c(input$select_series))
  })
  
  finalInput <- reactive({
    filter(dataInput(), season <= input$select_seasons[1])
  })
  
  output$lineplot <- renderPlotly({
    
    plot_ly(data=finalInput(), x=~as.numeric(series_ep)) %>%
    add_trace(y=~UserRating, color=~series_name, name="Nota", type='scatter', mode='lines') %>%
      
    layout(yaxis = list(title="Avaliação geral do episódio"),
           xaxis = list(title="Número do episódio na série"))
  })
  
  output$scatter <- renderPlotly({
    plot_ly(data=finalInput(), x=~as.numeric(series_ep), size=~log(UserVotes/sqrt(UserVotes))) %>%
    add_trace(y=~UserVotes, color=~series_name, name="Nota", type='scatter', alpha=0.9) %>%
    
    layout(yaxis = list(title="Número de votos do episódio"),
           xaxis = list(title="Número do episódio na série"))
  })
  
})
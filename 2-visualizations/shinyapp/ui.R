
library(shiny)
library(tidyr)
library(readr)
library(plotly)

shinyUI(fluidPage(
  titlePanel(h1("Popularidade de uma série com base em nas avaliações e votos de seus episódios", align = "center")),
  
  column(3,
         h3("TIPS:"),
         helpText("Selecione as séries de que deseja saber informações.",
                  "A partir do a avaliação do episódio e da quantidade de votos que recebeu,",
                  "veja quão popular é a série!",
                  "Filtre pelo número da temporada também! :)"),
         
         selectInput("select_series", label = h3("Séries"),
                     choices = unique(data$series_name), multiple=T),
         
         sliderInput("select_seasons", label = h3("Temporadas"),
                     min=1, max=max(data$season), value=6),
         
         helpText("OBS: o diâmetro do círculo é proporcional ao número de votos do episódios que o círculo representa.")
  ),
  
  mainPanel(
    fluidRow(
      box(
        plotlyOutput("lineplot", width = "100%"),
        title = "Notas gerais de episódios",
        width = 12
      ),
      
      box(
        plotlyOutput("scatter", width = "100%"),
        title = "Número de votos de episódios",
        width = 12
      )
    )
  )
))


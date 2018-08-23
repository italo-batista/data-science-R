
library(shiny)
library(tidyr)
library(readr)
library(plotly)

dados <- read_csv("series_from_imdb.csv")

shinyUI(fluidPage(
  titlePanel(h1("Popularidade de uma série com base nas avaliações e nos votos de seus episódios", align = "center")),
  
  br(),
  br(),
  
  sidebarLayout(
                sidebarPanel(
                  h5("TIPS:"),
                  helpText("Selecione as séries de que deseja saber informações.",
                           "A partir da avaliação do episódio e da quantidade de votos que este recebeu,",
                           "veja quão popular é a série!",
                           "Filtre pelo número da temporada também! :)",
                           "OBS: o diâmetro do círculo é proporcional ao número de votos do episódio que o círculo representa."),
                  
                  selectInput("select_series", label = h3("Séries"),
                              choices = unique(dados$series_name), multiple=T),
                  
                  sliderInput("select_seasons", label = h3("Temporadas"),
                              min=1, max=max(dados$season), value=6)
                ),
                
                mainPanel(
                  fluidRow(
                      plotlyOutput("lineplot", width = "90%"),
                      
                      br(),
                      br(),
                      
                      plotlyOutput("scatter", width = "90%")
                  )
                )
    )
))


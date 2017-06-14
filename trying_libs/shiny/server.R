# server.R

library(quantmod)
source("helpers.R")

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    
    dataInput <- reactive({
      getSymbols(input$symb, src = "google", 
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    })

    chartSeries(dataInput(), theme = chartTheme("white"), 
      type = "line", log.scale = input$log, TA = NULL)
  })
  
})



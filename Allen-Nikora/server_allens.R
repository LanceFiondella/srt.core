library(shiny)

shinyServer(function(input, output) {
  
  output$trendMessage <- renderText({ 
    "Cannot reject hypothesis of no reliability growth at .05 significance."
  })

  # This is solely for mock-up purposes - this is to save the failure data file after it's been
  # filtered or transformed.
  
    datasetInput <- rock
    output$saveData <- downloadHandler(
      filename = function() { paste("rock", '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput, file)
      }
    )
  
  # This is solely for mock-up purposes - this is to save the trend test table.
  
    trendInput <- cars
    output$saveTrendTable <- downloadHandler(
      filename = function() { paste("cars", '.csv', sep='') },
      content = function(file) {
        write.csv(trendInput, file)
      }
    )
  
}
)


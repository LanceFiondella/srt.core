# Plot model results (and raw data, if specified)

# First check to make sure that one or more sets of model results have been selected for display.
# The names of the models to display are returned from the selection list as a vector.

if((length(input$modelResultChoice) > 0) && !(input$modelResultChoice[1] == "None")){
    ModelPlot <- ggplot(,aes_string(x="Index",y="FailureDisplayType"))
  
  # Now plot the results for each of the models that was selected
  # by the user for display
  
  for (modelIndex in 1:length(input$modelResultChoice)) {
    
    # Pick up the set of model results indicated by modelResultChoice[modelIndex]
    
    # Now set up the plotting data according to whether we're displaying cumulative
    # failures, IF times, failure intensity, or reliability
    
    if(input$modelPlotChoice == "IF") {
      plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["IF"]])
      ModelPlot <- ModelPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),data_set_global))
      ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
    } else if(input$modelPlotChoice == "FC") {
      plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["FC"]])
      ModelPlot <- ModelPlot+ggtitle(paste(c("Failure Counts vs. Cumulative Test Time of"),data_set_global))
      ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Failure Counts per Test Interval"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Failure Counts per Test Interval")
    } else if(input$modelPlotChoice == "CF") {
      plot_data <- data.frame(ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["MVF"]], c(input$modelDataRange[1]:(input$modelDataRange[2]+input$modelNumPredSteps)))
      ModelPlot <- ModelPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),data_set_global))
      ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
    } else if(input$modelPlotChoice == "FI") {
      plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["FI"]])
      ModelPlot <- ModelPlot+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),data_set_global))
      ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
    } else if(input$modelPlotChoice == "REL") {
      # plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["IF"]])
    }
    
    names(plot_data) = c("Index","FailureDisplayType")
    
    # Finally, draw the plot depending on whether we want points only, lines only,
    # or both lines and data points.
        
    if(input$ModelDataPlotType == 1) {
      
      # Data points and lines
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType), color="blue")+ geom_line(data=plot_data, color="blue")
      
    } else if(input$ModelDataPlotType == 2) {
      
      # Data points only
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType), color="blue")
      
    } else if(input$ModelDataPlotType == 3) {
      
      # Lines only
      
      ModelPlot <- ModelPlot + geom_line(data=plot_data,aes(Index,FailureDisplayType), color="blue")
      
    }
    
    ModelPlot <- ModelPlot + theme(legend.position = "bottom")
  }
}

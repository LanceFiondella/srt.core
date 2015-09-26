# Plot model results (and raw data, if specified)

plot_model_results <- function(ModResults, DataModeled, DataSetName, DisplayModels, DataView, PlotView, PlotData) {
  
  require(ggplot2)
  
  PlotFault <- FALSE
  
  # Initialize the model results plot
  
  localResultsPlot <- ggplot()
  
  # Set up the values that we'll need to create a plot legend
  
  scaleManBreaks <- c()
  scaleManColors <- c()
  
  for (modelIndex in DisplayModels) {
    # Create plot data, axes, and titles based on the view
    # of the data selected by the user (e.g., MTTFs)
    
    if(DataView == "MTTF") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "MVF", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "IF", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
      localResultsPlot <- localResultsPlot + ggtitle(paste0("Interfailure Times vs. Cumulative Test Time for ", DataSetName))
      localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
    } else if(DataView == "MVF") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "MVF", sep="_")]], "Failure" = ModResults[["Failure"]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
      localResultsPlot <- localResultsPlot + ggtitle(paste0("Cumulative Failures vs. Cumulative Test Time for ", DataSetName))
      localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Cumulative Failures")
    } else if(DataView == "FI") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "MVF", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "FI", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
      localResultsPlot <- localResultsPlot + ggtitle(paste0("Failure Intensity vs. Cumulative Test Time for ", DataSetName))
      localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Failure Intensity")
    } else if(DataView == "R") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "MVF", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "Rel", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
      localResultsPlot <- localResultsPlot + ggtitle(paste0("Reliability vs. Cumulative Test Time for ", DataSetName))
      localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Reliability")
    } else if (DataView == "FC") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "MVF", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "FC", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
      localResultsPlot <- localResultsPlot + ggtitle(paste0("Failure Counts vs. Cumulative Test Time for ", DataSetName))
      localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Failures per Test Interval")
    } else {
      
      # Couldn't identify view of data to display.
      # Print an error message.
      
      print(msgModelDataViewUnknown)
      PlotFault <- TRUE
    }
    
    scaleManBreaks <- c(scaleManBreaks, get(paste(modelIndex, "fullname", sep="_")))
    scaleManColors <- c(scaleManColors, get(paste(modelIndex, "plotcolor", sep="_")))
    
    if (PlotView == "points_and_lines") {
      localResultsPlot <- localResultsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model)) + geom_line(data=model_plot_data, aes(Time,Failure,color=Model,linetype=Model))
    } else if (PlotView == "points") {
      localResultsPlot <- localResultsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model))
    } else if (PlotView == "lines") {
      localResultsPlot <- localResultsPlot + geom_line(data=model_plot_data, aes(Time,Failure,color=Model,linetype=Model))
    } else {
      
      # Couldn't identify the plot type.
      # Print an error message.
      
      print(paste0("plot_model_results: ", msgPlotTypeUnknown))
      PlotFault <- TRUE
    }
  }
  
  
  if(PlotData) {
    
    scaleManBreaks <- c(scaleManBreaks, "Data")
    scaleManColors <- c(scaleManColors, "black")
    
    if (dataType(names(DataModeled)) == "FR") {
      FN <- DataModeled$FN
      FT <- DataModeled$FT
      IF <- DataModeled$IF
    } else if (dataType(names(DataModeled)) == "FC") {
      
      # We need to complete the failure counts models.
      
    } else {
      # The type of the input data couldn't be determined.
      # Print an error message.
      
      print(msgInputDataTypeUnknown)
      PlotFault <- TRUE
    }
    
    # Now plot data depending on the view of the data.
    
    if(DataView == "MTTF") {
      model_plot_data <- data.frame("Time"=FT, "Failure"=IF, "Model"=rep("Data", length(FT)))
      localResultsPlot <- localResultsPlot
    } else if(DataView == "MVF") {
      model_plot_data <- data.frame("Time"=FT, "Failure"=FN, "Model"=rep("Data", length(FT)))
    } else if(DataView == "FI") {
      model_plot_data <- data.frame("Time"=FT, "Failure"=c(1/IF), "Model"=rep("Data", length(FT)))
    } else if (DataView == "FC") {
      model_plot_data <- data.frame("Time"=FT, "Failure"=FC, "Model"=rep("Data", length(FT)))
    } else {
      
      # Couldn't identify view of data to display.
      # Print an error message.
      
      print(msgModelDataViewUnknown)
      PlotFault <- TRUE
    }
    
    if (PlotView == "points_and_lines") {
      localResultsPlot <- localResultsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model)) + geom_step(data=model_plot_data, aes(Time,Failure,color=Model,linetype=Model))
    } else if (PlotView == "points") {
      localResultsPlot <- localResultsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model))
    } else if (PlotView == "lines") {
      localResultsPlot <- localResultsPlot + geom_step(data=model_plot_data, aes(Time,Failure,color=Model,linetype=Model))
    } else {
      
      # Couldn't identify the plot type.
      # Print an error message.
      
      print(paste0("plot_model_results: ", msgPlotTypeUnknown))
      PlotFault <- TRUE
    }
  }
  
  #localResultsPlot <- localResultsPlot + scale_color_manual("", breaks=scaleManBreaks, values=scaleManColors)
  localResultsPlot <- localResultsPlot + theme(legend.position = "bottom")

  if(PlotFault) {
    localResultsPlot = NULL
  }
  return(localResultsPlot)
}
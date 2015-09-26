# Plot model results (and raw data, if specified)

<<<<<<< HEAD
# First check to make sure that one or more sets of model results have been selected for display.
# The names of the models to display are returned from the selection list as a vector.

if((length(input$modelResultChoice) > 0) && !(input$modelResultChoice[1] == "None")){
  ModelPlot <- ggplot(,aes_string(x="Index",y="FailureDisplayType"))
    
  DataStart <- ModelResultsList[["DataStartAndEnd"]]$Start
  DataEnd <- ModelResultsList[["DataStartAndEnd"]]$End
  NumPreds <- ModelResultsList[["DataStartAndEnd"]]$NumPreds
=======
plot_model_results <- function(ModResults, DataModeled, DataSetName, DisplayModels, DataView, PlotView, PlotData) {
  
  require(ggplot2)
  
  PlotFault <- FALSE
  
  # Initialize the model results plot
  
  localResultsPlot <- ggplot()
>>>>>>> allen-development
  
  # Set up the values that we'll need to create a plot legend
  
  scaleManBreaks <- c()
  scaleManColors <- c()
  
<<<<<<< HEAD
  # Now plot the results for each of the models that was selected
  # by the user for display
  
  for (modelIndex in 1:length(input$modelResultChoice)) {
    
    # Pick up the set of model results indicated by modelResultChoice[modelIndex]
    
    # First add the appropriate break value and color value to the
    # arrays defining the appearance of the legend.
    
    scaleManBreaks <- c(scaleManBreaks, input$modelResultChoice[modelIndex])
    scaleManColors <- c(scaleManColors, K_ModelResultColors[[input$modelResultChoice[modelIndex]]])
    
    # Now set up the plotting data according to whether we're displaying cumulative
    # failures, IF times, failure intensity, or reliability
    
    if(input$modelPlotChoice == "IF") {
      plot_data <- data.frame(ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]], ModelResultsList[[input$modelResultChoice[modelIndex]]][["IF"]])
      ModelPlot <- ModelPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
      #ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
    } else if(input$modelPlotChoice == "FC") {
      plot_data <- data.frame(ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]], ModelResultsList[[input$modelResultChoice[modelIndex]]][["FC"]])
      ModelPlot <- ModelPlot+ggtitle(paste(c("Failure Counts vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
      #ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Failure Counts per Test Interval"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Failure Counts per Test Interval")
    } else if(input$modelPlotChoice == "CF") {
      plot_data <- data.frame(ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]], c(DataStart:(DataEnd + NumPreds)))
      ModelPlot <- ModelPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
      #ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
    } else if(input$modelPlotChoice == "FI") {
      plot_data <- data.frame(ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]], ModelResultsList[[input$modelResultChoice[modelIndex]]][["FI"]])
      ModelPlot <- ModelPlot+ggtitle(paste(c("Failure Intensity vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
      #ModelPlot <- ModelPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
      ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
    } else if(input$modelPlotChoice == "REL") {
      # plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["IF"]])
    }
    
    names(plot_data) = c("Index","FailureDisplayType")
    
    # Finally, draw the plot depending on whether we want points only, lines only,
    # or both lines and data points.
        
    if(input$ModelDataPlotType == 1) {
      
      # Data points and lines
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType), color=K_ModelResultColors[[input$modelResultChoice[modelIndex]]])+ geom_line(data=plot_data, color=K_ModelResultColors[[input$modelResultChoice[modelIndex]]])
      
    } else if(input$ModelDataPlotType == 2) {
      
      # Data points only
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType), color=K_ModelResultColors[[input$modelResultChoice[modelIndex]]])
      
    } else if(input$ModelDataPlotType == 3) {
      
      # Lines only
      
      ModelPlot <- ModelPlot + geom_line(data=plot_data,aes(Index,FailureDisplayType), color=K_ModelResultColors[[input$modelResultChoice[modelIndex]]])
      
    }
  }
  #ModelPlot <- ModelPlot + scale_color_manual("", breaks=scaleManBreaks, values=scaleManColors)
  #ModelPlot <- ModelPlot + theme(legend.position = "bottom")

  # Now plot the raw data if the user has asked for it.
  
  if(input$checkboxDataOnPlot) {
    
    if(ModelResultsList[["DataSetType"]] == "IFTimes") {
      FN <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$FN >= DataStart, select = c(FN, IF, FT)), FN <= DataEnd, select = FN)), use.names=FALSE)
      FT <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$FN >= DataStart, select = c(FN, IF, FT)), FN <= DataEnd, select = FT)), use.names=FALSE)
      IF <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$FN >= DataStart, select = c(FN, IF, FT)), FN <= DataEnd, select = IF)), use.names=FALSE)
    } else if(ModelResultsList[["DataSetType"]] == "FailureCounts") {
      FC <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$TI >= DataStart, select = c(TI, T, FC, CFC)), TI <= DataEnd, select = FC)), use.names=FALSE)
      CFC <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$TI >= DataStart, select = c(TI, T, FC, CFC)), TI <= DataEnd, select = CFC)), use.names=FALSE)
      CumT <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$TI >= DataStart, select = c(TI, T, FC, CFC)), TI <= DataEnd, select = T)), use.names=FALSE)
      TI <- c(unlist(subset(subset(ModelResultsList[["Data"]], ModelResultsList[["Data"]]$TI >= DataStart, select = c(TI, T, FC, CFC)), TI <= DataEnd, select = TI)), use.names=FALSE)
      
      #FT <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataEnd, select = FC_FT)), use.names=FALSE)
      #IF <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataEnd, select = FC_IF)), use.names=FALSE)
    }
        
    if(input$modelPlotChoice == "IF") {
      plot_data <- data.frame(FT, IF)
    } else if(input$modelPlotChoice == "FC") {
      plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["FC"]])
    } else if(input$modelPlotChoice == "CF") {
      plot_data <- data.frame(FT, FN)
    } else if(input$modelPlotChoice == "FI") {
      plot_data <- data.frame(FT, c(1/IF))
    }
    
    names(plot_data) = c("Index","FailureDisplayType")
    
    if(input$ModelDataPlotType == 1) {
      
      # Data points and lines
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType), color="black")+ geom_step(data=plot_data, color="black")
      
    } else if(input$ModelDataPlotType == 2) {
      
      # Data points only
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType), color="black")
      
    } else if(input$ModelDataPlotType == 3) {
      
      # Lines only
      
      ModelPlot <- ModelPlot + geom_step(data=plot_data,aes(Index,FailureDisplayType), color="black")
      
    } 
  }
}
=======
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
>>>>>>> allen-development

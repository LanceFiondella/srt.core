# Plot model results (and raw data, if specified)

# First check to make sure that one or more sets of model results have been selected for display.
# The names of the models to display are returned from the selection list as a vector.

if((length(input$modelResultChoice) > 0) && !(input$modelResultChoice[1] == "None")){
  plot_data <- data.frame("Failures"=c(1:length(ModelResultsList[[input$modelResultChoice[1]]][["MVF"]])))
  ModelPlot <- ggplot(, aes_string(x="Index",y="FailureDisplayType"))
  
  DataStart <- ModelResultsList[["DataStartAndEnd"]]$Start
  DataEnd <- ModelResultsList[["DataStartAndEnd"]]$End
  NumPreds <- ModelResultsList[["DataStartAndEnd"]]$NumPreds
  
  # Set up the values that we'll need to create a plot legend
  
  scaleManBreaks <- c()
  scaleManColors <- c()
  
  # Set up a table from which the model results will be plotted.
  
  for (modelIndex in 1:length(input$modelResultChoice)) {
    
    # First add the appropriate break value and color value to the
    # arrays defining the appearance of the legend.
    
    scaleManBreaks <- c(scaleManBreaks, input$modelResultChoice[modelIndex])
    scaleManColors <- c(scaleManColors, K_ModelResultColors[[input$modelResultChoice[modelIndex]]])
    
    # Set up the plotting data according to whether we're displaying cumulative
    # failures, IF times, failure intensity, or reliability
    
    if(input$modelPlotChoice == "IF") {
      plot_data[, paste(input$modelResultChoice[modelIndex], "CumTime", sep="_")] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]]
      plot_data[, input$modelResultChoice[modelIndex]] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["IF"]]
    } else if(input$modelPlotChoice == "FC") {
      plot_data[, paste(input$modelResultChoice[modelIndex], "CumTime", sep="_")] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]]
      plot_data[, input$modelResultChoice[modelIndex]] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["FC"]]
    } else if(input$modelPlotChoice == "CF") {
      plot_data[, paste(input$modelResultChoice[modelIndex], "CumTime", sep="_")] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]]
      plot_data[, input$modelResultChoice[modelIndex]] <- c(DataStart:(DataEnd + NumPreds))
    } else if(input$modelPlotChoice == "FI") {
      plot_data[, paste(input$modelResultChoice[modelIndex], "CumTime", sep="_")] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["MVF"]]+ModelResultsList[["TimeOffset"]]
      plot_data[, input$modelResultChoice[modelIndex]] <- ModelResultsList[[input$modelResultChoice[modelIndex]]][["FI"]]
    } else if(input$modelPlotChoice == "REL") {
      # plot_data <- data.frame((ModelResultsList[["Data"]])[["FT"]], ModelResultsList[[unlist(ModelsExecutedList[modelIndex], use.names=FALSE)]][["IF"]])
    }
  }
  
  # Add titles and axis labels
  
  if(input$modelPlotChoice == "IF") {
    ModelPlot <- ModelPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
    ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
  } else if(input$modelPlotChoice == "FC") {
    ModelPlot <- ModelPlot+ggtitle(paste(c("Failure Counts vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
    ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Failure Counts per Test Interval")
  } else if(input$modelPlotChoice == "CF") {
    ModelPlot <- ModelPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
    ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
  } else if(input$modelPlotChoice == "FI") {
    ModelPlot <- ModelPlot+ggtitle(paste(c("Failure Intensity vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
    ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
  } else if(input$modelPlotChoice == "REL") {
    ModelPlot <- ModelPlot+ggtitle(paste(c("Estimated Reliability vs. Cumulative Test Time of"),ModelResultsList[["DataSetName"]]))
    ModelPlot <- ModelPlot + xlab("Cumulative Test Time")+ylab("Estimated Reliability for Specified Mission Length")
  }
  
  # Now plot the results for each of the models that was selected
  # by the user for display
  
  for (modelIndex in 1:length(input$modelResultChoice)) {
    
    # Draw the plot depending on whether we want points only, lines only,
    # or both lines and data points.
    
    if(input$ModelDataPlotType == 1) {
      
      # Data points and lines
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data, aes(x=paste0(input$modelResultChoice[modelIndex], "_CumTime"), y=input$modelResultChoice[modelIndex], color=input$modelResultChoice[modelIndex]))
      
    } else if(input$ModelDataPlotType == 2) {
      
      # Data points only
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data, aes(x=paste0(input$modelResultChoice[modelIndex], "_CumTime"), y=input$modelResultChoice[modelIndex], color=input$modelResultChoice[modelIndex]))
      
    } else if(input$ModelDataPlotType == 3) {
      
      # Lines only
      
      ModelPlot <- ModelPlot + geom_line(data=plot_data, aes(x=paste0(input$modelResultChoice[modelIndex], "_CumTime"), y=input$modelResultChoice[modelIndex], color=input$modelResultChoice[modelIndex]))
      
    }
  }
  
  # Now plot the raw data if the user has asked for it.
  
  if(input$checkboxDataOnPlot) {
    
    scaleManBreaks <- c(scaleManBreaks, "Data")
    scaleManColors <- c(scaleManColors, "black")
    
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
    
    EmptyFill <- rep(NA, length(plot_data$Failures)-length(FT))
    if(input$modelPlotChoice == "IF") {
      plot_data[, "Data_CumTime"] <- c(FT, EmptyFill)
      plot_data[, "Data"] <- c(IF, EmptyFill)
    } else if(input$modelPlotChoice == "FC") {
      plot_data[, "Data_CumTime"] <- c(FT, EmptyFill)
      plot_data[, "Data"] <- c(IF, EmptyFill)
    } else if(input$modelPlotChoice == "CF") {
      plot_data[, "Data_CumTime"] <- c(FT, EmptyFill)
      plot_data[, "Data"] <- c(IF, EmptyFill)
    } else if(input$modelPlotChoice == "FI") {
      plot_data[, "Data_CumTime"] <- c(FT, EmptyFill)
      plot_data[, "Data"] <- c(c(1/IF), EmptyFill)
    }
    
    if(input$ModelDataPlotType == 1) {
      
      # Data points and lines
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(x=Data_CumTime, y=Data, color="Data")) + geom_line(data=plot_data,aes(x=Data_CumTime, y=Data, color="Data"))
      
    } else if(input$ModelDataPlotType == 2) {
      
      # Data points only
      
      ModelPlot <- ModelPlot + geom_point(data=plot_data,aes(Data_CumTime, Data, color="Data"))
      
    } else if(input$ModelDataPlotType == 3) {
      
      # Lines only
      
      ModelPlot <- ModelPlot + geom_step(data=plot_data,aes(Data_CumTime, Data, color="Data"))
      
    } 
  }
  
  ModelPlot <- ModelPlot + scale_color_manual("", breaks=scaleManBreaks, values=scaleManColors) + scale_y_continuous("")
  ModelPlot <- ModelPlot + theme(legend.position = "bottom")
  
}
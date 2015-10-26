# Plot model results (and raw data, if specified)

plot_model_results <- function(ModResults, DataModeled, DataSetName, DisplayModels, DataView, PlotView, PlotData, PlotDataEnd, RelMissionTime) {
  
  require(ggplot2)
  
  PlotFault <- FALSE
  
  # Initialize the model results plot
  
  localResultsPlot <- ggplot()
  
  # Set up the values that we'll need to create a plot legend
  
  scaleManBreaks <- c()
  scaleManColors <- c()
  
  # Create plot axes
  
  if(DataView == "IF") {
    localResultsPlot <- localResultsPlot + ggtitle(paste0("Interfailure Times vs. Cumulative Test Time for ", DataSetName))
    localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
  } else if(DataView == "MVF") {
    localResultsPlot <- localResultsPlot + ggtitle(paste0("Cumulative Failures vs. Cumulative Test Time for ", DataSetName))
    localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Cumulative Failures")
  } else if(DataView == "FI") {
    localResultsPlot <- localResultsPlot + ggtitle(paste0("Failure Intensity vs. Cumulative Test Time for ", DataSetName))
    localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Failure Intensity")
  } else if(DataView == "R") {
    localResultsPlot <- localResultsPlot + ggtitle(paste0("Reliability vs. Cumulative Test Time for ", DataSetName))
    localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Reliability")
  } else if(DataView == "R_growth") {
    localResultsPlot <- localResultsPlot + ggtitle(paste0("Reliability Growth vs. Cumulative Test Time for ", DataSetName, ": Operational Time of ", as.character(RelMissionTime)))
    localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Reliability Growth")
  } else if (DataView == "FC") {
    localResultsPlot <- localResultsPlot + ggtitle(paste0("Failure Counts vs. Cumulative Test Time for ", DataSetName))
    localResultsPlot <- localResultsPlot + xlab("Cumulative Test Time")+ylab("Failures per Test Interval")
  } else {
    
    # Couldn't identify view of data to display.
    # Print an error message.
    
    print(msgModelDataViewUnknown)
    PlotFault <- TRUE
  }
  
  for (modelIndex in DisplayModels) {
    # Create plot data, axes, and titles based on the view
    # of the data selected by the user (e.g., MTTFs)
    
    if(DataView == "IF") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "IF", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
    } else if(DataView == "MVF") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "MVF", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
    } else if(DataView == "FI") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "FI", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
    } else if(DataView == "R") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "Rel", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
    } else if(DataView == "R_growth") {
      
      # This is an interactive plot - users can change the mission time for
      # which reliability will be computed.  The plot will then be redrawn.
      
      rg_input_data <- data.frame("FT" = subset(ModResults, !is.infinite(get(paste0(modelIndex, "_CumTime"))), select=get(paste0(modelIndex, "_CumTime"))))
      names(rg_input_data) <- c("FT")
      model_params <- c()
      for (parmIndex in 1:length(get(paste0(modelIndex, "_params")))) {
        model_params <- c(model_params, ModResults[[paste0(modelIndex, "_parm_", parmIndex)]][length(DataModeled[[1]])])
      }
      names(model_params) <- paste(modelIndex, get(paste0(modelIndex, "_params")), sep="_")
      temp_R_growth <- data.frame("Reliability_Growth"=c(get(paste(modelIndex,"R_growth",sep="_"))(model_params, rg_input_data, RelMissionTime)[["Reliability_Growth"]], rep(1, length(ModResults[[paste(modelIndex, "CumTime", sep="_")]])-length(rg_input_data[[1]]))))
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", sep="_")]], "Failure" = temp_R_growth[["Reliability_Growth"]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
    } else if (DataView == "FC") {
      model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", sep="_")]], "Failure" = ModResults[[paste(modelIndex, "FC", sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
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
    
    # There aren't any sensible plots to be drawn if we're showing
    # reliability or reliability growth.
    
    if((DataView != "R") && (DataView != "R_growth")) {
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
      
      if(DataView == "IF") {
        model_plot_data <- data.frame("Time"=FT, "Failure"=IF, "Model"=rep("Data", length(FT)))
        localResultsPlot <- localResultsPlot
      } else if(DataView == "MVF") {
        model_plot_data <- data.frame("Time"=FT, "Failure"=FN, "Model"=rep("Data", length(FT)))
      } else if(DataView == "FI") {
        model_plot_data <- data.frame("Time"=FT, "Failure"=c(1/IF), "Model"=rep("Data", length(FT)))
      } else if (DataView == "FC") {
        model_plot_data <- data.frame("Time"=FT, "Failure"=FC, "Model"=rep("Data", length(FT)))
      } else if (!((DataView == "R") || (DataView == "R_growth"))) {
        
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
  }
  
  if(PlotDataEnd) {
    localResultsPlot <- localResultsPlot + geom_vline(xintercept=DataModeled$FT[length(DataModeled$FT)], linetype='longdash', alpha = 0.8)
  }
    
  
  #localResultsPlot <- localResultsPlot + scale_color_manual("", breaks=scaleManBreaks, values=scaleManColors)
  localResultsPlot <- localResultsPlot + theme(legend.position = "bottom")

  if(PlotFault) {
    localResultsPlot = NULL
  }
  return(localResultsPlot)
}
# Plot model results (and raw data, if specified)

#plot_model_results <- function(ModResults, DataModeled, DataSetName, DisplayModels, DataView, PlotView, PlotData, PlotDataEnd, RelMissionTime, plotWidthRange, plotHeightRange, plotPixels, AdditionalCurveLength) {
#plot_model_results          (ModelResults, ModeledData, ModeledDataName, input$modelResultChoice, input$modelPlotChoice, input$ModelDataPlotType, input$checkboxDataOnPlot, input$checkboxDataEndOnPlot, input$modelRelMissionTime, MPranges$x, MPranges$y, session$clientData$output_ModelPlot_width, input$modelCurveAdditionalTime)
plot_model_results <- function(ModResults, ModelsThatRan, DataModeled, DataSetName, input, plotWidthRange, plotHeightRange, plotPixels) {
  require(ggplot2)
  
  DataModeled <- DataModeled$FRate
  DisplayModels <- input$modelResultChoice
  DataView <- input$modelPlotChoice
  PlotView <- input$ModelDataPlotType
  PlotData <- input$checkboxDataOnPlot
  PlotDataEnd <- input$checkboxDataEndOnPlot
  RelMissionTime <- input$modelRelMissionTime
  AdditionalCurveLength <- input$modelCurveAdditionalTime
  PlotFault <- FALSE

  PlotModelEstim <- NULL
  PlotModelEstim[["Low"]] <- input$LowConfOnRsltPlot
  PlotModelEstim[["MLE"]] <- input$MLEOnRsltPlot
  PlotModelEstim[["High"]] <- input$HighConfOnRsltPlot

  # Suffix text that will be appended to the names of model result values
  # and parameter names in the table of model results.
  
  ConfIntSuffixes <- c("Low", "MLE", "High")
  
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
    # #print an error message.
    
    #print(msgModelDataViewUnknown)
    PlotFault <- TRUE
  }
  
  # Set up the time offset for the data that's going to be input to the models.
  # Since the model lines get updated each time the plot is redrawn, the models
  # need to be run each time the plot is redrawn.  We also need to set up an
  # offset for the starting failure number in case we're dealing with a subset
  # of the data.
  
  timeOffset <- DataModeled$FT[1]-DataModeled$IF[1]
  failureOffset <- DataModeled$FN[1]-1
  
  # Set up the vectors for the x-axis when drawing curves on the plot.
  
  if(!is.null(plotWidthRange) && !is.null(plotHeightRange)) {
    # We've zoomed in to a subset of the plot.  We don't need to specify the
    # x values for each model.
    
    startPoint <- max(plotWidthRange[1], DataModeled$FT[1])
    timeAxisLinePlotVals <- seq(from=startPoint, to=plotWidthRange[2], by=(plotWidthRange[2]-startPoint)/(plotPixels-1))
  }
  
  for (modelIndex in DisplayModels) {
    
    for (SuffixTag in ConfIntSuffixes) {
      
      if ((PlotModelEstim[[SuffixTag]]) && any(ModelsThatRan[[SuffixTag]] == modelIndex)) {
        # Get the model parameters.
        
        model_params <- c()
        model_params_label <- paste(modelIndex,"params",sep="_")
        for (parmIndex in 1:length(get(model_params_label))) {
          
          #model_params <- c(model_params, ModResults[[paste0(modelIndex, "_parm_", parmIndex)]][length(DataModeled[[1]])])
          
          model_parm_num <- paste0(modelIndex, "_", get(model_params_label)[parmIndex], "_", SuffixTag)
          model_params <- c(model_params, ModResults[[model_parm_num]][length(DataModeled[[1]])])
          
        }
        names(model_params) <- paste(modelIndex, get(paste0(modelIndex, "_params")), sep="_")
        
        # Create plot data, axes, and titles based on the view
        # of the data selected by the user (e.g., MTTFs)
        
        # Set up the vectors for the x-axis when drawing curves on the plot.
        # If we haven't already done this for a section of the plot that we're
        # zooming in to, we do it for the whole plot here.
        
        if(is.null(plotWidthRange) && is.null(plotHeightRange)) {
          # We're looking at the entire plot.
          
          xAxisVals <- unlist(subset(ModResults, !is.infinite(get(paste0(modelIndex, "_CumTime", "_", SuffixTag))), select=get(paste0(modelIndex, "_CumTime", "_", SuffixTag))), use.names=FALSE)
          IFVals <- unlist(DataModeled$IF, use.names=FALSE)
          timeAxisLinePlotVals <- seq(from=xAxisVals[1], to=xAxisVals[length(xAxisVals)]+AdditionalCurveLength, by=(xAxisVals[length(xAxisVals)]+AdditionalCurveLength-(xAxisVals[1]-IFVals[1]))/(plotPixels-1))
        }
        
        model_input_data <- data.frame("FT" = timeAxisLinePlotVals-timeOffset)
        
        if(DataView == "IF") {
          model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]], "Failure" = ModResults[[paste(modelIndex, "IF", SuffixTag, sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
          local_estimate <- get(paste(modelIndex,"MTTF",sep="_"))(model_params, model_input_data)[["MTTF"]]
          model_line_data <- model_plot_data
          # model_line_data <- data.frame("Time"= unlist(model_input_data+timeOffset, use.names=FALSE), "Failure"=local_estimate, "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(local_estimate)))
        } else if(DataView == "MVF") {
          model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]], "Failure" = ModResults[[paste(modelIndex, "MVF", SuffixTag,  sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
          local_estimate <- get(paste(modelIndex,"MVF",sep="_"))(model_params, model_input_data)[["Failure"]] + failureOffset
          model_line_data <- data.frame("Time"= timeAxisLinePlotVals, "Failure"=local_estimate, "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(local_estimate)))
          
          # Now we see if this is a case in which the model assumes a finite number of failures, and if we've
          # asked the model to predict ahead for more failures than the model thinks are left.
          
          if (is.infinite(ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]][length(ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]])])) {
            # model_line_data[length(model_line_data[,1])+1,] <- unlist(c(Inf, model_params[get(paste0(modelIndex, "_numfailsparm")[1])], get(paste(modelIndex, "fullname", sep="_"))), use.names=FALSE)
          }
        } else if(DataView == "FI") {
          model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]], "Failure" = ModResults[[paste(modelIndex, "FI", SuffixTag, sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
          local_estimate <- get(paste(modelIndex,"FI",sep="_"))(model_params, model_input_data)[["Failure_Rate"]]
          model_line_data <- data.frame("Time"= timeAxisLinePlotVals, "Failure"=local_estimate, "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(local_estimate)))
        } else if(DataView == "R") {
          model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]], "Failure" = ModResults[[paste(modelIndex, "Rel", SuffixTag, sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
        } else if(DataView == "R_growth") {
          
          # This is an interactive plot - users can change the mission time for
          # which reliability will be computed.  The plot will then be redrawn.
          
          rg_input_data <- data.frame("FT" = subset(ModResults, !is.infinite(get(paste0(modelIndex, "_CumTime", "_", SuffixTag))), select=get(paste0(modelIndex, "_CumTime", "_", SuffixTag)))-timeOffset)
          names(rg_input_data) <- c("FT")
          temp_R_growth <- data.frame("Reliability_Growth"=c(get(paste(modelIndex,"R_growth",sep="_"))(model_params, rg_input_data, RelMissionTime)[["Reliability_Growth"]], rep(1, length(ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]])-length(rg_input_data[[1]]))))
          model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]], "Failure" = temp_R_growth[["Reliability_Growth"]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
          local_estimate <- get(paste(modelIndex,"R_growth",sep="_"))(model_params, model_input_data, RelMissionTime)[["Reliability_Growth"]]
          model_line_data <- data.frame("Time"= timeAxisLinePlotVals, "Failure"=local_estimate, "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(local_estimate)))
          
        } else if (DataView == "FC") {
          model_plot_data <- data.frame("Time" = ModResults[[paste(modelIndex, "CumTime", SuffixTag, sep="_")]], "Failure" = ModResults[[paste(modelIndex, "FC", SuffixTag, sep="_")]], "Model" = rep(get(paste(modelIndex, "fullname", sep="_")), length(ModResults[["Failure"]])))
        } else {
          
          # Couldn't identify view of data to display.
          # #print an error message.
          
          #print(msgModelDataViewUnknown)
          PlotFault <- TRUE
        }
        
        scaleManBreaks <- c(scaleManBreaks, get(paste(modelIndex, "fullname", sep="_")))
        scaleManColors <- c(scaleManColors, get(paste(modelIndex, "plotcolor", sep="_")))
        
        if (PlotView == "points_and_lines") {
          localResultsPlot <- localResultsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model)) + geom_line(data=model_line_data, aes(Time,Failure,color=Model,linetype=Model))
        } else if (PlotView == "points") {
          localResultsPlot <- localResultsPlot + geom_point(data=model_plot_data,aes(Time,Failure,color=Model))
        } else if (PlotView == "lines") {
          localResultsPlot <- localResultsPlot + geom_line(data=model_line_data, aes(Time,Failure,color=Model,linetype=Model))
        } else {
          
          # Couldn't identify the plot type.
          # #print an error message.
          
          #print(paste0("plot_model_results: ", msgPlotTypeUnknown))
          PlotFault <- TRUE
        }
      } # Endif - plot only if the MLE or confidence bound checkbox on the plot is true
    } # End for - draw plots for MLE values and confidence bounds for current model.
  } # End for - draw results for all models
  
  
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
        # #print an error message.
        
        #print(msgInputDataTypeUnknown)
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
        # #print an error message.
        
        #print(msgModelDataViewUnknown)
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
        # #print an error message.
        
        #print(paste0("plot_model_results: ", msgPlotTypeUnknown))
        PlotFault <- TRUE
      }
    }
  }
  
  if(PlotDataEnd) {
    localResultsPlot <- localResultsPlot + geom_vline(xintercept=DataModeled$FT[length(DataModeled$FT)], linetype='longdash', alpha = 0.8)
  }
    
  
  #localResultsPlot <- localResultsPlot + scale_color_manual("", breaks=scaleManBreaks, values=scaleManColors)
  localResultsPlot <- localResultsPlot + theme(legend.position = "bottom", text = element_text(size=14))

  if(PlotFault) {
    localResultsPlot = NULL
  }
  return(localResultsPlot)
}

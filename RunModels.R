DataIntervalStart <- input$modelDataRange[1]
DataIntervalEnd <- input$modelDataRange[2]
InitialParmEndObs <- input$parmEstIntvl

# These two lists are used to keep track of models
# that executed successfully and those that did not.

ModelsExecutedList <<- list()
ModelsFailedExecutionList <<- list()

# Create the data structures that will hold model results as well as the
# data to which the models are applied.  The basic structure is a list
# of data frames, each data frame holding raw failure data or a set of
# model results.

# ModelResultsList is the list that will hold the model results.
# Each model's results will be a data frame containing
# the model's estimates and predictions as well as the
# parameter values.  If a model doesn't converge, the
# value NaN will be entered into its results frame

ModelResultsList <<- list()
tempResultsFrame <- data.frame()

if((DataIntervalEnd - DataIntervalStart + 1) >= K_minDataModelIntervalWidth) {
  # The results list will also hold the subsetted data on
  # which the models are run.
  
  input_data <- data.frame(x=data_global())
  names(input_data) <- gsub("x.", "", names(input_data))
  
  EmptyDataEntries <- rep(NA, input$modelNumPredSteps)
  
  # These are the data sets that will be input directly into the models.
  
  if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data)))>0)) {
    FN <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FN)), use.names=FALSE)
    FT <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
    IF <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
    
    tempResultsFrame <- data.frame("FN"=c(FN, EmptyDataEntries), "IF"=c(IF, EmptyDataEntries), "FT"=c(FT, EmptyDataEntries))
    ModelResultsList[["Data"]] <<- tempResultsFrame
    
    InitialModelPreds <- rep(NA, length(IF)+length(EmptyDataEntries))
    
    # Now run all of the models for the current data type and put the results
    # the list of results.

    names(IF) <- c(DataIntervalStart:DataIntervalEnd)
    for(ModelName in 1:length(K_IF_ModelsList)) {
      if(K_IF_ModelsList[ModelName] == "JM") {
        tempResultsFrame <- data.frame("JM_N0"=InitialModelPreds, "JM_PHI"=InitialModelPreds, "JM_IF"=InitialModelPreds, "JM_MVF"=InitialModelPreds, "JM_FI"=InitialModelPreds, "JM_REL"=InitialModelPreds)
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          ModelInputIF <- c(unlist(subset(IF, as.numeric(names(IF))<=index), use.names=FALSE))
          model_params <- JM_BM_MLE(ModelInputIF)
          if(!(model_params[1] == "nonconvergence")) {
            tempResultsFrame$JM_N0[index] <- model_params[1]
            tempResultsFrame$JM_PHI[index] <- model_params[2]
          } else {
            tempResultsFrame$JM_N0[index] <- NaN  # Indicates MLE non-convergence
            tempResultsFrame$JM_PHI[index] <- NaN  # Indicates MLE non-convergence
            ModelsFailedExecutionList$K_IF_ModelsList[ModelName] <<- K_IF_ModelsList[ModelName]
          }
        }
        if(length(names(ModelsFailedExecutionList)) > 0) {
          if(!(grep(K_IF_ModelsList[ModelName], names(ModelsFailedExecutionList)))) {
            ModelsExecutedList$K_IF_ModelsList[ModelName] <<- K_IF_ModelsList[ModelName]
            
            # Now we compute the MVF, IF and Reliability Estimates and Predictions
            # for this model.  We only do this if there were no instances of non-
            # convergences of the parameter estimates.
            
            ModelInputIF <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
            frame_params <- data.frame("N0"=c(model_params[1]),"Phi"=c(model_params[2]))
            mvf_plot_data <- JM_MVF(frame_params,ModelInputIF)
            tbf_plot_data <- JM_T(frame_params,ModelInputIF)
            fi_plot_data <- JM_FR(frame_params,ModelInputIF)
            rel_plot_data <- JM_R(frame_params,ModelInputIF)
          }
        } else {
          ModelInputIF <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
          frame_params <- data.frame("N0"=c(model_params[1]),"Phi"=c(model_params[2]))
          mvf_plot_data <- JM_MVF(frame_params,ModelInputIF)
          tbf_plot_data <- JM_T(frame_params,ModelInputIF)
          fi_plot_data <- JM_FR(frame_params,ModelInputIF)
          rel_plot_data <- JM_R(frame_params,ModelInputIF)
        }
        
      } else if(K_IF_ModelsList[ModelName] == "GM") {
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          
        }
      } else if(K_IF_ModelsList[ModelName] == "GO") {
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          
        }
      } else if(K_IF_ModelsList[ModelName] == "DSS") {
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          
        }
      } else if(K_IF_ModelsList[ModelName] == "WEI") {
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          
        }
      }
    }
  } else if((length(grep("CFC",names(input_data)))>0) || (length(grep("FC",names(input_data)))>0)) {
    FC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)), use.names=FALSE)
    CFC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)), use.names=FALSE)
    CumT <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)), use.names=FALSE)
    TI <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)), use.names=FALSE)
    
    # Do in-place conversion of FC to IF data - for the time being we'll run the IF/FT models on the converted data.
    # Later, as models are added, we can add models that use the FC data explicitly.
    
    FN <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FN)), use.names=FALSE)
    IF <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_IF)), use.names=FALSE)
    FT <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FT)), use.names=FALSE)
    IF_TI <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_TI)), use.names=FALSE)
    
    # Since we're using FC data converted to IF, we also have to find the failure numbers which most closely
    # matches the test intervals specified by DataIntervalStart and InitialParmEndObs.
    
    InitialModelPreds <- rep(NA, length(IF)+length(EmptyDataEntries))
    
    for (j in 1:length(IF_TI)) {
      if(IF_TI[j] >= InitialParmEndObs) {
        break
      }
    }
    IF_InitialParmEndObs <- j
    for (k in 1:length(IF_TI)) {
      if(IF_TI[j] >= DataIntervalStart) {
        break
      }
    }
    IF_DataIntervalStart <- k
    
    if((IF_InitialParmEndObs > IF_DataIntervalStart) && (IF_InitialParmEndObs < length(IF))) {
      tempResultsFrame <- data.frame("FN"=c(FN, EmptyDataEntries), "IF"=c(IF, EmptyDataEntries), "FT"=c(FT, EmptyDataEntries))
      ModelResultsList[["Data"]] <<- tempResultsFrame
      
      # Now run all of the models for the current data type and put the results
      # the list of results.
      
      for(ModelName in 1:length(K_FC_ModelsList)) {
        if(K_FC_ModelsList[ModelName] == "JM") {
          for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
            
          }
        } else if(K_FC_ModelsList[ModelName] == "GM") {
          for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
            
          }
        } else if(K_FC_ModelsList[ModelName] == "GO") {
          for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
            
          }
        } else if(K_FC_ModelsList[ModelName] == "DSS") {
          for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
            
          }
        } else if(K_FC_ModelsList[ModelName] == "WEI") {
          for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
            
          }
        }
      }
    }
  }
  # Clean up
  
  tempResultsFrame <- data.frame()
  
}
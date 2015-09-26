<<<<<<< HEAD
DataIntervalStart <- input$modelDataRange[1]
DataIntervalEnd <- input$modelDataRange[2]
InitialParmEndObs <- input$parmEstIntvl
ReliabilityEstimationInterval <- input$modelRelInterval

# Read the list of models to run.

SelectedModelsToRun <- as.list(input$modelsToRun)

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

# Store the start and end points of the data set and the number of
# failures for which to make predictions in "ModelResultsList".
# Also store the name of the data set that is being modeled.

tempResultsFrame <- data.frame("Start"=DataIntervalStart, "End"=DataIntervalEnd, "NumPreds"=input$modelNumPredSteps)
ModelResultsList[["DataStartAndEnd"]] <<- tempResultsFrame
ModelResultsList[["DataSetName"]] <<- data_set_global
ModelResultsList[["DataSetType"]] <<- data_set_global_type

input_data <- data.frame(x=data_global())
names(input_data) <- gsub("x.", "", names(input_data))

EmptyDataEntries <- rep(NA, input$modelNumPredSteps)

# These are the data sets that will be input directly into the models.

if(ModelResultsList[["DataSetType"]] == "IFTimes") {
  FN <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FN)), use.names=FALSE)
  FT <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
  IF <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
  
  if(DataIntervalStart == 1) {
    ModelResultsList[["TimeOffset"]] <<- 0
  } else {
    ModelResultsList[["TimeOffset"]] <<- input_data$FT[DataIntervalStart-1]
  }
  
  tempResultsFrame <- data.frame("FN"=c(FN, EmptyDataEntries), "IF"=c(IF, EmptyDataEntries), "FT"=c(FT, EmptyDataEntries))
  ModelResultsList[["Data"]] <<- tempResultsFrame
  
  InitialModelPreds <- rep(NA, length(IF)+length(EmptyDataEntries))
  
  # We need to set the names in the list of models to run.
  
  ModelsToRunNames <- c()
  for (index_temp in 1:length(SelectedModelsToRun)) {
    for (index_temp1 in 1:length(K_IF_ModelsList)) {
      if (SelectedModelsToRun[index_temp] == unlist(K_IF_ModelsList[index_temp1], use.names=FALSE)) {
        ModelsToRunNames[index_temp] <- names(K_IF_ModelsList[index_temp1])
      }
    }
  }
  names(SelectedModelsToRun) <- ModelsToRunNames
  
  # Now run all of the models for the current data type and put the results
  # the list of results.
  
  names(IF) <- c(DataIntervalStart:DataIntervalEnd)
  for(ModelListIndex in 1:length(SelectedModelsToRun)) {
    if(SelectedModelsToRun[ModelListIndex] == "JM") {
      tempResultsFrame <- data.frame("JM_N0"=InitialModelPreds, "JM_PHI"=InitialModelPreds, "IF"=InitialModelPreds, "MVF"=InitialModelPreds, "FI"=InitialModelPreds, "REL"=InitialModelPreds)
      ModelEstimatesConverged <- TRUE
      for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
        ModelInputData <- c(unlist(subset(IF, as.numeric(names(IF))<=index), use.names=FALSE))
        model_params <- JM_BM_MLE(ModelInputData)
        if(!(model_params[1] == "nonconvergence")) {
          tempResultsFrame$JM_N0[index] <- model_params[1]
          tempResultsFrame$JM_PHI[index] <- model_params[2]
        } else {
          tempResultsFrame$JM_N0[index] <- NaN  # Indicates MLE non-convergence
          tempResultsFrame$JM_PHI[index] <- NaN  # Indicates MLE non-convergence
          ModelEstimatesConverged <- FALSE
        }
      }
      if(ModelEstimatesConverged == TRUE) {
        ModelsExecutedList[[names(SelectedModelsToRun)[ModelListIndex]]] <<- unlist(SelectedModelsToRun[ModelListIndex], use.names=FALSE)
        
        # Now we compute the MVF, IF and Reliability Estimates and Predictions
        # for this model.  We only do this if there were no instances of non-
        # convergences of the parameter estimates.
        
        # First we have to adjust for the fact that this is a finite failures
        # model, and we may be asking for the model to make predictions for
        # more failures than the model thinks remain.
        
        if(length(IF)+length(EmptyDataEntries) < model_params[1]) {
          FillData <- EmptyDataEntries
          ModelPredsNA <- c()
          ModelPredsNaN <- c()
          ModelPredsInF <- c()
        } else {
          if(abs(model_params[1]-round(model_params[1])) < K_tol) {
            # N0 is a whole number
            FillData <- rep(NA, (model_params[1]-length(IF)-1))
          } else {
            # N0 is not a whole number
            FillData <- rep(NA, (floor(model_params[1])-length(IF)))
          }
          ModelPredsNA <- rep(NA, length(EmptyDataEntries)-length(FillData))
          ModelPredsNaN <- rep(NaN, length(EmptyDataEntries)-length(FillData))
          ModelPredsInF <- rep(Inf, length(EmptyDataEntries)-length(FillData))
        }
        
        ModelInputData <- data.frame("FT"=c(FT, FillData),"IF"=c(IF, FillData),"FN"=c(1:length(FT), FillData))
        frame_params <- data.frame("N0"=c(model_params[1]),"Phi"=c(model_params[2]))
        tempResultsFrame$MVF <- c(JM_MVF(frame_params,ModelInputData)[["Time"]], ModelPredsInF)
        tempResultsFrame$IF <- c(JM_T(frame_params,ModelInputData)[["Failure"]], ModelPredsInF)
        ModelInputData <- data.frame("FT"=subset(tempResultsFrame$MVF, tempResultsFrame$MVF != Inf),"IF"=c(IF, FillData),"FN"=c(1:length(FT), FillData))
        tempResultsFrame$FI <- c(JM_FR_alt1(frame_params,ModelInputData)[["FailureInt"]], ModelPredsNA)
        rel_plot_data <- JM_R(frame_params,ModelInputData)
      } else {
        ModelsFailedExecutionList[[names(SelectedModelsToRun)[ModelListIndex]]] <<- SelectedModelsToRun[ModelListIndex]
        ModelEstimatesConverged <- TRUE
      }
      
      # Add the data frame containing results for this model to the
      # list of data frames holding all model results and clean up.
      
      ModelResultsList[["JM"]] <<- tempResultsFrame
      tempResultsFrame <- data.frame()
      
    } else if(SelectedModelsToRun[ModelListIndex] == "GM") {
      for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
        tempResultsFrame <- data.frame("GM_D"=InitialModelPreds, "GM_PHI"=InitialModelPreds, "IF"=InitialModelPreds, "MVF"=InitialModelPreds, "FI"=InitialModelPreds, "REL"=InitialModelPreds)
        ModelEstimatesConverged <- TRUE
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          ModelInputData <- c(unlist(subset(IF, as.numeric(names(IF))<=index), use.names=FALSE))
          model_params <- GM_BM_MLE(ModelInputData)
          if(!(model_params[1] == "nonconvergence")) {
            tempResultsFrame$GM_D[index] <- model_params[1]
            tempResultsFrame$GM_PHI[index] <- model_params[2]
          } else {
            tempResultsFrame$GM_D[index] <- NaN  # Indicates MLE non-convergence
            tempResultsFrame$GM_PHI[index] <- NaN  # Indicates MLE non-convergence
            ModelEstimatesConverged <- FALSE
          }
        }
        if(ModelEstimatesConverged == TRUE) {
          ModelsExecutedList[[names(SelectedModelsToRun)[ModelListIndex]]] <<- unlist(SelectedModelsToRun[ModelListIndex], use.names=FALSE)
          
          # Now we compute the MVF, IF and Reliability Estimates and Predictions
          # for this model.  We only do this if there were no instances of non-
          # convergences of the parameter estimates.
          
          ModelInputData <- data.frame("FT"=c(FT, EmptyDataEntries),"IF"=c(IF, EmptyDataEntries),"FN"=c(1:length(FT), EmptyDataEntries))
          frame_params <- data.frame("D0"=c(model_params[1]),"Phi"=c(model_params[2]))
          tempResultsFrame$MVF <- c(GM_MVF(frame_params,ModelInputData)[["Time"]])
          tempResultsFrame$IF <- c(GM_T(frame_params,ModelInputData)[["Failure"]])
          tempFTVector <- interF_to_failureT(tail(unlist(tempResultsFrame$IF, use.names=FALSE), length(EmptyDataEntries)))
          ModelInputData <- data.frame("FT"=c(FT, (tempFTVector+FT[length(FT)])),"IF"=c(IF, EmptyDataEntries),"FN"=c(1:length(FT), EmptyDataEntries))
          tempResultsFrame$FI <- c(GM_FR_alt1(frame_params,ModelInputData)[["Failure"]])
          rel_plot_data <- GM_R(frame_params,ModelInputData)
        } else {
          ModelsFailedExecutionList[[names(SelectedModelsToRun)[ModelListIndex]]] <<- SelectedModelsToRun[ModelListIndex]
          ModelEstimatesConverged <- TRUE
        }
        
        # Add the data frame containing results for this model to the
        # list of data frames holding all model results and clean up.
        
        ModelResultsList[["GM"]] <<- tempResultsFrame
        tempResultsFrame <- data.frame()
      }
    } else if(SelectedModelsToRun[ModelListIndex] == "GO") {
      for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
        tempResultsFrame <- data.frame("GO_Alpha"=InitialModelPreds, "GO_Beta"=InitialModelPreds, "IF"=InitialModelPreds, "MVF"=InitialModelPreds, "FI"=InitialModelPreds, "REL"=InitialModelPreds)
        ModelEstimatesConverged <- TRUE
        for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
          ModelInputData <- c(unlist(subset(FT, as.numeric(names(IF))<=index), use.names=FALSE))
          model_params <- GO_BM_MLE(ModelInputData)
          if(!(model_params[1] == "nonconvergence")) {
            tempResultsFrame$GO_Alpha[index] <- model_params[1]
            tempResultsFrame$GO_Beta[index] <- model_params[2]
          } else {
            tempResultsFrame$GO_Alpha[index] <- NaN  # Indicates MLE non-convergence
            tempResultsFrame$GO_Beta[index] <- NaN  # Indicates MLE non-convergence
            ModelEstimatesConverged <- FALSE
          }
        }
        if(ModelEstimatesConverged == TRUE) {
          ModelsExecutedList[[names(SelectedModelsToRun)[ModelListIndex]]] <<- unlist(SelectedModelsToRun[ModelListIndex], use.names=FALSE)
          
          # Now we compute the MVF, IF and Reliability Estimates and Predictions
          # for this model.  We only do this if there were no instances of non-
          # convergences of the parameter estimates.
          
          # First we have to adjust for the fact that this is a finite failures
          # model, and we may be asking for the model to make predictions for
          # more failures than the model thinks remain.
          
          if(length(IF)+length(EmptyDataEntries) < model_params[1]) {
            FillData <- EmptyDataEntries
            ModelPredsNA <- c()
            ModelPredsNaN <- c()
            ModelPredsInF <- c()
          } else {
            if(abs(model_params[1]-round(model_params[1])) < K_tol) {
              # Alpha is a whole number
              FillData <- rep(NA, (model_params[1]-length(FT)-1))
            } else {
              # Alpha is not a whole number
              FillData <- rep(NA, (floor(model_params[1])-length(FT)))
            }
            ModelPredsNA <- rep(NA, length(EmptyDataEntries)-length(FillData))
            ModelPredsNaN <- rep(NaN, length(EmptyDataEntries)-length(FillData))
            ModelPredsInF <- rep(Inf, length(EmptyDataEntries)-length(FillData))
          }
          
          ModelInputData <- data.frame("FT"=c(FT, FillData),"IF"=c(IF, FillData),"FN"=c(1:length(FT), FillData))
          frame_params <- data.frame("aMLE"=c(model_params[1]),"bMLE"=c(model_params[2]))
          tempResultsFrame$MVF <- c(GO_BM_MVF_alt1(frame_params,ModelInputData)[["Time"]]+ModelResultsList[["TimeOffset"]], ModelPredsInF)
          ModelInputData <- data.frame("FT"=subset(tempResultsFrame$MVF, tempResultsFrame$MVF != Inf),"IF"=c(IF, FillData),"FN"=c(1:length(FT), FillData))
          tempResultsFrame$IF <- c(GO_T_alt1(frame_params,ModelInputData)[["Time"]], ModelPredsInF)
          tempResultsFrame$FI <- c(GO_FR_alt1(frame_params,ModelInputData)[["Time"]], ModelPredsNA)
          #rel_plot_data <- GO_R(frame_params,ModelInputData)
        } else {
          ModelsFailedExecutionList[[names(SelectedModelsToRun)[ModelListIndex]]] <<- SelectedModelsToRun[ModelListIndex]
          ModelEstimatesConverged <- TRUE
        }
        
        # Add the data frame containing results for this model to the
        # list of data frames holding all model results and clean up.
        
        ModelResultsList[["GO"]] <<- tempResultsFrame
        tempResultsFrame <- data.frame()
        
      }
    } else if(SelectedModelsToRun[ModelListIndex] == "DSS") {
      for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
        
      }
    } else if(SelectedModelsToRun[ModelListIndex] == "WEI") {
      for (index in (InitialParmEndObs-DataIntervalStart+1):length(IF)) {
        
      }
    }
  }
  
  # Update the model results selection pull-downs with the names of the
  # models that have been successfully run.
  
  updateSelectInput(session, "modelResultChoice", choices = ModelsExecutedList, selected=ModelsExecutedList[[names(ModelsExecutedList[1])]])
  updateSelectInput(session, "modelDetailChoice", choices = ModelsExecutedList, selected=ModelsExecutedList[[names(ModelsExecutedList[1])]])
  updateSelectInput(session, "modelResultsForEval", choices = ModelsExecutedList, selected=ModelsExecutedList[[names(ModelsExecutedList[1])]])
  
  AllModelsList <- c(ModelsExecutedList, ModelsFailedExecutionList)
  AllNames <- sort(names(AllModelsList))
  tempList <- list()
  for (index in 1:length(AllNames)) {
    tempList[index] <- AllModelsList[[AllNames[index]]]
  }
  names(tempList) <- AllNames
  updateSelectInput(session, "AllModelsRun", choices = tempList, selected=AllModelsList[[names(tempList[1])]])
  
} else if(ModelResultsList[["DataSetType"]] == "FailureCounts") {
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
  
  # We need to set the names in the list of models to run.
  
  ModelsToRunNames <- c()
  for (index_temp in 1:length(SelectedModelsToRun)) {
    for (index_temp1 in 1:length(K_FC_ModelsList)) {
      if (SelectedModelsToRun[index_temp] == unlist(K_FC_ModelsList[index_temp1], use.names=FALSE)) {
        ModelsToRunNames[index_temp] <- names(K_FC_ModelsList[index_temp1])
      }
    }
  }
  names(SelectedModelsToRun) <- ModelsToRunNames
  
  
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
    
    for(ModelListIndex in 1:length(SelectedModelsToRun)) {
      if(SelectedModelsToRun[ModelListIndex] == "JM") {
        for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
          
        }
      } else if(SelectedModelsToRun[ModelListIndex] == "GM") {
        for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
          
        }
      } else if(SelectedModelsToRun[ModelListIndex] == "GO") {
        for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
          
        }
      } else if(SelectedModelsToRun[ModelListIndex] == "DSS") {
        for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
          
        }
      } else if(SelectedModelsToRun[ModelListIndex] == "WEI") {
        for (index in (IF_InitialParmEndObs-IF_DataIntervalStart+1):length(IF)) {
          
        }
      }
    }
  }
}
# Clean up

tempResultsFrame <- data.frame()
  
=======
library(rootSolve)

run_models <- function(raw_data, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, tol_local) {
  
  in_data <- raw_data
  if (dataType(names(in_data)) == "FR") {
    in_data$FT <- in_data$FT - OffsetTime
  } else {
    # Need to complete for failure counts data
  }
  DataStart <- DataRange[1]
  DataEnd <- DataRange[2]
  localEstIntvlEnd <- ParmInitIntvl-DataStart+1
  
  # Set up two local vectors to hold the names of models that completed
  # successfully and those that did not.
  
  PlottableModels <- c()
  UnplottableModels <- c()
  
  local_results <- data.frame("Failure"=c(DataStart:(DataEnd+PredAheadSteps)))
  
  # Set up a vector of fill data (use NA for fill)
  
  naFill <- rep(NA, length(in_data[[1]])+PredAheadSteps)
  NaNFill <- rep(NaN, length(in_data[[1]])+PredAheadSteps)
  
  # Now run the models identified in Models2Run
  
  if(dataType(names(in_data))=="FR"){
    for(modelID in Models2Run) {
      
      # First set up the columns in the results data frame that will hold parameters estimates and predictions.
      
      for (paramNum in 1:length(get(paste(modelID,"params",sep="_")))) {
        local_results[[paste0(modelID, "_parm_", paramNum)]] <- naFill
      }
      local_results[[paste0(modelID, "_MVF")]] <- NaNFill
      local_results[[paste0(modelID, "_IF")]] <- NaNFill
      local_results[[paste0(modelID, "_FI")]] <- NaNFill
      local_results[[paste0(modelID, "_Rel")]] <- NaNFill
      
      ParmEstimatesConverged <- TRUE
      for (failure_num in c(localEstIntvlEnd:length(in_data[[1]]))) {
        model_params <- get(paste(modelID,get(paste(modelID,"methods",sep="_"))[1],"MLE",sep="_"))(head(get(paste("in_data"))[[get(paste(modelID,"input",sep="_"))]], failure_num))
        
        # Now put the parameter estimates into the results frame
        
        for (paramNum in 1:length(get(paste(modelID,"params",sep="_")))) {
          if(typeof(model_params)!="character") {
            local_results[[paste0(modelID, "_parm_", paramNum)]][failure_num] <- model_params[paramNum]
          } else {
            # The model results didn't converge.  Use NaN to indicate nonconvergence.
            
            local_results[[paste0(modelID, "_parm_", paramNum)]][failure_num] <- NaN
            
            # Also indicate that this is a model that won't be displayed on the plot.
            
            ParmEstimatesConverged <- FALSE
          }
        } # End for - we've estimated the parameters for the current model for the current failure.
      } # End for - we've estimated model parameters for the current model over the entire dataset.
      
      if(ParmEstimatesConverged) {
        PlottableModels <- c(PlottableModels, modelID)
        
        # Here we compute the model estimates of MVF, IF, FI, and Reliability.
        # First we create empty fill vectors into which we may need to add
        # values for finite-failures models.  See below.
        
        ModelPredsNA <- c()
        ModelPredsNaN <- c()
        ModelPredsInF <- c()
        ModelPredsZero <- c()
        FillData <- rep(NA, PredAheadSteps)
        EmptyDataEntries <- rep(NA, PredAheadSteps)
        
        # The next thing we do is determine whether this is a finite-failures
        # model.  If it is, we may have to add some fill onto the end of the
        # predictions vector we get, because we may have asked the model to
        # make predictions for more future failures than the model thinks
        # there actually are.
        
        if (get(paste(modelID,"failcount",sep="_"))[1] == "finite") {
          ExpectedTotalFailures <- model_params[get(paste(modelID,"numfailsparm",sep="_"))[1]]
          if(DataEnd-DataStart+1+PredAheadSteps < ExpectedTotalFailures) {
            FillData <- rep(NA, PredAheadSteps)
            EmptyDataEntries <- rep(NA, PredAheadSteps)
          } else {
            # Here we take care of the situation in which we're asking for
            # predictions further ahead than the model thinks there are
            # failures left to discover.
            
            if(abs(ExpectedTotalFailures-round(ExpectedTotalFailures)) < tol_local) {
              
              # The model's expected number of failures is a whole number
              
              FillData <- rep(NA, (ExpectedTotalFailures-length(in_data[[1]])-1))
            } else {
              # The model's expected number of failures is not a whole number
              
              FillData <- rep(NA, (floor(ExpectedTotalFailures)-length(in_data[[1]])))
            }
            ModelPredsNA <- rep(NA, PredAheadSteps-length(FillData))
            ModelPredsNaN <- rep(NaN, PredAheadSteps-length(FillData))
            ModelPredsInF <- rep(Inf, PredAheadSteps-length(FillData))
            ModelPredsZero <- rep(0, PredAheadSteps-length(FillData))
          }
        } # Endif - are we working with a finite or infinite failures model?
        
        # Compute the MVF, IF, FI, and Reliability functions for the model.
        
        pred_input_data <- data.frame("IF" = c(in_data[["IF"]], FillData), "FT" = c(in_data[["FT"]], FillData))
        local_results[[paste0(modelID, "_MVF")]] <- c(get(paste(modelID,"MVF",sep="_"))(model_params, pred_input_data)[["Time"]]+OffsetTime, ModelPredsInF)
        pred_input_data <- data.frame("IF" = c(in_data[["IF"]], FillData), "FT" = head(local_results[[paste0(modelID, "_MVF")]], length(in_data[["FT"]])+length(FillData)))
        local_results[[paste0(modelID, "_FI")]] <- c(get(paste(modelID,"FI",sep="_"))(model_params, pred_input_data)[["Time"]], ModelPredsZero)
        local_results[[paste0(modelID, "_IF")]] <- c(get(paste(modelID,"MTTF",sep="_"))(model_params, pred_input_data)[["Time"]], ModelPredsInF)
        #local_results[[paste0(modelID, "_Rel")]] <- NaNFill
        pred_input_data <- NULL
        
      } else {
        UnplottableModels <- c(UnplottableModels, modelID)
      }
    } # End for - we've applied all of the selected models to the entire dataset.
  } else if (dataType(names(in_data))=="FC") {
    
    # FC models need to be completed.
  } else {
    
    # We should never get here.  If we do, that means that it couldn't
    # be determined whether the input data was TTFs or FCs.
    
    print("Type of input data for the models could not be determined.")
  }
  
  # Return model results here, as well as the
  # vectors of plottable and unplottable models.
  # This is all packaged up in a list.
  
  return(list("Results"=local_results, "SuccessfulModels"=PlottableModels, "FailedModels"=UnplottableModels))
  
}
>>>>>>> allen-development

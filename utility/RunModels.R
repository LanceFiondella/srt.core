library(rootSolve)

#run_models <- function(raw_data, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local) {
run_models <- function(raw_data, input, tol_local) {
  #print(names(raw_data))
  DataRange <- input$modelDataRange
  PredAheadSteps <<- input$modelNumPredSteps
  Models2Run <- input$modelsToRun
  RelMissionTime <- input$modelRelMissionTime
  

  if (("FRate" %in% (names(raw_data))) && !("FCount" %in% (names(raw_data)))) {

    dataType <- "FT"

      if(DataRange[1] == 1) {
          OffsetTime <- 0
        } else {
          OffsetTime <- tail(head(raw_data$FRate, DataRange[1]-1), 1)[["FT"]]
        }
    ModeledData <<- tail(head(raw_data$FRate, DataRange[2]), (DataRange[2]-DataRange[1]+1))
    ModeledData$FT <- ModeledData$FT - OffsetTime
    ParmInitIntvl <- length(ModeledData[,1])
    results <- process_models(raw_data, ModeledData, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local, dataType)
  
  
  } else if (("FRate" %in% (names(raw_data))) && ("FCount" %in% (names(raw_data)))) {
    
    #Runs all FT models if there are failed models in the results, only those are run using FC models

    dataType <- "FT"
        if(DataRange[1] == 1) {
            OffsetTime <- 0
          } else {
            OffsetTime <- tail(head(raw_data$FRate, DataRange[1]-1), 1)[["FT"]]
          }
        ModeledData <<- tail(head(raw_data$FRate, DataRange[2]), (DataRange[2]-DataRange[1]+1))
        ModeledData$FT <- ModeledData$FT - OffsetTime
        ParmInitIntvl <- length(ModeledData[,1])
        results <- process_models(raw_data, ModeledData, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local, dataType)
    
    if (length(results[["FailedModels"]]) > 0){
      dataType <- "FC"
      if(DataRange[1] == 1) {
          OffsetTime <- 0
      } else {
          OffsetTime <- tail(head(raw_data$FCount, DataRange[1]-1), 1)[["FT"]]
      }
      #ModeledData <<- tail(head(raw_data$FCount, DataRange[2]), (DataRange[2]-DataRange[1]+1))
      ModeledData <<- tail(head(raw_data$FRate, DataRange[2]), (DataRange[2]-DataRange[1]+1))
      ParmInitIntvl <- length(ModeledData[,1])
      results <- process_models(raw_data, ModeledData, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, results[["FailedModels"]], RelMissionTime, tol_local, dataType)    
    }
  }
  return(results)
}


process_models <- function(raw_data, in_data, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local, dataType){
  DataStart <- DataRange[1]
  DataEnd <- DataRange[2]
  OffsetFailure <- DataStart-1
  localEstIntvlEnd <- ParmInitIntvl-DataStart+1
  

  # Set up two local vectors to hold the names of models that completed
  # successfully and those that did not.
  
  PlottableModels <- c()
  UnplottableModels <- c()
  
  local_results <- data.frame("Failure"=c((DataStart:DataEnd), rep(NA,PredAheadSteps)))
  
  # Set up a vector of fill data (use NA for fill)
  
  naFill <- rep(NA, length(in_data[[1]])+PredAheadSteps)
  NaNFill <- rep(NaN, length(in_data[[1]])+PredAheadSteps)

  for(modelID in Models2Run) {
      # First set up the columns in the results data frame that will hold parameters estimates and predictions.
      
      #Setup labels
      model_params_label <- paste(modelID,"params",sep="_")
      model_CumTime <- paste0(modelID, "_CumTime") 
      model_MVF  <- paste0(modelID, "_MVF")
      model_IF <- paste0(modelID, "_IF")
      model_FI <- paste0(modelID, "_FI")
      model_Rel <- paste0(modelID, "_Rel")
      model_MVF_inv <- paste(modelID,"MVF_inv",sep="_")
      model_R_growth <- paste0(modelID, "_R_growth")
      model_MTTF <- paste(modelID,"MTTF",sep="_")
      model_lnL <- paste(modelID,dataType,"lnL",sep="_")

      for (paramNum in 1:length(get(model_params_label))) {
        #model_parm_num <- paste0(modelID, "_parm_", paramNum)
        model_parm_num <- paste0(modelID, "_", get(model_params_label)[paramNum])
        local_results[[model_parm_num]] <- naFill
      }
      local_results[[model_CumTime]] <- NaNFill
      local_results[[model_MVF]] <- NaNFill
      local_results[[model_IF]] <- NaNFill
      local_results[[model_FI]] <- NaNFill
      local_results[[model_Rel]] <- NaNFill
      
      ParmEstimatesConverged <- TRUE
      for (failure_num in c(localEstIntvlEnd:length(in_data[[1]]))) {
        model_methods <- paste(modelID,"methods",sep="_")
        sel_method <- NA
        model_input <- paste(modelID,"input",sep="_")
        
        for (method in get(model_methods)){
          
          model_sm_MLE <- paste(modelID, method, dataType, "MLE",sep="_")
          if(dataType == "FC" && dataType %in% get(model_input)){
            tVec <- head(raw_data$FCount$T, failure_num)
            kVec <- head(raw_data$FCount$FC, failure_num)
            temp_params <- get(model_sm_MLE)(tVec, kVec)
          } else if (dataType == "FT" && dataType %in% get(model_input)){
            tVec <- head(in_data$FT, failure_num)
            temp_params <- get(model_sm_MLE)(tVec)
          } else if ("IF" %in% get(model_input)){
            IF <- head(in_data$IF, failure_num)
            model_sm_MLE <- paste(modelID, method, "IF", "MLE",sep="_")
            temp_params <-get(model_sm_MLE)(IF)
          }
          
          #temp_lnL <- get(model_lnL)(tVec,temp_params)
          if(!anyNA(temp_params)){
            #lnL_value <- temp_lnL
            model_params <- temp_params
            sel_method <- method
            break
          }
        }

        if(is.na(sel_method)){
          print("None of the algorithms work")
          ParmEstimatesConverged <- FALSE
        }
        
        #print(paste0("Selected method for ",modelID))
        #print(sel_method)

        

        #This for loop selects one of the methods indicated in the Model_Specifications.R file.
        

        
        # Now put the parameter estimates into the results frame
        
        for (paramNum in 1:length(get(model_params_label))) {
            #model_parm_num <- paste0(modelID, "_parm_", paramNum) 
            model_parm_num <- paste0(modelID, "_", get(model_params_label)[paramNum])
            if(typeof(model_params)!="character") {
              #print(model_params)
              #print("Type of param number : ")
              #print(modelID)
              #print(model_params_label)
              #print(paramNum)
              #print(typeof(model_params[[paramNum]]))
              local_results[[model_parm_num]][failure_num] <- model_params[paramNum]
            } 
            else {
              # The model results didn't converge.  Use NaN to indicate nonconvergence.
              local_results[[model_parm_num]][failure_num] <- NaN
              # Also indicate that this is a model that won't be displayed on the plot.
              ParmEstimatesConverged <- FALSE
            }
        } # End for - we've estimated the parameters for the current model for the current failure.

      } # End for - we've estimated model parameters for the current model over the entire dataset.
      
      in_data <- raw_data$FRate

      if(ParmEstimatesConverged) {
        
        PlottableModels <- c(PlottableModels, modelID)
        
        # Here we compute the model estimates of MVF, IF, FI, and Reliability.
        # First we create empty fill vectors into which we may need to add
        # values for finite-failures models.  See below.
        
        ModelPredsNA <- c()
        ModelPredsNaN <- c()
        ModelPredsInf <- c()
        ModelPredsZero <- c()
        ModelPredsOnes <- c()
        FillData <- rep(NA, PredAheadSteps)

        # Compute the MVF, IF, FI, Reliability, and Reliability Growth functions for the model.
        
        pred_input_data <- data.frame("IF" = in_data[["IF"]], "FT" = in_data[["FT"]])
        
        # First estimate MVF, then forecast.
        local_estim <- get(model_MVF)(model_params, pred_input_data)[["Failure"]]
        
        # The next thing we do is determine whether this is a finite-failures
        # model.  If it is, we may have to add some fill onto the end of the
        # predictions vector we get, because we may have asked the model to
        # make predictions for more future failures than the model thinks
        # there actually are.
        
        if (get(paste(modelID,"Finite",sep="_"))) {
          ExpectedTotalFailures <- model_params[get(paste(modelID,"numfailsparm",sep="_"))[1]]
          
          if(abs(local_estim[length(local_estim)]-round(local_estim[length(local_estim)])) < tol_local) {
            lower_pred_bound <- local_estim[length(local_estim)]+1
          } else {
            lower_pred_bound <- round(local_estim[length(local_estim)]+1)
            
          }
          
          if(PredAheadSteps < ExpectedTotalFailures-(DataEnd-DataStart+1)) {
            invMVFinput <- c(lower_pred_bound:(lower_pred_bound+PredAheadSteps-1))
            FillData <- c()
          } else {
            # Here we take care of the situation in which we're asking for
            # predictions further ahead than the model thinks there are
            # failures left to discover.
            
            if(lower_pred_bound < ExpectedTotalFailures) {
              if(abs(ExpectedTotalFailures-round(ExpectedTotalFailures)) < tol_local) {
                
                # The model's expected number of failures is a whole number
                invMVFinput <- c(lower_pred_bound:(floor(ExpectedTotalFailures)-1))
                
              } else {
                # The model's expected number of failures is not a whole number
                
                invMVFinput <- c(lower_pred_bound:as.integer(ExpectedTotalFailures))
                
              }
            } else {
              invMVFinput <- c()
            }
            ModelPredsNA <- rep(NA, PredAheadSteps-length(invMVFinput))
            ModelPredsNaN <- rep(NaN, PredAheadSteps-length(invMVFinput))
            ModelPredsInf <- rep(Inf, PredAheadSteps-length(invMVFinput))
            ModelPredsZero <- rep(0, PredAheadSteps-length(invMVFinput))
            ModelPredsOnes <- rep(1, PredAheadSteps-length(invMVFinput))
          }
        } else {
          ExpectedTotalFailures <- 0
          invMVFinput <- c((floor(local_estim[length(local_estim)])+1):(floor(local_estim[length(local_estim)])+PredAheadSteps))
        } # Endif - are we working with a finite or infinite failures model?

        if(length(invMVFinput) > 0) {
          pred_input_data <- data.frame("FN" = invMVFinput)
          local_results[[model_CumTime]] <- c(in_data[["FT"]]+OffsetTime, get(model_MVF_inv)(model_params, pred_input_data)[["Time"]]+OffsetTime, ModelPredsInf)
          local_results[[model_MVF]] <- c(local_estim+OffsetFailure, invMVFinput+OffsetFailure, rep(as.numeric(ExpectedTotalFailures+OffsetFailure), length(ModelPredsNA)))
        } else {
          local_results[[model_CumTime]] <- c(in_data[["FT"]]+OffsetTime, ModelPredsInf)
          local_results[[model_MVF]] <- c(local_estim+OffsetFailure, rep(as.numeric(ExpectedTotalFailures+OffsetFailure), PredAheadSteps))
        }

        pred_input_data <- data.frame("FT" = subset(local_results, !is.infinite(get(paste0(modelID, "_CumTime"))), select=get(paste0(modelID, "_CumTime")))-OffsetTime)
        names(pred_input_data) <- c("FT")
        
        if(any(sapply(model_params,is.finite) )== TRUE){
                
              local_results[[model_FI]] <- try(c(get(model_FI)(model_params, pred_input_data)[["Failure_Rate"]], ModelPredsZero),silent=TRUE)
              
              local_results[[model_IF]] <- try(c(get(model_MTTF)(model_params, pred_input_data)[["MTTF"]], ModelPredsInf),silent=TRUE)
              
              local_results[[model_R_growth]] <- try(c(get(model_R_growth)(model_params, pred_input_data, RelMissionTime)[["Reliability_Growth"]], ModelPredsOnes), silent=TRUE)
        
        }
        
        pred_input_data <- NULL
        
      } else {
        UnplottableModels <- c(UnplottableModels, modelID)
      }
    } # End for - we've applied all of the selected models to the entire dataset.
    return(list("Results"=local_results, "SuccessfulModels"=PlottableModels, "FailedModels"=UnplottableModels))
}

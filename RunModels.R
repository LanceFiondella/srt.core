library(rootSolve)

run_models <- function(raw_data, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local) {
  
  in_data <- raw_data
  if (dataType(names(in_data)) == "FR") {
    in_data$FT <- in_data$FT - OffsetTime
  } else {
    # Need to complete for failure counts data
  }
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
  
  # Now run the models identified in Models2Run
  
  if(dataType(names(in_data))=="FR"){
    for(modelID in Models2Run) {
      # First set up the columns in the results data frame that will hold parameters estimates and predictions.
      
      for (paramNum in 1:length(get(paste(modelID,"params",sep="_")))) {
        local_results[[paste0(modelID, "_parm_", paramNum)]] <- naFill
      }
      local_results[[paste0(modelID, "_CumTime")]] <- NaNFill
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
        ModelPredsInf <- c()
        ModelPredsZero <- c()
        ModelPredsOnes <- c()
        FillData <- rep(NA, PredAheadSteps)

        # Compute the MVF, IF, FI, Reliability, and Reliability Growth functions for the model.
        
        pred_input_data <- data.frame("IF" = in_data[["IF"]], "FT" = in_data[["FT"]])
        
        # First estimate MVF, then forecast.
        local_estim <- get(paste(modelID,"MVF",sep="_"))(model_params, pred_input_data)[["Failure"]]
        
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
            lower_pred_bound <- floor(local_estim[length(local_estim)])+1
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
          local_results[[paste0(modelID, "_CumTime")]] <- c(in_data[["FT"]]+OffsetTime, get(paste(modelID,"MVF_inv",sep="_"))(model_params, pred_input_data)[["Time"]]+OffsetTime, ModelPredsInf)
          local_results[[paste0(modelID, "_MVF")]] <- c(local_estim+OffsetFailure, invMVFinput+OffsetFailure, rep(as.numeric(ExpectedTotalFailures+OffsetFailure), length(ModelPredsNA)))
        } else {
          local_results[[paste0(modelID, "_CumTime")]] <- c(in_data[["FT"]]+OffsetTime, ModelPredsInf)
          local_results[[paste0(modelID, "_MVF")]] <- c(local_estim+OffsetFailure, rep(as.numeric(ExpectedTotalFailures+OffsetFailure), PredAheadSteps))
        }

        pred_input_data <- data.frame("FT" = subset(local_results, !is.infinite(get(paste0(modelID, "_CumTime"))), select=get(paste0(modelID, "_CumTime")))-OffsetTime)
        names(pred_input_data) <- c("FT")
        local_results[[paste0(modelID, "_FI")]] <- c(get(paste(modelID,"FI",sep="_"))(model_params, pred_input_data)[["Failure_Rate"]], ModelPredsZero)
        local_results[[paste0(modelID, "_IF")]] <- c(get(paste(modelID,"MTTF",sep="_"))(model_params, pred_input_data)[["MTTF"]], ModelPredsInf)
        
        local_results[[paste0(modelID, "_R_growth")]] <-c(get(paste(modelID,"R_growth",sep="_"))(model_params, pred_input_data, RelMissionTime)[["Reliability_Growth"]], ModelPredsOnes)
        
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
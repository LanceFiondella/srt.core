library(rootSolve)

run_models <- function(raw_data, DataRange, parmConfInterval, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local) {
  
  in_data <- raw_data
  if (dataType(names(in_data)) == "FR") {
    in_data$FT <- in_data$FT - OffsetTime
    results <- run_FR_models(in_data, DataRange, parmConfInterval, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local)
  } else if (dataType(names(in_data))=="FC") {
    # Need to complete for failure counts data
    results <- run_FC_models()
  }
  
  # Return model results here, as well as the
  # vectors of plottable and unplottable models.
  # This is all packaged up in a list.
  
  #return(list("Results"=local_results, "SuccessfulModels"=PlottableModels, "FailedModels"=UnplottableModels))
  return(results)
  
}

run_FC_models <- function(){

  
}

run_FR_models <- function(in_data, DataRange, ParmConfInterval, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local){
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
      model_lnL <- paste(modelID,"lnL",sep="_")

      for (paramNum in 1:length(get(model_params_label))) {
        model_parm_num = paste0(modelID, "_parm_", paramNum)
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
        
        tVec <- head(in_data[[get(model_input)]], failure_num)

        #This for loop selects one of the methods indicated in the Model_Specifications.R file.
        
        lnL_value <- Inf
        for (method in get(model_methods)){
          model_sm_MLE <- paste(modelID,method,"MLE",sep="_")    
          temp_params <- get(model_sm_MLE)(tVec)
          temp_lnL <- get(model_lnL)(as.list(temp_params),tVec,FALSE)
          if(!anyNA(temp_params)){
            lnL_value <- temp_lnL
            model_params <- temp_params
            sel_method <- method
            break
          }
        }
        
        if(is.na(sel_method)){
          print("None of the algorithms work")
          ParmEstimatesConverged <- FALSE
        }
        
        print("Selected method")
        print(sel_method)
        
        # Now put the parameter estimates into the results frame
        
        for (paramNum in 1:length(get(model_params_label))) {
            model_parm_num <- paste0(modelID, "_parm_", paramNum) 
            if(typeof(model_params)!="character") {
              
              # Now we estimate confidence bounds for the model parameters.
              
              if(paramNum == 1) {
                
                # Test code
                print(unlist(c(model_sm_MLE, length(tVec))))
                fit<-optim(model_params,x=tVec,NegLnL=TRUE,get(model_lnL),hessian=T,method="Nelder-Mead")
                se<-sqrt(diag(solve(fit$hessian)))
                CritValue<-qnorm(0.5+ParmConfInterval/2)
                lowerConfBound<-model_params-CritValue*se
                upperConfBound<-model_params+CritValue*se
                print(unlist(c(model_sm_MLE,length(tVec),model_params-fit$par,lowerConfBound,model_params,upperConfBound),use.names=FALSE))
                # End test code
              }
              local_results[[model_parm_num]][failure_num] <- model_params[paramNum]
            } 
            else {
              # The model results didn't converge.  Use NaN to indicate nonconvergence.
              local_results[[model_parm_num]][failure_num] <- NaN
              # Also indicate that this is a model that won't be displayed on the plot.
              #print("Models that didnt converge")
              #print(modelID)
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
            print(modelID)
            print(lower_pred_bound)
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

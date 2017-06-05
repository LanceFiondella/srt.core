library(rootSolve)
library(numDeriv)

#run_models <- function(raw_data, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local) {
run_models <- function(raw_data, input, tol_local) {
  print(names(raw_data))
  DataRange <- input$modelDataRange
  PredAheadSteps <<- input$modelNumPredSteps
  Models2Run <- input$modelsToRun
  RelMissionTime <- input$modelRelMissionTime
  ParmConfIntvl <- input$parmConfInterval
  

  if (("FRate" %in% (names(raw_data))) && !("FCount" %in% (names(raw_data)))) {

    dataType <- "FT"

      if(DataRange[1] == 1) {
          OffsetTime <- 0
        } else {
          OffsetTime <- tail(head(raw_data$FRate, DataRange[1]-1), 1)[["FT"]]
        }
    ModeledData <<- tail(head(raw_data$FRate, DataRange[2]), (DataRange[2]-DataRange[1]+1))
    ModeledData$FT <- ModeledData$FT - OffsetTime
    #ParmInitIntvl <- length(ModeledData[,1])
    ParmInitIntvl <- input$parmEstIntvl
    results <- process_models(raw_data, ModeledData, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local, dataType, ParmConfIntvl)

  } else if (("FRate" %in% (names(raw_data))) && ("FCount" %in% (names(raw_data)))) {
    # Need to complete for failure counts data
    #Runs all FC models if there are failed models in the results, only those are run using FR models
    
    dataType <- "FC"
    if(DataRange[1] == 1) {
          OffsetTime <- 0
        } else {
          OffsetTime <- tail(head(raw_data$FCount, DataRange[1]-1), 1)[["FT"]]
        }
    #ModeledData <<- tail(head(raw_data$FCount, DataRange[2]), (DataRange[2]-DataRange[1]+1))
    ModeledData <<- tail(head(raw_data$FRate, DataRange[2]), (DataRange[2]-DataRange[1]+1))
    #ParmInitIntvl <- length(ModeledData[,1])
    ParmInitIntvl <- input$parmEstIntvl
    results <- process_models(raw_data, ModeledData, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local, dataType, ParmConfIntvl)
    
    if (length(results[["FailedModels"]]) > 0){
      dataType <- "FT"
      if(DataRange[1] == 1) {
          OffsetTime <- 0
        } else {
          OffsetTime <- tail(head(raw_data$FRate, DataRange[1]-1), 1)[["FT"]]
        }
    ModeledData <<- tail(head(raw_data$FRate, DataRange[2]), (DataRange[2]-DataRange[1]+1))
    ModeledData$FT <- ModeledData$FT - OffsetTime
    #ParmInitIntvl <- length(ModeledData[,1])
    ParmInitIntvl <- input$parmEstIntvl
    results <- process_models(raw_data, ModeledData, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, results[["FailedModels"]], RelMissionTime, tol_local, dataType, ParmConfIntvl)

    }
  }
  return(results)
}



# Estimate parameter confidence interval using the hessian and Fisher information.
# This function can be used for both FT and FC types of models.

estim_conf_int <- function(in_model_lnL, in_model_params, in_param_names, in_ParmConfIntvl, in_fail_data){
  # First set the lower and upper confidence bounds to NaN.  This will indicate
  # that we're not able to compute the confidence bound values using the hessian
  # and Fisher information.
  
  out_lowerConfBound <- rep(0/0,length(in_model_params))
  out_upperConfBound <- out_lowerConfBound
  
  options(show.error.messages=FALSE)
  modelHessian <- try(numDeriv::hessian(f=get(in_model_lnL),x=as.numeric(in_model_params),paramNames=names(in_model_params),negLnL=TRUE,failData=in_fail_data,"complex"),silent=TRUE)
  options(show.error.messages=TRUE)
  if(is.numeric(modelHessian) && (is.nan(sum(modelHessian)) == FALSE) && (det(modelHessian) != 0)) {
    options(show.error.messages=FALSE)
    modelFisher <- try(Matrix::solve(modelHessian,diag(length(in_model_params))),silent=TRUE)
    options(show.error.messages=TRUE)
    if(is.numeric(modelFisher) && (is.nan(sum(modelFisher)) == FALSE)) {
      if (all(diag(modelFisher) > 0) == TRUE) {
        se <- sqrt(diag(modelFisher))
        CritValue<-qnorm(0.5+in_ParmConfIntvl/2)
        out_lowerConfBound<-in_model_params-CritValue*se
        out_upperConfBound<-in_model_params+CritValue*se
      }
    }
  }
  ConfBoundsList <- c()
  ConfBoundsList$LowerBoundsValues <- out_lowerConfBound
  ConfBoundsList$UpperBoundsValues <- out_upperConfBound
  return(ConfBoundsList)
}




process_models <- function(raw_data, in_data, DataRange, ParmInitIntvl, OffsetTime, PredAheadSteps, Models2Run, RelMissionTime, tol_local, dataType, ConfInterval){
  DataStart <- DataRange[1]
  DataEnd <- DataRange[2]
  OffsetFailure <- DataStart-1
  localEstIntvlEnd <- ParmInitIntvl-DataStart+1
  
  # Suffix text that will be appended to the names of model result values
  # and parameter names in the table of model results.
  
  ConfIntSuffixes <- c("Low", "MLE", "High")
  ModelResultType <- c("CumTime", "MVF", "IF", "FI", "R_growth", "Rel")

  # Set up local vectors to hold the names of models that completed
  # successfully and those that did not.  There are vectors to hold
  # this information for MLEs as well as high and low confidence bounds.
  
  PlottableModels <- c()
  PlottableModelsLow <- c()
  PlottableModelsMLE <- c()
  PlottableModelsHigh <- c()

  UnplottableModels <- c()
  UnplottableModelsLow <- c()
  UnplottableModelsMLE <- c()
  UnplottableModelsHigh <- c()
  
  local_results <- data.frame("Failure"=c((DataStart:DataEnd), rep(NA,PredAheadSteps)))
  
  # Set up a vector of fill data (use NA for fill)
  
  naFill <- rep(NA, length(in_data[[1]])+PredAheadSteps)
  NaNFill <- rep(NaN, length(in_data[[1]])+PredAheadSteps)

  for(modelID in Models2Run) {
    model_params_label <- paste(modelID,"params",sep="_")
    model_lnL <- paste(modelID,dataType,"lnL",sep="_")
    ParmEstimatesConverged <- NULL
    
      #for(SuffixTag in ConfIntSuffixes){
        # First set up the columns in the results data frame that will hold parameters estimates and predictions.
        
        #Setup labels
        #model_CumTime <- paste0(modelID, "_CumTime", "_", SuffixTag) 
        #model_MVF  <- paste0(modelID, "_MVF", "_", SuffixTag)
        #model_IF <- paste0(modelID, "_IF", "_", SuffixTag)
        #model_FI <- paste0(modelID, "_FI", "_", SuffixTag)
        #model_Rel <- paste0(modelID, "_Rel", "_", SuffixTag)
        #model_MVF_inv <- paste0(modelID, "_", "MVF_inv",  "_", SuffixTag)
        #model_R_growth <- paste0(modelID, "_R_growth", "_", SuffixTag)
        #model_MTTF <- paste0(modelID, "_", "MTTF", "_", SuffixTag)
      #} # End for - for MLE, low confidence, and high confidence bounds.
      
      for (SuffixTag in ConfIntSuffixes) {
        for (paramNum in 1:length(get(model_params_label))) {
          #model_parm_num <- paste0(modelID, "_parm_", paramNum)
          model_parm_num <- paste0(modelID, "_", get(model_params_label)[paramNum], "_", SuffixTag)
          local_results[[model_parm_num]] <- naFill
        }
        ParmEstimatesConverged[[SuffixTag]] <- TRUE
      }
    
      for (ResultType in ModelResultType) {
        for (SuffixTag in ConfIntSuffixes) {
          local_results[[paste(modelID, ResultType, SuffixTag, sep="_")]] <- naFill
        }
      }
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
          model_params <- NULL
          if(!anyNA(temp_params)){
            #lnL_value <- temp_lnL
            model_params[["MLE"]] <- temp_params
            sel_method <- method
            break
          }
        }

        if(is.na(sel_method)){
          print("None of the algorithms work")
          ParmEstimatesConverged[["MLE"]] <- FALSE
        }
        
        print(paste0("Selected method for ",modelID))
        print(sel_method)

        
        # Now we estimate confidence bounds for the model parameters.

        ConfBounds <- NULL
        model_params[["Low"]] <- rep(0/0,length(model_params$MLE))
        model_params[["High"]] <- model_params[["Low"]]
        
        if(typeof(model_params$MLE)!="character") {
            if(dataType == "FC" && dataType %in% get(model_input)){
              ConfBounds <- NULL
              model_params[["Low"]] <- rep(0/0,length(model_params[["MLE"]]))
              model_params[["High"]] <- lowerConfBound
            } else if (dataType == "FT" && dataType %in% get(model_input)){
              ConfBounds <- estim_conf_int(model_lnL, model_params[["MLE"]], param_names, ConfInterval, tVec)
              model_params[["Low"]] <- ConfBounds$LowerBoundsValues
              model_params[["High"]] <- ConfBounds$UpperBoundsValues
              print(unlist(c(model_sm_MLE,length(tVec),model_params[["Low"]],model_params[["MLE"]],model_params[["High"]]),use.names=FALSE))
            } else if ("IF" %in% get(model_input)) {
              model_lnL <- paste(modelID, "FT", "lnL", sep="_")
              ConfBounds <- estim_conf_int(model_lnL, model_params[["MLE"]], param_names, ConfInterval, IF)
              model_params[["Low"]] <- ConfBounds$LowerBoundsValues
              model_params[["High"]] <- ConfBounds$UpperBoundsValues
              print(unlist(c(model_sm_MLE,length(IF),model_params[["Low"]],model_params[["MLE"]],model_params[["High"]]),use.names=FALSE))
            }
        }
        
        #This for loop selects one of the methods indicated in the Model_Specifications.R file.
        
        
        # Now put the parameter estimates into the results frame
        
        for(SuffixTag in ConfIntSuffixes) {
          for (paramNum in 1:length(get(model_params_label))) {
              #model_parm_num <- paste0(modelID, "_parm_", paramNum) 
              model_parm_num <- paste0(modelID, "_", get(model_params_label)[paramNum], "_", SuffixTag)
              if(typeof(model_params[[SuffixTag]])!="character") {
                print(model_params[[SuffixTag]])
                print("Type of param number : ")
                print(modelID)
                print(model_params_label)
                print(paramNum)
                print(typeof(model_params[[SuffixTag]][[paramNum]]))
                local_results[[model_parm_num]][failure_num] <- (model_params[[SuffixTag]])[paramNum]
              } 
              else {
                # The model results didn't converge.  Use NaN to indicate nonconvergence.
                local_results[[model_parm_num]][failure_num] <- NaN
              }
          } # End for - we've estimated the parameters for the current model for the current failure.
        } # End for - parameters for MLE, low confidence, and high confidence bounds.
      } # End for - we've estimated model parameters for the current model over the entire dataset.
    

      for(SuffixTag in ConfIntSuffixes) {
        if(any(is.nan(as.vector(unlist(model_params[[SuffixTag]]))))) {
          # Indicate that this is a model that won't be displayed on the plot.
          ParmEstimatesConverged[[SuffixTag]] <- FALSE
        } else {
          ParmEstimatesConverged[[SuffixTag]] <- TRUE
        }
      }
        
      #in_data <- raw_data$FRate
      
      model_FI <- paste0(modelID, "_FI")
      model_MTTF <- paste0(modelID, "_MTTF")
      model_MVF  <- paste0(modelID, "_MVF")
      model_MVF_inv <- paste0(modelID,"_", "MVF_inv")
      model_R_growth <- paste0(modelID, "_R_growth")
      model_CumTime <- paste0(modelID, "_CumTime") 
      model_IF <- paste0(modelID, "_IF")
      model_Rel <- paste0(modelID, "_Rel")
      
      for (SuffixTag in ConfIntSuffixes) {
        if(ParmEstimatesConverged[[SuffixTag]]) {
          # This is done here because it may be the case that we have confidence values for the
          # parameters that converge, but produce results that make no sense, such as MVF values
          # that are less than 0.  Here we look for this type of nonsensical value and process the
          # model as an unplottable model in this case.
          
          # First compute the MVF, IF, FI, Reliability, and Reliability Growth functions for the model.
          pred_input_data <- data.frame("IF" = in_data[["IF"]], "FT" = in_data[["FT"]])
          
          # Next estimate MVF, then forecast.
          local_estim <- get(model_MVF)(model_params[[SuffixTag]], pred_input_data)[["Failure"]]
          
          if (!any(tail(local_estim, length(local_estim)-1) - head(local_estim, length(local_estim)-1) < 0)) {
            # If the parameter estimates using the last point in the data set converged and passed
            # the nonsense predictions test above, the model is plottable.  Otherwise, we flag it
            # as not being plottable.
            
            if (SuffixTag == "Low") {
              PlottableModelsLow <- c(PlottableModelsLow, modelID)
            } else if (SuffixTag == "MLE") {
              PlottableModelsMLE <- c(PlottableModelsMLE, modelID)
            } else if (SuffixTag == "High") {
              PlottableModelsHigh <- c(PlottableModelsHigh, modelID)
            }
            
            # Here we compute the model estimates of MVF, IF, FI, and Reliability.
            # First we create empty fill vectors into which we may need to add
            # values for finite-failures models.  See below.
            
            ModelPredsNA <- c()
            ModelPredsNaN <- c()
            ModelPredsInf <- c()
            ModelPredsZero <- c()
            ModelPredsOnes <- c()
            FillData <- rep(NA, PredAheadSteps)
            
            # The next thing we do is determine whether this is a finite-failures
            # model.  If it is, we may have to add some fill onto the end of the
            # predictions vector we get, because we may have asked the model to
            # make predictions for more future failures than the model thinks
            # there actually are.
            
            if (get(paste(modelID,"Finite",sep="_"))) {
              ExpectedTotalFailures <- model_params[[SuffixTag]][get(paste(modelID,"numfailsparm",sep="_"))[1]]
              
              if(abs(local_estim[length(local_estim)]-round(local_estim[length(local_estim)])) < tol_local) {
                lower_pred_bound <- local_estim[length(local_estim)]+1
              } else {
                lower_pred_bound <- round(local_estim[length(local_estim)]+1)
              }
              
              if(PredAheadSteps < ExpectedTotalFailures-local_estim[length(local_estim)]) {
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
              # Infinite failures model.
              
              ExpectedTotalFailures <- 0
              invMVFinput <- c((floor(local_estim[length(local_estim)])+1):(floor(local_estim[length(local_estim)])+PredAheadSteps))
            } # Endif - is this a finite failures model?
            
            if(length(invMVFinput) > 0) {
              pred_input_data <- data.frame("FN" = invMVFinput)
              local_results[[paste0(model_CumTime, "_", SuffixTag)]] <- c(in_data[["FT"]]+OffsetTime, get(model_MVF_inv)(model_params[[SuffixTag]], pred_input_data)[["Time"]]+OffsetTime, ModelPredsInf)
              local_results[[paste0(model_MVF, "_", SuffixTag)]] <- c(local_estim+OffsetFailure, invMVFinput+OffsetFailure, rep(as.numeric(ExpectedTotalFailures+OffsetFailure), length(ModelPredsNA)))
            } else {
              local_results[[paste0(model_CumTime, "_", SuffixTag)]] <- c(in_data[["FT"]]+OffsetTime, ModelPredsInf)
              local_results[[paste0(model_MVF, "_", SuffixTag)]] <- c(local_estim+OffsetFailure, rep(as.numeric(ExpectedTotalFailures+OffsetFailure), PredAheadSteps))
            }
            
            pred_input_data <- data.frame("FT" = subset(local_results, !is.infinite(get(paste0(modelID, "_CumTime","_", SuffixTag))), select=get(paste0(modelID, "_CumTime", "_", SuffixTag)))-OffsetTime)
            names(pred_input_data) <- c("FT")
            if(any(sapply(model_params[[SuffixTag]],is.finite)) == TRUE){
              local_results[[paste0(model_FI, "_", SuffixTag)]] <- try(c(get(model_FI)(model_params[[SuffixTag]], pred_input_data)[["Failure_Rate"]], ModelPredsZero),silent=TRUE)
              local_results[[paste0(model_IF, "_", SuffixTag)]] <- try(c(get(model_MTTF)(model_params[[SuffixTag]], pred_input_data)[["MTTF"]], ModelPredsInf),silent=TRUE)
              local_results[[paste0(model_R_growth, "_", SuffixTag)]] <- try(c(get(model_R_growth)(model_params[[SuffixTag]], pred_input_data, RelMissionTime)[["Reliability_Growth"]], ModelPredsOnes), silent=TRUE)
            }
            
            pred_input_data <- NULL
          } else {
            if (SuffixTag == "Low") {
              UnplottableModelsLow <- c(UnplottableModelsLow, modelID)
            } else if (SuffixTag == "MLE") {
              UnplottableModelsMLE <- c(UnplottableModelsMLE, modelID)
            } else if (SuffixTag == "High") {
              UnplottableModelsHigh <- c(UnplottableModelsHigh, modelID)
            }
          }
        } else {
          if (SuffixTag == "Low") {
            UnplottableModelsLow <- c(UnplottableModelsLow, modelID)
          } else if (SuffixTag == "MLE") {
            UnplottableModelsMLE <- c(UnplottableModelsMLE, modelID)
          } else if (SuffixTag == "High") {
            UnplottableModelsHigh <- c(UnplottableModelsHigh, modelID)
          }
        } # Endif - did the model's parameter estimates converge (MLE or confidence bound estimates)?
      } # End for - models results have been computed for MLE parameter estimates & high and low confidence bounds.
  } # End for - we've applied all of the selected models to the entire dataset.
  PlottableModels <- list("Low"=PlottableModelsLow, "MLE"=PlottableModelsMLE, "High"=PlottableModelsHigh)
  UnplottableModels <- list("Low"=UnplottableModelsLow, "MLE"=UnplottableModelsMLE, "High"=UnplottableModelsHigh)
  return(list("Results"=local_results, "SuccessfulModels"=PlottableModels, "FailedModels"=UnplottableModels))
}

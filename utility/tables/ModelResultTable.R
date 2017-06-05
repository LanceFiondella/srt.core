model_result_table <- function (in_data, NumObservations, input, ResultsToShow, RelMissionTime) {
  
  SuffixConfInt <- c("Low", "MLE", "High")
  ModelResultType <- c("CumTime", "MVF", "IF", "FI", "R_growth")
  
  TableModelEstim <- NULL
  TableModelEstim[["Low"]] <- input$LowConfOnTable
  TableModelEstim[["MLE"]] <- input$MLEOnTable
  TableModelEstim[["High"]] <- input$HighConfOnTable

  # First column holds failure numbers.
  local_MR_Table <- data.frame("Failure"=in_data$Failure)
  
  MR_Table_Error <- FALSE
  
  if(length(ResultsToShow) > 0) {
    for(modelID in ResultsToShow) {
      for (paramNum in 1:length(get(paste(modelID,"params",sep="_")))) {
        for (SuffixTag in SuffixConfInt) {
          if (TableModelEstim[[SuffixTag]]) {
            #local_MR_Table[[paste0(modelID, "_parm_", paramNum)]] <- as.character(in_data[[paste0(modelID, "_parm_", paramNum)]])
            param_name <- paste(modelID, get(paste0(modelID, "_params"))[paramNum], sep="_")
            local_MR_Table[[paste(param_name, SuffixTag, sep="_")]] <- as.character(in_data[[paste(param_name, SuffixTag, sep="_")]])
          }
        } # End for - put MLE parameter estimates into table as well as high and low CI values.
      } # End for - put estimates for all of current model's parameters into table.
      
      for (ResultType in ModelResultType) {
        for (SuffixTag in SuffixConfInt) {
          if(TableModelEstim[[SuffixTag]]) {
            if (ResultType == "R_growth") {
              # Reliability growth is an interactive tabular display - if users change
              # the mission time, the table will be updated.
              
              rg_input_data <- data.frame("FT" = subset(in_data, !is.infinite(get(paste0(modelID, "_CumTime", "_", SuffixTag))), select=get(paste0(modelID, "_CumTime", "_", SuffixTag))))
              names(rg_input_data) <- c("FT")
              model_params <- c()
              for (parmIndex in 1:length(get(paste0(modelID, "_params")))) {
                #model_params <- c(model_params, in_data[[paste0(modelID, "_parm_", parmIndex)]][NumObservations])
                param_name <- paste(modelID, get(paste0(modelID, "_params"))[parmIndex], SuffixTag, sep="_")
                model_params <- c(model_params, in_data[[param_name]][NumObservations])
              }
              names(model_params) <- paste(modelID, get(paste0(modelID, "_params")), sep="_")
              temp_R_growth <- data.frame("Reliability_Growth"=c(get(paste(modelID, ResultType, sep="_"))(model_params, rg_input_data, RelMissionTime)[["Reliability_Growth"]], rep(1, length(in_data[[paste(modelID, "CumTime", SuffixTag, sep="_")]])-length(rg_input_data[[1]]))))
              local_MR_Table[[paste(modelID, ResultType, SuffixTag, sep="_")]] <- as.character(temp_R_growth[["Reliability_Growth"]])
            } else {
              local_MR_Table[[paste(modelID, ResultType, SuffixTag, sep="_")]] <- as.character(in_data[[paste(modelID, ResultType, SuffixTag, sep="_")]])
            }
          }
        } # End for - put MLE model results into table as well as high and low CI values.
      } # End for - put estimates for all of current model's results into table.
    } # End for - build table entries for all models that were run.
  } else {
    
    # Somehow we don't have any model results to display.
    # #print an error message.
    
    MR_Table_Error <- TRUE
    
  }

  if(MR_Table_Error) {
    local_MR_Table <- data.frame()
  }
  local_MR_Table = round_table(local_MR_Table, 6)
  return(local_MR_Table)
}

#This function rounds the entire table to 'round_decimal' number of places regardless of whether there are NA and text present
round_table <- function(MR_Table, round_decimal) {
MR_Table[] <- lapply(MR_Table, function(x) {
           x1 <- type.convert(as.character(x), as.is=TRUE)
    ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), round_decimal), x1)})
return(MR_Table)
    
}

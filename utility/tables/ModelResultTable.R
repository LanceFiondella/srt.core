model_result_table <- function (in_data, NumObservations, ResultsToShow, RelMissionTime) {
  
  # First column holds failure numbers.
  local_MR_Table <- data.frame("Failure"=in_data$Failure)
  
  MR_Table_Error <- FALSE
  
  if(length(ResultsToShow) > 0) {
    for(modelID in ResultsToShow) {
      for (paramNum in 1:length(get(paste(modelID,"params",sep="_")))) {
        #local_MR_Table[[paste0(modelID, "_parm_", paramNum)]] <- as.character(in_data[[paste0(modelID, "_parm_", paramNum)]])
        param_name <- paste(modelID, get(paste0(modelID, "_params"))[paramNum], sep="_")
        local_MR_Table[[param_name]] <- as.character(in_data[[param_name]])
      }
      local_MR_Table[[paste0(modelID, "_CumTime")]] <- as.character(in_data[[paste0(modelID, "_CumTime")]])
      local_MR_Table[[paste0(modelID, "_MVF")]] <- as.character(in_data[[paste0(modelID, "_MVF")]])
      local_MR_Table[[paste0(modelID, "_IF")]] <- as.character(in_data[[paste0(modelID, "_IF")]])
      local_MR_Table[[paste0(modelID, "_FI")]] <- as.character(in_data[[paste0(modelID, "_FI")]])
      # local_MR_Table[[paste0(modelID, "_Rel")]] <- as.character(in_data[[paste0(modelID, "_Rel")]])
      
      # Reliability growth is an interactive tabular display - if users change
      # the mission time, the table will be updated.
      
      rg_input_data <- data.frame("FT" = subset(in_data, !is.infinite(get(paste0(modelID, "_CumTime"))), select=get(paste0(modelID, "_CumTime"))))
      names(rg_input_data) <- c("FT")
      model_params <- c()
      for (parmIndex in 1:length(get(paste0(modelID, "_params")))) {
        #model_params <- c(model_params, in_data[[paste0(modelID, "_parm_", parmIndex)]][NumObservations])
        param_name <- paste(modelID, get(paste0(modelID, "_params"))[parmIndex], sep="_")
        model_params <- c(model_params, in_data[[param_name]][NumObservations])
      }
      names(model_params) <- paste(modelID, get(paste0(modelID, "_params")), sep="_")
      temp_R_growth <- data.frame("Reliability_Growth"=c(get(paste(modelID,"R_growth",sep="_"))(model_params, rg_input_data, RelMissionTime)[["Reliability_Growth"]], rep(1, length(in_data[[paste(modelID, "CumTime", sep="_")]])-length(rg_input_data[[1]]))))
      local_MR_Table[[paste0(modelID, "_R_growth")]] <- as.character(temp_R_growth[["Reliability_Growth"]])
    }
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

model_result_table <- function (in_data, ResultsToShow) {
  
  # First column holds failure numbers.
  
  local_MR_Table <- data.frame("Failure"=in_data$Failure)
  
  MR_Table_Error <- FALSE
  
  if(length(ResultsToShow) > 0) {
    for(modelID in ResultsToShow) {
      for (paramNum in 1:length(get(paste(modelID,"params",sep="_")))) {
        local_MR_Table[[paste0(modelID, "_parm_", paramNum)]] <- in_data[[paste0(modelID, "_parm_", paramNum)]]
      }
      local_MR_Table[[paste0(modelID, "_MVF")]] <- in_data[[paste0(modelID, "_MVF")]]
      local_MR_Table[[paste0(modelID, "_IF")]] <- in_data[[paste0(modelID, "_IF")]]
      local_MR_Table[[paste0(modelID, "_FI")]] <- in_data[[paste0(modelID, "_FI")]]
      local_MR_Table[[paste0(modelID, "_Rel")]] <- in_data[[paste0(modelID, "_Rel")]]
    }
  } else {
    
    # Somehow we don't have any model results to display.
    # Print an error message.
    
    MR_Table_Error <- TRUE
    
  }

  if(MR_Table_Error) {
    local_MR_Table <- data.frame()
  }  
  return(local_MR_Table)
}
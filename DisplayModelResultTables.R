tempResultMatrix <- NULL
if (length(names(ModelResultsList)) > 5) {
  # There's at least one set of model results in ModelResultsList.
  
  numResultCols <- 0
  DisplayTableNames <- c()
  
  ModelsToDisplay <- unlist(ModelsExecutedList, use.names=FALSE)
  for (resultIndex in 1:length(ModelsToDisplay)) {
    # First add the names of the current results table to the overall
    # list of column names in the table that will be displayed.
    
    CurrentTableNames <- names(ModelResultsList[[ModelsToDisplay[resultIndex]]])
    numResultCols <- numResultCols + length(CurrentTableNames)
    DisplayTableNames <- c(DisplayTableNames, CurrentTableNames)
    
    # Now add the columns of the current results table to the display table.
    
    for (columnIndex in 1:length(CurrentTableNames)) {
      tempResultMatrix <- c(tempResultMatrix, unlist(ModelResultsList[[ModelsToDisplay[resultIndex]]][columnIndex], use.names=FALSE))
    }
  }
  
  tempResultMatrix <- matrix(tempResultMatrix, ncol=numResultCols)
  DisplayTableNames <- gsub("MVF", "CumTimeAtFailure", DisplayTableNames)
  DisplayTableNames <- gsub("IF", "InterfailTime", DisplayTableNames)
  DisplayTableNames <- gsub("FI", "FailureIntensity", DisplayTableNames)
  DisplayTableNames <- gsub("REL", "Reliability", DisplayTableNames)
  colnames(tempResultMatrix) <- DisplayTableNames
} else {
  # There are no model results to display
  
  tempResultMatrix <- matrix()
}

tempResultMatrix
if (length(names(ModelResultsList)) > 5) {
  # There's at least one set of model results in ModelResultsList.
  
  numResultCols <- 1  # Allow for a column containing failure numbers.
  DisplayTableNames <- c("Failure Number")
  
  ModelsToDisplay <- unlist(ModelsExecutedList, use.names=FALSE)
  for (resultIndex in 1:length(ModelsToDisplay)) {
    # First add the names of the current results table to the overall
    # list of column names in the table that will be displayed.
    
    CurrentTableNames <- names(ModelResultsList[[ModelsToDisplay[resultIndex]]])
    numResultCols <- numResultCols + length(CurrentTableNames)
    
    # First add a column of failure numbers

    OutputTable <- c(ModelResultsList[["DataStartAndEnd"]]$Start:(ModelResultsList[["DataStartAndEnd"]]$End+ModelResultsList[["DataStartAndEnd"]]$NumPreds))

    # Now add the columns of the current results table to the display table.
    
    for (columnIndex in 1:length(CurrentTableNames)) {
      OutputTable <- c(OutputTable, unlist(ModelResultsList[[ModelsToDisplay[resultIndex]]][columnIndex], use.names=FALSE))
    }
    CurrentTableNames <- gsub("MVF", paste0(ModelsToDisplay[resultIndex], "_CumTimeAtFailure"), CurrentTableNames)
    CurrentTableNames <- gsub("IF", paste0(ModelsToDisplay[resultIndex], "_InterfailTime"), CurrentTableNames)
    CurrentTableNames <- gsub("FI", paste0(ModelsToDisplay[resultIndex], "_FailureIntensity"), CurrentTableNames)
    CurrentTableNames <- gsub("REL", paste0(ModelsToDisplay[resultIndex], "_Reliability"), CurrentTableNames)
    DisplayTableNames <- c(DisplayTableNames, CurrentTableNames)
  }
  
  OutputTable <- matrix(OutputTable, ncol=numResultCols)
  colnames(OutputTable) <- DisplayTableNames
} else {
  # There are no model results to display
  
  OutputTable <- matrix()
}
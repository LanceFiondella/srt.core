if (length(names(ModelResultsList)) > 5) {
  # There's at least one set of model results in ModelResultsList.
  
  numResultCols <- 1  # Allow for a column containing failure numbers.
  DisplayTableNames <- c("Failure Number")
  
  # First add a column of failure numbers
  
  OutputTable <- c(ModelResultsList[["DataStartAndEnd"]]$Start:(ModelResultsList[["DataStartAndEnd"]]$End+ModelResultsList[["DataStartAndEnd"]]$NumPreds))
  
  # Now get the list of model results that the user wants to
  # display in the table.  This includes models that ran
  # successfully, as well as those that did not.
  
  ModelsForTableDisplay <- as.list(input$AllModelsRun)
  
  ModelsToDisplay <- unlist(ModelsExecutedList, use.names=FALSE)
  for (resultIndex in 1:length(ModelsToDisplay)) {
    for(ModelsForTableDisplayIndex in 1:length(ModelsForTableDisplay)) {
      if(ModelsToDisplay[resultIndex] == ModelsForTableDisplay[ModelsForTableDisplayIndex]) {
        # First add the names of the current results table to the overall
        # list of column names in the table that will be displayed.
        
        CurrentTableNames <- names(ModelResultsList[[ModelsToDisplay[resultIndex]]])
        numResultCols <- numResultCols + length(CurrentTableNames)
        
        # Now add the columns of the current results table to the display table.
        
        for (columnIndex in 1:length(CurrentTableNames)) {
          if(CurrentTableNames[columnIndex] == "MVF") {
            OutputTable <- c(OutputTable, unlist(ModelResultsList[[ModelsToDisplay[resultIndex]]][columnIndex], use.names=FALSE)+ModelResultsList[["TimeOffset"]])
          } else {
            OutputTable <- c(OutputTable, unlist(ModelResultsList[[ModelsToDisplay[resultIndex]]][columnIndex], use.names=FALSE))
          }
        }
        CurrentTableNames <- gsub("MVF", paste0(ModelsToDisplay[resultIndex], "_CumTimeAtFailure"), CurrentTableNames)
        CurrentTableNames <- gsub("IF", paste0(ModelsToDisplay[resultIndex], "_InterfailTime"), CurrentTableNames)
        CurrentTableNames <- gsub("FI", paste0(ModelsToDisplay[resultIndex], "_FailureIntensity"), CurrentTableNames)
        CurrentTableNames <- gsub("REL", paste0(ModelsToDisplay[resultIndex], "_Reliability"), CurrentTableNames)
        DisplayTableNames <- c(DisplayTableNames, CurrentTableNames)
      }
    }
  }
  
  OutputTable <- matrix(OutputTable, ncol=numResultCols)
  colnames(OutputTable) <- DisplayTableNames
} else {
  # There are no model results to display
  
  OutputTable <- matrix()
}
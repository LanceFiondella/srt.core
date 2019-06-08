data_or_trend_table <- function(inputData, DataRange, DataOrTrend, TrendTest) {
  
  in_data <- inputData
  DataColNames <- names(in_data)
  names(in_data) <- gsub("x.", "", DataColNames)
  
  D_or_T_table <- data.frame()
  TableCreateError <- FALSE
  
  DataStart <- DataRange[1]
  DataEnd <- DataRange[2]
  
  if (dataType(names(in_data)) == "FR") {
    
    IF <- tail(head(in_data$IF, DataEnd), (DataEnd-DataStart+1))
    FT <- tail(head(in_data$FT, DataEnd), (DataEnd-DataStart+1))
    FN <- tail(head(in_data$FN, DataEnd), (DataEnd-DataStart+1))
    
    if (DataOrTrend == 1) {
      
      # Create a table of the failure data
      
      D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Failure Time"=FT)
      
    } else if (DataOrTrend == 2) {
      
      # Create a table of the trend test
      
      if (TrendTest == "LP") {
        
        # Laplace Test

        trendStat <- laplace_trend_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Laplace Test Statistic"=trendStat$Laplace_factor)
        
      } else if (TrendTest == "RA") {
        
        # Running Arithmetic Average
        
        trendStat <- running_average_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Running Average IF Time"=trendStat$Running_Average)
        
      } else {
        
        # Couldn't determine trend test type.
        # #print an error message.
        
        TableCreateError <- TRUE
      }
    } else {
      
      # Couldn't determine whether to create a data or
      # trend test table.
      # #print an error message.
      
      TableCreateError <- TRUE
    }
  } else if (dataType(names(in_data)) == "FC") {
    
    FC <- tail(head(in_data$FC, DataEnd), (DataEnd-DataStart+1))
    CFC <- tail(head(in_data$CFC, DataEnd), (DataEnd-DataStart+1))
    CumT <- tail(head(in_data$T, DataEnd), (DataEnd-DataStart+1))
    TI <- tail(head(in_data$TI, DataEnd), (DataEnd-DataStart+1))
    
    FC_to_IF_inData <- FCFrame_to_IFFrame(in_data$T, in_data$FC)
      
    FN <- c(unlist(subset(subset(FC_to_IF_inData, FC_to_IF_inData$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_FN)), use.names=FALSE)
    FT <- c(unlist(subset(subset(FC_to_IF_inData, FC_to_IF_inData$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_FT)), use.names=FALSE)
    IF <- c(unlist(subset(subset(FC_to_IF_inData, FC_to_IF_inData$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_IF)), use.names=FALSE)
    IntervalNum <- c(unlist(subset(subset(FC_to_IF_inData, FC_to_IF_inData$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_TI)), use.names=FALSE)
    
    if (DataOrTrend == 1) {
      
      # Create a table of the failure data
      
      D_or_T_table <- data.frame("Test Interval"=TI, "Cumulative Test Time"=CumT, "Failure Counts"=FC, "Cumulative Failure Count"=CFC)
      
    } else if (DataOrTrend == 2) {
      
      # Create a table of the trend test
      
      if (TrendTest == "LP") {
        
        # Laplace Test
        
        trendStat <- laplace_trend_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Laplace Test Statistic"=trendStat$Laplace_factor)
        
      } else if (TrendTest == "RA") {
        
        # Running Arithmetic Average
        
        trendStat <- running_average_test(IF)
        D_or_T_table <- data.frame("Failure Number"=FN, "Times Between Failures"=IF, "Running Average IF Time"=trendStat$Running_Average)
        
      } else {
        
        # Couldn't determine trend test type.
        # #print an error message.
        
        TableCreateError <- TRUE
      }
    } else {
      
      # Couldn't determine whether to create a data or
      # trend test table.
      # #print an error message.
      
      TableCreateError <- TRUE
    }
  } else {
    
    # Couldn't identify input data type.
    # #print an error message.
    
    TableCreateError <- TRUE
  }

    
  if (TableCreateError) {
    
    # If we've encountered any errors in creating the table,
    # return an empty data frame.
    
    D_or_T_table <- data.frame()
  }
  D_or_T_table = round_table(D_or_T_table, 6)
  return(D_or_T_table)
}

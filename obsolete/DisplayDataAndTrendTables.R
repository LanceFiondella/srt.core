DataIntervalStart_FDT <- input$modelDataRange[1]
DataIntervalEnd_FDT <- input$modelDataRange[2]
tempDataMatrix <- matrix()
if (!(is.null(input$file) && (input$type == 2)) || (!(is.null(input$dataSheetChoice)) && (input$type == 1))) {
  data <- data.frame(x=data_global())
  DataColNames <- names(data)
  names(data) <- gsub("x.", "", DataColNames)
  NameArray <- names(data)
  
  if(input$DataPlotAndTableTabset == "Data and Trend Test Table") {
    if(length(grep("IF",names(data))) || length(grep("FT",names(data)))) {
      FN <- data$FN
      
      IF <- c(unlist(subset(subset(data, data$FN >= DataIntervalStart_FDT, select = c(FN, IF, FT)), FN <= DataIntervalEnd_FDT, select = IF)), use.names=FALSE)
      FT <- c(unlist(subset(subset(data, data$FN >= DataIntervalStart_FDT, select = c(FN, IF, FT)), FN <= DataIntervalEnd_FDT, select = FT)), use.names=FALSE)
      FN <- c(unlist(subset(subset(data, data$FN >= DataIntervalStart_FDT, select = c(FN, IF, FT)), FN <= DataIntervalEnd_FDT, select = FN)), use.names=FALSE)
      
      if(input$PlotDataOrTrend == 1) {
        
        NameArray <- c("Failure Number", "Times Between Failures", "Failure Time")
        tempDataMatrix <- matrix(c(FN, IF, FT), ncol=3)
      } else if(input$PlotDataOrTrend == 2) {
        if (input$trendPlotChoice == "LP") {
          sol <- laplace_trend_test(IF)
          NameArray <- c("Failure Number", "Times Between Failures", "Laplace Test Statistic")
          tempDataMatrix <- matrix(c(FN, IF, sol$Laplace_factor), ncol=3)
        } else if (input$trendPlotChoice == "RA") {
          sol <- running_average_test(IF)
          NameArray <- c("Failure Number", "Times Between Failures", "Running Average IF Time")
          tempDataMatrix <- matrix(c(FN, IF, sol$Running_Average), ncol=3)
        }
      }
    } else if(length(grep("CFC",names(data))) || length(grep("FC",names(data)))) {
      FC <- c(unlist(subset(subset(data, data$TI >= DataIntervalStart_FDT, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd_FDT, select = FC)), use.names=FALSE)
      CFC <- c(unlist(subset(subset(data, data$TI >= DataIntervalStart_FDT, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd_FDT, select = CFC)), use.names=FALSE)
      CumT <- c(unlist(subset(subset(data, data$TI >= DataIntervalStart_FDT, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd_FDT, select = T)), use.names=FALSE)
      TI <- c(unlist(subset(subset(data, data$TI >= DataIntervalStart_FDT, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd_FDT, select = TI)), use.names=FALSE)
      
      FN <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_FN)), use.names=FALSE)
      FT <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_FT)), use.names=FALSE)
      IF <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_IF)), use.names=FALSE)
      IntervalNum <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart_FDT, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd_FDT, select = FC_TI)), use.names=FALSE)
      
      if(input$PlotDataOrTrend == 1) {
        NameArray <- c("Test Interval", "Cumulative Test Time", "Failure Counts", "Cumulative Failure Count")
        tempDataMatrix <- matrix(c(TI, CumT, FC, CFC), ncol=4)
      } else if(input$PlotDataOrTrend == 2) {
        if(input$trendPlotChoice == "LP") {
          sol <- laplace_trend_test(IF)
          NameArray <- c("Failure Number", "Times Between Failures", "Laplace Test Statistic")
          tempDataMatrix <- matrix(c(FN, IF, sol$Laplace_factor), ncol=3)
        } else if(input$trendPlotChoice == "RA") {
          sol <- running_average_test(IF)
          NameArray <- c("Failure Number", "Times Between Failures", "Running Average IF Time")
          tempDataMatrix <- matrix(c(FN, IF, sol$Running_Average), ncol=3)
        }
      }
    }
    colnames(tempDataMatrix) <- NameArray
  }
}
tempDataMatrix

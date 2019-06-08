plot_trend_tests <- function(in_data, convertedFCData, DataName, DataRange, TrendTest, Confidence, LaplaceStat, PlotType, MinIntervalWidth){
  
  require(ggplot2)
  
  PlotFault <- FALSE
  
  DataIntervalStart <- DataRange[1]
  DataIntervalEnd <- DataRange[2]
  
  # Initialize the plot
  
  localTrendPlot <- ggplot()
  
  if((DataIntervalEnd - DataIntervalStart + 1) >= MinIntervalWidth) {
    localTrendPlot <- ggplot(,aes_string(x="index",y="trend_test_statistic"))
    if((length(grep("FT",names(in_data)))>0) || (length(grep("IF",names(in_data))) > 0)) {
      IF <- c(unlist(subset(subset(in_data, in_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
      FT <- c(unlist(subset(subset(in_data, in_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
    } else if((length(grep("CFC",names(in_data)))>0) || (length(grep("FC",names(in_data)))>0)) {
      FC <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)), use.names=FALSE)
      CFC <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)), use.names=FALSE)
      CumT <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)), use.names=FALSE)
      TI <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)), use.names=FALSE)
      
      FT <- c(unlist(subset(subset(convertedFCData, convertedFCData$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FT)), use.names=FALSE)
      IF <- c(unlist(subset(subset(convertedFCData, convertedFCData$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_IF)), use.names=FALSE)
    }

    if(TrendTest == "LP") {
      plot_data <- laplace_trend_test(IF)
      if((length(grep("FT",names(in_data)))>0) || (length(grep("IF",names(in_data))) > 0)) {
        plot_data$Index <- plot_data$Index + (DataIntervalStart - 1)
      } else {
        if(DataIntervalStart > 1) {
          plot_data$Index <- plot_data$Index + in_data$CFC[DataIntervalStart - 1]
        }
      }
      localTrendPlot <- localTrendPlot + xlab("Failure Number")+ylab("Laplace Test Statistic")
      localTrendPlot <- localTrendPlot+ggtitle(paste(paste(c("Laplace trend test of"),DataName), paste("\nReliability growth with confidence greater than", as.character(Confidence))))
      localTrendPlot <- localTrendPlot+ggtitle(paste(c("Laplace trend test of"),DataName))
      localTrendPlot <- localTrendPlot + geom_hline(yintercept = LaplaceStat, color="red", alpha=0.8)
      localTrendPlot <- localTrendPlot + geom_hline(aes(yintercept=c(qnorm(0.1),qnorm(0.05),qnorm(0.01),qnorm(0.001),qnorm(0.0000001),qnorm(0.0000000001))),linetype="dotted",color='black',alpha=0.8)
      localTrendPlot <- localTrendPlot+xlab("Failure Number")+ylab("Laplace Test Statistic")
    } else if(TrendTest == "RA") {
      plot_data <- running_average_test(IF)
      if((length(grep("FT",names(in_data)))>0) || (length(grep("IF",names(in_data))) > 0)) {
        plot_data$Index <- plot_data$Index + (DataIntervalStart - 1)
      } else {
        if(DataIntervalStart > 1) {
          plot_data$Index <- plot_data$Index + in_data$CFC[DataIntervalStart - 1]
        }
      }
      localTrendPlot <- localTrendPlot + xlab("Failure Number")+ylab("Running Average of Interfailure Times")
      localTrendPlot <- localTrendPlot+ggtitle(paste(c("Running Average trend test of"),DataName))
    } else {
      # Couldn't identify view of data to display.
      # #print an error message.
      
      PlotFault <- TRUE
    }
    
    names(plot_data) = c("index","trend_test_statistic")
    
    if(PlotType == "points_and_lines"){
      localTrendPlot <- localTrendPlot + geom_point(data=plot_data,aes(index,trend_test_statistic))+ geom_step(data=plot_data)
    }
    if(PlotType=="points"){
      localTrendPlot <- localTrendPlot + geom_point(data=plot_data,aes(index,trend_test_statistic))
    }
    if(PlotType=="lines"){
      localTrendPlot <- localTrendPlot + geom_step(data=plot_data,aes(index,trend_test_statistic))
    }
    localTrendPlot <- localTrendPlot + theme(legend.position = "bottom", text = element_text(size=14))
  } else {
    PlotFault <- TRUE
  }

  if(PlotFault) {
    localTrendPlot = NULL
  }
  return(localTrendPlot)
}

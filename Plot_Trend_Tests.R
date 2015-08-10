DataIntervalStart <- input$modelDataRange[1]
DataIntervalEnd <- input$modelDataRange[2]

if((DataIntervalEnd - DataIntervalStart + 1) >= K_minDataModelIntervalWidth) {
  DataAndTrendPlot <- ggplot(,aes_string(x="index",y="trend_test_statistic"))
  if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data))) > 0)) {
    IF <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
    FT <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
  } else if((length(grep("CFC",names(input_data)))>0) || (length(grep("FC",names(input_data)))>0)) {
    FC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)), use.names=FALSE)
    CFC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)), use.names=FALSE)
    CumT <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)), use.names=FALSE)
    TI <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)), use.names=FALSE)
    
    FT <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FT)), use.names=FALSE)
    IF <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_IF)), use.names=FALSE)
  }
  
  if(input$trendPlotChoice=="LP") {
    plot_data <- laplace_trend_test(IF)
    if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data))) > 0)) {
      plot_data$Index <- plot_data$Index + (DataIntervalStart - 1)
    } else {
      if(DataIntervalStart > 1) {
        plot_data$Index <- plot_data$Index + input_data$CFC[DataIntervalStart - 1]
      }
    }
    DataAndTrendPlot <- DataAndTrendPlot + xlab("Failure Number")+ylab("Laplace Test Statistic")
    DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Laplace trend test of"),data_set))
    DataAndTrendPlot <- DataAndTrendPlot + geom_hline(aes(yintercept=c(qnorm(0.1),qnorm(0.05),qnorm(0.01),qnorm(0.001),qnorm(0.0000001),qnorm(0.0000000001)),color=c("0.1","0.05","0.01","0.001","0.0000001","0.0000000001"),linetype="dotted"),alpha=0.8)
    DataAndTrendPlot <- DataAndTrendPlot+xlab("Failure Number")+ylab("Laplace Test Statistic")
  } else if(input$trendPlotChoice=="RA") {
    plot_data <- running_average_test(IF)
    if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data))) > 0)) {
      plot_data$Index <- plot_data$Index + (DataIntervalStart - 1)
    } else {
      if(DataIntervalStart > 1) {
        plot_data$Index <- plot_data$Index + input_data$CFC[DataIntervalStart - 1]
      }
    }
    DataAndTrendPlot <- DataAndTrendPlot + xlab("Failure Number")+ylab("Running Average of Interfailure Times")
    DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Running Average trend test of"),data_set))
  }
  
  names(plot_data) = c("index","trend_test_statistic")
  
  if(input$DataPlotType==1){
    DataAndTrendPlot <- DataAndTrendPlot + geom_point(data=plot_data,aes(index,trend_test_statistic))+ geom_step(data=plot_data)
  }
  if(input$DataPlotType==2){
    DataAndTrendPlot <- DataAndTrendPlot + geom_point(data=plot_data,aes(index,trend_test_statistic))
  }
  if(input$DataPlotType==3){
    DataAndTrendPlot <- DataAndTrendPlot + geom_step(data=plot_data,aes(index,trend_test_statistic))
  }
  DataAndTrendPlot <- DataAndTrendPlot + theme(legend.position = "bottom")
}


if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data))) > 0)) {
  if(length(grep("FT",names(input_data)))>0) {
    FT <- input_data$FT
    IF <- failureT_to_interF(input_data$FT)
  } else if(length(grep("IF",names(input_data)))>0) {
    FT <- interF_to_failureT(data$IF)
    IF <- input_data$IF
  }
} else if((length(grep("CFC",names(input_data)))>0) || (length(grep("FC",names(input_data)))>0)) {
  if (length(grep("CFC",names(input_data)))>0) {
    CFC <- input_data$CFC[input_data$CFC > 0]
    FC <- CumulativeFailureC_to_failureC(CFC)
    T <- input_data$T[input_data$CFC > 0]
  } else if (length(grep("FC",names(input_data)))>0) {
    FC <- input_data$FC[input_data$FC > 0]
    CFC <- FailureC_to_CumulativeFailureC(FC)
    T <- input_data$T[input_data$CFC > 0]
  }
  FT <-failureC_to_failureT(T,FC)
  IF <- failureT_to_interF(failure_T = FT)
}

if(input$trendPlotChoice=="LP") {
  plot_data <- laplace_trend_test(IF)
  q <- q + xlab("Failure Number")+ylab("Laplace Test Statistic")
  q <- q+ggtitle(paste(c("Laplace trend test of"),data_set))
  q <- q + geom_hline(aes(yintercept=c(qnorm(0.1),qnorm(0.05),qnorm(0.01),qnorm(0.001),qnorm(0.0000001),qnorm(0.0000000001)),color=c("0.1","0.05","0.01","0.001","0.0000001","0.0000000001"),linetype="dotted"),alpha=0.8)
  q <- q+xlab("Failure Number")+ylab("Laplace Test Statistic")
} else if(input$trendPlotChoice=="RA") {
  plot_data <- running_average_test(IF)
  q <- q + xlab("Failure Number")+ylab("Running Average of Interfailure Times")
  q <- q+ggtitle(paste(c("Running Average trend test of"),data_set))
}

names(plot_data) = c("index","trend_test_statistic")

if(input$DataPlotType==1){
  q <- q + geom_point(data=plot_data,aes(index,trend_test_statistic))+ geom_step(data=plot_data)
}
if(input$DataPlotType==2){
  q <- q + geom_point(data=plot_data,aes(index,trend_test_statistic))
}
if(input$DataPlotType==3){
  q <- q + geom_step(data=plot_data,aes(index,trend_test_statistic))
}
q <- q + theme(legend.position = "bottom")


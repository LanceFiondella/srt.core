if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data)))>0)) {
  
  # Plot interfailure times or failure times data.
  
  if(length(grep("FT",names(input_data)))>0) {
    source("Data_Format.R")
    FT <- input_data$FT
    IF <- failureT_to_interF(input_data$FT)
  }
  else if(length(grep("IF",names(input_data))) > 0) {
    source("Data_Format.R")
    FT <- interF_to_failureT(data$IF)
    IF <- input_data$IF
  }
  
  if(input$dataPlotChoice == "IF") {
    
    # Interfailure Times vs. Elapsed Test Time
    
    plot_data <- data.frame(FT, IF)
    q <- q+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
  } else if(input$dataPlotChoice == "CF") {
    
    # Cumulative Failures vs. Elapsed Test Time
    
    plot_data <- data.frame(FT, c(1:length(FT)))
    q <- q+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
  } else if(input$dataPlotChoice == "FI") {
    
    # Empirical Failure Intensity vs. Elapsed Test Time
    
    # Compute the empirical failure intensity, which in this case is the inverse of the
    # interfailure time.  In the case of IF values of 0, R will produce a value of
    # "Inf", which will indicate a division of zero but will still be inserted into
    # the vector of failure intensities at the proper place.  When plotting such
    # a value, the data point will be placed somewhat above the upper value of the
    # y scale to separate it from other data points when using ggplot.  DON'T TRY
    # DIVIDING BY 0 IN OTHER PROGRAMMING LANGUAGES!!
    
    plot_data <- data.frame(FT, c(1/IF))
    q <- q+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
  }
  
  names(plot_data) = c("Index","FailureDisplayType")
  
} else if(length(grep("CFC",names(input_data))) || length(grep("FC",names(input_data)))) {
  if(length(grep("CFC", names(input_data)))){
    FC <- CumulativeFailureC_to_failureC(input_data$CFC)
    CFC <- input_data$CFC
  } else if(length(grep("FC", names(input_data)))) {
    FC <- input_data$FC
    CFC <- FailureC_to_CumulativeFailureC(input_data$FC)
  }
  FT <- failureC_to_failureT(input_data$T,FC)
  IF <- failureT_to_interF(failure_T = FT)
  IntervalTime <- input_data$T
  
  if((input$dataPlotChoice == "FC")) {
    
    # Failure Counts vs. Elapsed Test Time
    
    plot_data <- data.frame(IntervalTime, FC)
    q <- q+ggtitle(paste(c("Failure Counts vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Failure Counts per Test Interval"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Failure Counts per Test Interval")
  } else if((input$dataPlotChoice=="IF")) {
    
    # Interfailure Times vs. Elapsed Test Time
    
    plot_data <- data.frame(FT, IF)
    q <- q+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
  } else if(input$dataPlotChoice=="CF") {
    
    # Cumulative Failures vs. Elapsed Test Time
    
    plot_data <- data.frame(IntervalTime, CFC)
    q <- q+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
  } else if(input$dataPlotChoice=="FI") {
    
    # Empirical Failure Intensity vs. Elapsed Test Time
    
    plot_data <- data.frame(IntervalTime, c(FC/IntervalTime))
    q <- q+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),data_set))
    q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
    q <- q + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
    
  }
  
  names(plot_data) = c("Index","FailureDisplayType")
  
}

if(input$DataPlotType==1) {
  
  # Data points and lines
  
  q <- q + geom_point(data=plot_data,aes(Index,FailureDisplayType))+ geom_step(data=plot_data)
}
if(input$DataPlotType==2) {
  
  # Data points only
  
  q <- q + geom_point(data=plot_data,aes(Index,FailureDisplayType))
}
if(input$DataPlotType==3) {
  
  # Lines only
  
  q <- q + geom_step(data=plot_data,aes(Index,FailureDisplayType))
}
q <- q + theme(legend.position = "bottom")


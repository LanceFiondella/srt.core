DataIntervalStart <- input$modelDataRange[1]
DataIntervalEnd <- input$modelDataRange[2]

if((DataIntervalEnd - DataIntervalStart + 1) >= K_minDataModelIntervalWidth) {
  q <- ggplot(,aes_string(x="Index",y="FailureDisplayType"))
  if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data)))>0)) {
    
    IF <- subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)
    FT <- subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)
    FN <- subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FN)
    
    if(input$dataPlotChoice == "IF") {
      
      # Interfailure Times vs. Elapsed Test Time
      
      plot_data <- data.frame(FT, IF)
      q <- q+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),data_set))
      q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
      q <- q + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
    } else if(input$dataPlotChoice == "CF") {
      
      # Cumulative Failures vs. Elapsed Test Time
      
      plot_data <- data.frame(FT, FN)
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
    
  } else if((length(grep("CFC",names(input_data)))>0) || (length(grep("FC",names(input_data)))>0)) {
    FC <- subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)
    CFC <- subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)
    CumT <- subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)
    TI <- subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)
    
    FT <- failureC_to_failureT(CumT, FC)
    IF <- failureT_to_interF(failure_T = FT)
    
    if((input$dataPlotChoice == "FC")) {
      
      # Failure Counts vs. Elapsed Test Time
      
      plot_data <- data.frame(CumT, FC)
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
      
      plot_data <- data.frame(CumT, CFC)
      q <- q+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),data_set))
      q <- q + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
      q <- q + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
    } else if(input$dataPlotChoice=="FI") {
      
      # Empirical Failure Intensity vs. Elapsed Test Time
      
      plot_data <- data.frame(CumT, c(FC/CumT))
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
}

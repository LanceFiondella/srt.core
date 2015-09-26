DataIntervalStart <- input$modelDataRange[1]
DataIntervalEnd <- input$modelDataRange[2]

if((DataIntervalEnd - DataIntervalStart + 1) >= K_minDataModelIntervalWidth) {
  DataAndTrendPlot <- ggplot(,aes_string(x="Index",y="FailureDisplayType"))
  if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data)))>0)) {
    
    IF <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
    FT <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
    FN <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FN)), use.names=FALSE)
    
    if(input$dataPlotChoice == "IF") {
      
      # Interfailure Times vs. Elapsed Test Time
      
      plot_data <- data.frame(FT, IF)
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
    } else if(input$dataPlotChoice == "CF") {
      
      # Cumulative Failures vs. Elapsed Test Time
      
      plot_data <- data.frame(FT, FN)
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
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
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
    }
    
    names(plot_data) = c("Index","FailureDisplayType")
    
  } else if((length(grep("CFC",names(input_data)))>0) || (length(grep("FC",names(input_data)))>0)) {
    FC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)), use.names=FALSE)
    CFC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)), use.names=FALSE)
    CumT <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)), use.names=FALSE)
    TI <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)), use.names=FALSE)
<<<<<<< HEAD

=======
    
>>>>>>> allen-development
    FT <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FT)), use.names=FALSE)
    IF <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_IF)), use.names=FALSE)
    
    if((input$dataPlotChoice == "FC")) {
      
      # Failure Counts vs. Elapsed Test Time
      
      plot_data <- data.frame(CumT, FC)
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Failure Counts vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Failure Counts per Test Interval"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Failure Counts per Test Interval")
    } else if((input$dataPlotChoice=="IF")) {
      
      # Interfailure Times vs. Elapsed Test Time
      
      plot_data <- data.frame(FT, IF)
      plot_data$FT <- plot_data$FT + input_data$T[DataIntervalStart]
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
    } else if(input$dataPlotChoice=="CF") {
      
      # Cumulative Failures vs. Elapsed Test Time
      
      plot_data <- data.frame(CumT, CFC)
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
    } else if(input$dataPlotChoice=="FI") {
      
      # Empirical Failure Intensity vs. Elapsed Test Time
      
      plot_data <- data.frame(FT, c(1/IF))
      DataAndTrendPlot <- DataAndTrendPlot+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),data_set))
      DataAndTrendPlot <- DataAndTrendPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
      DataAndTrendPlot <- DataAndTrendPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
      
    }
    
    names(plot_data) = c("Index","FailureDisplayType")
    
  }
  
<<<<<<< HEAD
  if(input$DataPlotType==1) {
    
    # Data points and lines
    
    DataAndTrendPlot <- DataAndTrendPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType))+ geom_step(data=plot_data)
  }
  if(input$DataPlotType==2) {
=======
  if(input$DataPlotType=="points_and_lines") {
    
    # Data points and lines
    
    DataAndTrendPlot <- DataAndTrendPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType))+ geom_step(data=plot_data)
  }
  if(input$DataPlotType=="points") {
>>>>>>> allen-development
    
    # Data points only
    
    DataAndTrendPlot <- DataAndTrendPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType))
  }
<<<<<<< HEAD
  if(input$DataPlotType==3) {
=======
  if(input$DataPlotType=="lines") {
>>>>>>> allen-development
    
    # Lines only
    
    DataAndTrendPlot <- DataAndTrendPlot + geom_step(data=plot_data,aes(Index,FailureDisplayType))
  }
  DataAndTrendPlot <- DataAndTrendPlot + theme(legend.position = "bottom")
<<<<<<< HEAD
}
=======
}
>>>>>>> allen-development

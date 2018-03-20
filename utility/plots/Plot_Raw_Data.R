plot_failure_data <- function(in_data, convertedFCData, DataName, DataRange, DataView, PlotType, MinIntervalWidth) {
  
  require(ggplot2)
  
  PlotFault <- FALSE
  
  DataIntervalStart <- DataRange[1]
  DataIntervalEnd <- DataRange[2]
  # Initialize the plot.
  
  localDataPlot <- ggplot()
  if('FCount' %in% names(in_data)){
            convertedFCData <- in_data.FC
  }
  
  if('FRate' %in% names(in_data)){
  in_data <- in_data.FR
  }
  
  if((DataIntervalEnd - DataIntervalStart + 1) >= MinIntervalWidth) {
    localDataPlot <- ggplot(,aes_string(x="Index",y="FailureDisplayType"))
    if((length(grep("FT",names(in_data)))>0) || (length(grep("IF",names(in_data)))>0)) {
      
      IF <- c(unlist(subset(subset(in_data, in_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
      FT <- c(unlist(subset(subset(in_data, in_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
      FN <- c(unlist(subset(subset(in_data, in_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FN)), use.names=FALSE)
      
      if(DataView == "IF") {
        
        # Interfailure Times vs. Elapsed Test Time
        
        plot_data <- data.frame(FT, IF)
        localDataPlot <- localDataPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
      } else if(DataView == "CF") {
        
        # Cumulative Failures vs. Elapsed Test Time
        
        plot_data <- data.frame(FT, FN)
        localDataPlot <- localDataPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
      } else if(DataView == "FI") {
        
        # Empirical Failure Intensity vs. Elapsed Test Time
        
        # Compute the empirical failure intensity, which in this case is the inverse of the
        # interfailure time.  In the case of IF values of 0, R will produce a value of
        # "Inf", which will indicate a division of zero but will still be inserted into
        # the vector of failure intensities at the proper place.  When plotting such
        # a value, the data point will be placed somewhat above the upper value of the
        # y scale to separate it from other data points when using ggplot.  DON'T TRY
        # DIVIDING BY 0 IN OTHER PROGRAMMING LANGUAGES!!
        
        plot_data <- data.frame(FT, c(1/IF))
        localDataPlot <- localDataPlot+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
      } else {
        # Couldn't identify view of data to display.
        # #print an error message.
        
        PlotFault <- TRUE
      }
      
      names(plot_data) = c("Index","FailureDisplayType")
      
    } else if((length(grep("CFC",names(in_data)))>0) || (length(grep("FC",names(in_data)))>0)) {
      FC <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)), use.names=FALSE)
      CFC <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)), use.names=FALSE)
      CumT <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)), use.names=FALSE)
      TI <- c(unlist(subset(subset(in_data, in_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)), use.names=FALSE)
      
      FT <- c(unlist(subset(subset(convertedFCData, convertedFCData$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FT)), use.names=FALSE)
      IF <- c(unlist(subset(subset(convertedFCData, convertedFCData$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_IF)), use.names=FALSE)
      
      if((DataView == "FC")) {
        
        # Failure Counts vs. Elapsed Test Time
        
        plot_data <- data.frame(CumT, FC)
        localDataPlot <- localDataPlot+ggtitle(paste(c("Failure Counts vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Failure Counts per Test Interval"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Failure Counts per Test Interval")
      } else if((DataView=="IF")) {
        
        # Interfailure Times vs. Elapsed Test Time
        
        plot_data <- data.frame(FT, IF)
        plot_data$FT <- plot_data$FT + in_data$T[DataIntervalStart]
        localDataPlot <- localDataPlot+ggtitle(paste(c("Interfailure Times vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Times Between Successive Failures"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Times Between Successive Failures")
      } else if(DataView=="CF") {
        
        # Cumulative Failures vs. Elapsed Test Time
        
        plot_data <- data.frame(CumT, CFC)
        localDataPlot <- localDataPlot+ggtitle(paste(c("Cumulative Failures vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Cumulative Number of Failures"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Cumulative Number of Failures")
      } else if(DataView=="FI") {
        
        # Empirical Failure Intensity vs. Elapsed Test Time
        
        plot_data <- data.frame(FT, c(1/IF))
        localDataPlot <- localDataPlot+ggtitle(paste(c("Empirical Failure Intensity vs. Cumulative Test Time of"),DataName))
        localDataPlot <- localDataPlot + scale_color_manual(name = "Legend",  labels = c("Cumulative Test Time", "Number of Failures per Unit Time"),values = c("blue","red"))
        localDataPlot <- localDataPlot + xlab("Cumulative Test Time")+ylab("Number of Failures per Unit Time")
        
      } else {
        # Couldn't identify view of data to display.
        # #print an error message.
        
        PlotFault <- TRUE
      }
      
      names(plot_data) = c("Index","FailureDisplayType")
      
    }
    
     else {
      # Couldn't determine whether we're working with IF or FC data.
      # #print an error message.
      
      PlotFault <- TRUE
    }
    
    if(PlotType=="points_and_lines") {
      
      # Data points and lines
      
      localDataPlot <- localDataPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType))+ geom_step(data=plot_data)
    }
    if(PlotType=="points") {
      
      # Data points only
      
      localDataPlot <- localDataPlot + geom_point(data=plot_data,aes(Index,FailureDisplayType))
    }
    if(PlotType=="lines") {
      
      # Lines only
      
      localDataPlot <- localDataPlot + geom_step(data=plot_data,aes(Index,FailureDisplayType))
    }
    localDataPlot <- localDataPlot + theme(legend.position = "bottom", text = element_text(size=14))
  } else {
    PlotFault <- TRUE
  }
  
  if(PlotFault) {
    localDataPlot = NULL
  }
  return(localDataPlot)
}

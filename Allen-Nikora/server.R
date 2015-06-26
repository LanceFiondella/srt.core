library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("GO_BM.R")
source("Data_Format.R")
source("Laplace_trend_test.R")

shinyServer(function(input, output) {#reactive shiny fuction
  
  output$distPlot <- renderPlot({ #reactive function, basically Main()
    
    inFile <- input$file
    #print(inFile)#Read of input file
    if (is.null(inFile))#error handling for null file pointer
      return("Please Upload a CSV File")
    if (input$type==1)
      data_set <- input$dataSheetChoice
      #print(data_set)
      
      data <- read.xls(inFile$datapath,sheet=data_set)#Reads xls and xlsx files. Error handling needed
    if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    
    #if (data[1] =="FC")
    #two coumns
    #else
    #one column
    
    
    
    Time <- names(data[1])#generic name of column name of data frame (x-axis)
    Failure <- names(data[2])#(y-axis)
    #p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    #value <- c("blue","red")
    q <- ggplot(,aes_string(x="index",y="laplace_factor"))
    model <- ""
    if(input$trendPlotChoice=="LP"){
      #-------------------------------------------------------------
      
      input_data <- data
      if(length(grep("[DATA]",data_set)) >0){
        #input_data <- data
        source("Data_Format.R")
        FC <- CumulativeFailureC_to_failureC(input_data$CFC)
        FT <-failureC_to_failureT(input_data$T,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- laplace_trend_test(IF)
      }
      else if(length(grep("J",data_set))>0){
        source("Data_Format.R")
        FC <-CumulativeFailureC_to_failureC(input_data$CFC)
        FT <-failureC_to_failureT(input_data$TI,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- laplace_trend_test(IF)
      }
      else if(data_set=="CDS"){
        IF <- failureT_to_interF(input_data$FT)
        sol <- laplace_trend_test(IF)
      }
      else{
        sol <- laplace_trend_test(input_data$IF)
      }
      
      
      
      # ------------------------------------------------------------
      plot_data <- sol
      names(plot_data) = c("index","laplace_factor")
      print(plot_data)
      if(input$DataPlotType==1){
        q <- q + geom_point(data=plot_data,aes(index,laplace_factor))+ geom_line(data=plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
      }
      if(input$DataPlotType==2){
        q <- q + geom_point(data=plot_data,aes(index,laplace_factor))#+ geomline(data=plot_data)
      }
      if(input$DataPlotType==3){
        q <- q + geom_line(data=plot_data,aes(index,laplace_factor))
      }
      q <- q+ggtitle(paste(c("Laplace trend of "),data_set))
      }
      q <- q + theme(legend.position = c(0.9, 0.9)) 
      #label <- c("Trend test")
      #value <- c("")
    
    #q <- q + scale_color_manual(labels =c("laplace_trend"),values = c("blue","blue"))
    #p <- p + scale_color_manual(labels = c("plotx","test"),values = c("blue","blue"))
    
    q
    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
})


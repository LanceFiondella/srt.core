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
    
    inFile <- input$file #Read of input file
    if (is.null(inFile))#error handling for null file pointer
      return("Please Upload a CSV File")
    if (input$type==1)
      data_set <- input$DataSet
      
      data <- read.xls(inFile$datapath,sheet=data_set)#Reads xls and xlsx files. Error handling needed
    if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    
    #if (data[1] =="FC")
    #two coumns
    #else
    #one column
    
    
    
    Time <- names(data[1])#generic name of column name of data frame (x-axis)
    Failure <- names(data[2])#(y-axis)
    p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    value <- c("blue","red")
    q <- ggplot(,aes_string(x="index",y="laplace_factor"))
    model <- ""
    if(input$TrendTest=="LP"){
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
      #print(plot_data)
      q <- q + geom_point(data=plot_data,aes(color="blue"))
      #label <- c("Trend test")
      #value <- c("")
    }
    if (input$OD == TRUE){
      p <- p + geom_point(data = data,aes(color="blue",group="Original Data")) + geom_line(data = data,aes(color="blue",group="Original Data"))#adds scatter plot points to plot object
      label <- c("Original Data","")
      value <- c("blue","red")
    }
    p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    if (input$Model == "JM"){
      newdata <- JMmodel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Jolinski-Moranda Model"))
      model <- c("Jolinski-Moranda Model")
    }
    if (input$Model == "GEO"){
      newdata <- GeoModel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Geometric Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Geometric Model"))
      model <- c("Geometric Model")
    }
    if (input$Model == "GO"){
      y <- as.vector(data[,2])
      #print(y)
      newdata <- GO_BM_MLE(y)
      newdata <- y * as.vector(newdata)
      newdata <- data.frame(newdata)
      data[,2] <- newdata[,1]
      #print(data)
      p <- p + geom_point(data=data,aes(color="red",group="Geol-Okumoto Model"))
      p <- p + geom_line(data=data,aes(color="red",group="Geol-Okumoto Model"))
      model <- c("Geol-Okumoto Model")
    }
    if (input$Model == "YS"){
      newdata <- YamadaModel(data)
      p <- p + geom_point(data=newdata,aes(color="red",group="Yamada S-Shaped Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Yamada S-Shaped Model"))
      model <- c("Yamada S-Shaped Model")
    }
    if(input$OD == FALSE){
      label = c(model,"")
    }else{
      label = c("Original Data",model)
    }
    p <- p + scale_color_manual(name = "Legend",  labels = label,values = value)
    
    p
    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
})


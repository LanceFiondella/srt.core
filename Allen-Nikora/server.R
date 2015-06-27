library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("JM_BM.R")
source("GO_BM.R")
source("Data_Format.R")
source("Laplace_trend_test.R")
source("RA_Test.R")
data_global <- data.frame()
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
      data_global <<- data
    if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    print(data)
    #if (data[1] =="FC")
    #two coumns
    #else
    #one column
    
    
    
    Time <- names(data[1])#generic name of column name of data frame (x-axis)
    Failure <- names(data[2])#(y-axis)
    #p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    #value <- c("blue","red")
    
    model <- ""
    if(input$trendPlotChoice=="LP"){
      #-------------------------------------------------------------
      q <- ggplot(,aes_string(x="index",y="laplace_factor"))
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
      if(input$DataPlotType==1){
        q <- q + geom_point(data=plot_data,aes(index,laplace_factor))+ geom_line(data=plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
      }
      if(input$DataPlotType==2){
        q <- q + geom_point(data=plot_data,aes(index,laplace_factor))#()#+ geomline(data=plot_data)
      }
      if(input$DataPlotType==3){
        q <- q + geom_line(data=plot_data,aes(index,laplace_factor))
      }
      #q <- q + geom_smooth()
      q <- q+ggtitle(paste(c("Laplace trend of "),data_set))
      #legend(title="LP")
      }


      if(input$trendPlotChoice=="RA"){
        q <- ggplot(,aes_string(x="Index",y="Running_Average"))

      #-------------------------------------------------------------
      
      input_data <- data
      if(length(grep("[DATA]",data_set)) >0){
        #input_data <- data
        source("Data_Format.R")
        FC <- CumulativeFailureC_to_failureC(input_data$CFC)
        FT <-failureC_to_failureT(input_data$T,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- running_average_test(IF)
      }
      else if(length(grep("J",data_set))>0){
        source("Data_Format.R")
        FC <-CumulativeFailureC_to_failureC(input_data$CFC)
        FT <-failureC_to_failureT(input_data$TI,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- running_average_test(IF)
      }
      else if(data_set=="CDS"){
        IF <- failureT_to_interF(input_data$FT)
        sol <- running_average_test(IF)
      }
      else{
        sol <- running_average_test(input_data$IF)
      }
      
      
      
      # ------------------------------------------------------------
      plot_data <- sol
      names(plot_data) = c("Index","Running_Average")
      print(plot_data)
      if(input$DataPlotType==1){
        q <- q + geom_point(data=plot_data,aes(Index,Running_Average))+ geom_line(data=plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
      }
      if(input$DataPlotType==2){
        q <- q + geom_point(data=plot_data,aes(Index,Running_Average))#+ geomline(data=plot_data)
      }
      if(input$DataPlotType==3){
        q <- q + geom_line(data=plot_data,aes(Index,Running_Average))
      }
      q <- q+ggtitle(paste(c("Running Average trend test of "),data_set))
      #legend(title="RA")
      }
      q <- q + theme(legend.position = c(0.9, 0.9))
            #label <- c("Trend test")
      #value <- c("")
    q
    #q <- q + scale_color_manual(labels =c("laplace_trend"),values = c("blue","blue"))
    #p <- p + scale_color_manual(labels = c("plotx","test"),values = c("blue","blue"))
   
    
    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
  output$MVFPlot <- renderPlot({
    data <- data_global
    #data
    Time <- "Time"
    Failure <- "Failure"
    #Time <- names(data[1])#generic name of column name of data frame (x-axis)
    #Failure <- names(data[2])#(y-axis)
    #p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    value <- c("blue","red")
    #p <- ggplot(,aes_string(x=Time,y=Failure))
    if(input$runModels!=0){          ###################should think of isolate here
      plus <- 0
      if(length(input$modelResultChoice)>0){
        for( i in input$modelResultChoice){
          if(i==6){

            p <- ggplot(,aes_string(x=Time,y=Failure))
            print("_____________________________")
            print(data)


            # if("IF" in names(data))
            #
            #
            # else
            #
            #

            new_params <- JM_BM_MLE(data$IF)
            #print(data)
            frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
            print(frame_params)
            mvf_plot_data <- JM_MVF(frame_params,data)
            #names(plot_data) = c("Index","Running_Average")
            
            print(mvf_plot_data)
            if(input$DataPlotType==1){
              p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
            }
            if(input$DataPlotType==2){
              p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
            }
            if(input$DataPlotType==3){
              p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
            }
            if(input$checkboxDataOnPlot){
              original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)


              # Temporary solution
              # original data could be arranged in any way
              # So we should come up with some solution here
              #
              #
              #
              #
              #

              print("-----------------------------------")
              print(original_data)
              p <- p + geom_line(data=original_data,aes(Time,Failure))
            }
            p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))
            #legend(title="RA"
            
            #label <- c("Trend test")
      #value <- c(""
            p("Jelinski Moranda")
          }
          else if(i==3){
            p("Hello i am Geometric model")
          }
          else if(i==8){
            print("Goalokulo")
          }
          else{
            print("Other")
          }
          p <- p + theme(legend.position = c(0.9, 0.9))
        }
      }
    }
    #print(input$runModels)
    #p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    #print(input$modelResultChoice)
    
    p


    })
})


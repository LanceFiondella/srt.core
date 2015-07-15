library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("JM_BM.R")
source("GO_BM.R")
source("GM_BM.R")
source("Data_Format.R")
source("Laplace_trend_test.R")
source("RA_Test.R")
source("ErrorMessages.R")  # Text for error messages

# Initialize constants ------------------------------------

K_minDataModelIntervalWidth <- 5

K_CategoryFirst <- 1
K_CategoryLast <- 5

# Start main program ------------------------------------

openFilename <- ""
data_global <- data.frame()

shinyServer(function(input, output, clientData, session) {#reactive shiny function

  output$distPlot <- renderPlot({ #reactive function, basically Main()
    
    inFile <- input$file
    #print(inFile)#Read of input file
    if (is.null(inFile))#error handling for null file pointer
      return("Please Upload a CSV File")
    if (input$type==1) {
      if(inFile$name != openFilename) {
          tempArray <- c(sheetNames(inFile$datapath))
        
          # Populate the list of data sheet names in the Excel file.
        
          filelistLength <- length(tempArray)
          fileList <- list()
          for (i in 1:filelistLength) {
            fileList[[tempArray[i]]] <- tempArray[i]
          }
          updateSelectInput(session, "dataSheetChoice", choices = fileList)
          openFilename <<- inFile$name
      }
      data_set <- input$dataSheetChoice
    }
      
      data <- read.xls(inFile$datapath,sheet=data_set)#Reads xls and xlsx files. Error handling needed
      data_global <<- data
    if (input$type==2) {
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling
    }
    #print(data)
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
        FT <-failureC_to_failureT(input_data$T,FC)
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

      
      q <- q + geom_hline(aes(yintercept=c(qnorm(0.025),qnorm(0.10),qnorm(0.001),qnorm(0.0000001),qnorm(0.0000000001)),color=c("0.025","0.1","0.001","0.0000001","0.0000000001"),linetype="dotted"),alpha=0.8)
      #q <- q + scale_color_manual("Signigicance",value=c("0.025"="blue","0.1"="green","0.001"="black","0.0000001"="red","0.0000000001"="g"))
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
        FT <-failureC_to_failureT(input_data$T,FC)
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
      #print(plot_data)
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
      #g <- c(0.025,0.1,0.001,0.0001)
      
      #q <- q+geom_hline(aes(yintercept=qnorm(g[1],g[2],g[3],g[4])))
      q <- q + theme(legend.position = c(0.9, 0.9))
            #label <- c("Trend test")
      #value <- c("")
    q
    #q <- q + scale_color_manual(labels =c("laplace_trend"),values = c("blue","blue"))
    #p <- p + scale_color_manual(labels = c("plotx","test"),values = c("blue","blue"))
   
    
    #plot(data) Leave this here to use if ggplot() stops working. 
  } )
  
  output$ModelPlot <- renderPlot({
    data <- data_global
    #data
    Time <- "Time"
    Failure <- "Failure"
    #Time <- names(data[1])#generic name of column name of data frame (x-axis)
    #Failure <- names(data[2])#(y-axis)
    #p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    value <- c("blue","red")
    #p <- ggplot(,aes_string(x=Time,y=Failure))
    #mvf_plot_data <- data.frame()
    if(input$runModels!=0){          ###################should think of isolate here
      plus <- 0
      if(length(input$modelResultChoice)>0){
        p <- ggplot(,aes_string(x=Time,y=Failure));
          for(i in input$modelResultChoice){
            if(i==6){
            if(length(grep("IF",names(data)))){
              if(input$modelPlotChoice==2){
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))#+ geomline(data=plot_data)
              }

              if(input$modelPlotChoice==1){
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice))
              }

              if(input$modelPlotChoice==3){
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))

                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice))

              }
              if(input$modelPlotChoice==4){
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))

                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Reliabililty function plot of"),input$dataSheetChoice))
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              if(input$modelPlotChoice==2){
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))
              }
              if(input$modelPlotChoice==1){
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice))
              }
              if(input$modelPlotChoice==3){
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice))
              }
              if(input$modelPlotChoice==4){
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice))
              }
            }
            else if(length(grep("CFC",names(data)))){
              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              if(input$modelPlotChoice==2){
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))

                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))                
              }
              if(input$modelPlotChoice==1){
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Time Between Failure function plot of"),input$dataSheetChoice))
              }
              if(input$modelPlotChoice==3){
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice))
              }
              if(input$modelPlotChoice==4){
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure))
                }
                p <- p+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice))
              }
            }
          }

          else if(i==3){
          if(length(grep("IF",names(data)))){
              if(input$modelPlotChoice==2){
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==1){
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Time Between Failure function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==3){
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==4){
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              if(input$modelPlotChoice==2){
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==1){
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=IF)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==3){
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==4){
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
              }
            }
            else if(length(grep("CFC",names(data)))){
              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              if(input$modelPlotChoice==2){
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==1){
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==3){
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
              }
              if(input$modelPlotChoice==4){
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p <- p + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p <- p + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p <- p + geom_line(data=original_data,aes(Time,Failure));
                }
                p <- p+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
              }
            }          
        }
        else if(i==8){
          print("Goel-okumoto");
        }
        else{
          print("Other");
        }
        #print("hello i am out");
        p <- p + theme(legend.position = c(0.9, 0.9));
        #print("hello i am a out");
          
          
          #print("i ==6")
        }
      #print("model selections")
    }
    #print("i am far out");
    #print(input$runModels)
    #p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    #print(input$modelResultChoice)
    }
    p



    })
})


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
data_global <- data.frame()

shinyServer(function(input, output) {


  output$sheetChoice <- renderUI({
    if(input$type==1){
      inFile <- input$file
      if(is.null(inFile)){
      return("Please upload an excel file")
    }
      sheets_present <- sheetNames(xls=inFile$datapath)
      # #print(sheets_present)
      selectInput("dataSheetChoice","Choose Sheet", c(NULL,sheets_present))
    }
    else{
      #textInput("dataSheetChoice","Choose Sheet", c("test"))
      return("Please upload a csv file")
    }
    })

  output$message <- renderUI({
      sliderInput('test', 'test_label', 0, 5, 3, step = 1, round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = TRUE, width = NULL, sep = ",", pre = NULL, post = NULL)

      #animationOptions(interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL)
      #p("HEllO")
    })
  
  output$distPlot <- renderPlot({ #reactive function, basically Main()


    inFile <- input$file

    

    if(is.null(inFile)){
      return("Please upload an excel file")
    }


    if(input$type==1){

      if(length(grep(".csv",inFile$name))>0){
        return("Please upload excel sheet")
      }

      if(is.null(input$dataSheetChoice)){
        return("No sheet selected")
      }

      data_set <- input$dataSheetChoice

      data <- read.xls(inFile$datapath,sheet=data_set)
      data_global <<- data

    }
   
  #   inFile <- input$file
  #   #
  #   ##print(inFile)#Read of input file
  #   sheet_count <- sheetCount(xls=inFile$datapath)
  #   sheets_present <- NULL
  #   if (is.null(inFile))#error handling for null file pointer
  #     return("Please Upload a CSV File")
  #   else if (input$type==1){
      
  #     ##print(sheets_present)
  #     if(is.null(sheets_present)){
  #       return("No sheets present")
  #     }
  #     if(sheet_count > 0){
  #     output$sheetChoice <- renderUI({
  #       sheets_present <- sheetNames(xls=inFile$datapath)
  #       #print(sheets_present)
  #       selectInput("dataSheetChoice", "Choose Sheet", c('',sheets_present),selected=NULL)
  #     })
  #  }
    

  #   if(!is.null(input$dataSheetChoice)){
  #     data_set <- input$dataSheetChoice
      
  #     data <- read.xls(inFile$datapath,sheet=data_set)#Reads xls and xlsx files. Error handling needed
  #     data_global <<- data
  #   }
  # }
    

    else if (input$type==2){
      if(length(grep(".xls",inFile$name))>0){
        #print(inFile)
        return("Please upload excel sheet")
      }
      #print(inFile)
      data <- read.csv(inFile$datapath, head = TRUE, sep = ',', quote = " % ")#same as before needs error handling
      data_set <- "input data"
    }
    ##print(data)
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
      #print(names(input_data))
      # if(length(grep("[DATA]",data_set)) >0){
      #   #input_data <- data
      #   source("Data_Format.R")
      #   FC <- CumulativeFailureC_to_failureC(input_data$CFC)
      #   FT <-failureC_to_failureT(input_data$T,FC)
      #   IF <- failureT_to_interF(failure_T = FT)
      #   sol <- laplace_trend_test(IF)
      # }
      # else if(length(grep("J",data_set))>0){
      #   source("Data_Format.R")
      #   FC <-CumulativeFailureC_to_failureC(input_data$CFC)
      #   FT <-failureC_to_failureT(input_data$T,FC)
      #   IF <- failureT_to_interF(failure_T = FT)
      #   sol <- laplace_trend_test(IF)
      # }
      # else if(data_set=="CDS"){
      #   IF <- failureT_to_interF(input_data$FT)
      #   sol <- laplace_trend_test(IF)
      # }
      # else{
      #   sol <- laplace_trend_test(input_data$IF)
      # }

      if(length(grep("FT",names(input_data)))>0) {
        ##print("PICKED FT pattern")
        #FT <- input_data$FT > 0
        source("Data_Format.R")
        IF <- failureT_to_interF(input_data$FT)
        sol <- laplace_trend_test(IF)
      }
      else if(length(grep("IF",names(input_data))) > 0) {

        ##print("PICKED IF pattern")
        source("Data_Format.R")
        #IF <- failureT_to_interF(input_data$FT)
        IF <- input_data$IF
        sol <- laplace_trend_test(IF)
      }
      else if(length(grep("CFC",names(input_data)))>0) { 
        ##print("PICKED CFC pattern")     
        source("Data_Format.R")
        CFC <- input_data$CFC[input_data$CFC > 0]
        FC <- CumulativeFailureC_to_failureC(CFC)
        T <- input_data$T[input_data$CFC > 0]
        FT <-failureC_to_failureT(T,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- laplace_trend_test(IF)
      }
      else if(length(grep("FC",names(input_data)))>0){
        ##print("PICKED FC pattern")
        source("Data_Format.R")
        FC <- input_data$FC[input_data$FC > 0]
        T <- input_data$T[input_data$CFC > 0]
        FT <-failureC_to_failureT(T,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- laplace_trend_test(IF)
      }
      
      
      
      # ------------------------------------------------------------
      plot_data <- sol
      names(plot_data) = c("index","laplace_factor")
      ##print(plot_data)
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

      
      q <- q + geom_hline(aes(yintercept=c(qnorm(0.1),qnorm(0.05),qnorm(0.01),qnorm(0.001),qnorm(0.0000001),qnorm(0.0000000001)),color=c("0.1","0.05","0.01","0.001","0.0000001","0.0000000001"),linetype="dotted"),alpha=0.8)
      #q <- q + scale_color_manual("Signigicance",value=c("0.025"="blue","0.1"="green","0.001"="black","0.0000001"="red","0.0000000001"="g"))
      }


      if(input$trendPlotChoice=="RA"){
        q <- ggplot(,aes_string(x="Index",y="Running_Average"))

      #-------------------------------------------------------------
      
      input_data <- data



      if(length(grep("FT",names(input_data)))>0) {
        ##print("PICKED FT pattern")
        #FT <- input_data$FT > 0
        source("Data_Format.R")
        IF <- failureT_to_interF(input_data$FT)
        sol <- running_average_test(IF)
      }
      else if(length(grep("IF",names(input_data))) > 0) {

        ##print("PICKED IF pattern")
        source("Data_Format.R")
        IF <- input_data$IF
        sol <- running_average_test(IF)
      }
      else if(length(grep("CFC",names(input_data)))>0) { 
        ##print("PICKED CFC pattern")     
        source("Data_Format.R")
        CFC <- input_data$CFC[input_data$CFC > 0]
        FC <- CumulativeFailureC_to_failureC(CFC)
        T <- input_data$T[input_data$CFC > 0]
        FT <-failureC_to_failureT(T,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- running_average_test(IF)
      }
      else if(length(grep("FC",names(input_data)))>0){
        ##print("PICKED FC pattern")
        source("Data_Format.R")
        FC <- input_data$FC[input_data$FC > 0]
        T <- input_data$T[input_data$CFC > 0]
        FT <-failureC_to_failureT(T,FC)
        IF <- failureT_to_interF(failure_T = FT)
        sol <- running_average_test(IF)
      }
      # if(length(grep("[DATA]",data_set)) >0){
      #   #input_data <- data
      #   source("Data_Format.R")
      #   FC <- CumulativeFailureC_to_failureC(input_data$CFC)
      #   FT <-failureC_to_failureT(input_data$T,FC)
      #   IF <- failureT_to_interF(failure_T = FT)
      #   sol <- running_average_test(IF)
      # }
      # else if(length(grep("J",data_set))>0){
      #   source("Data_Format.R")
      #   FC <-CumulativeFailureC_to_failureC(input_data$CFC)
      #   FT <-failureC_to_failureT(input_data$T,FC)
      #   IF <- failureT_to_interF(failure_T = FT)
      #   sol <- running_average_test(IF)
      # }
      # else if(data_set=="CDS"){
      #   IF <- failureT_to_interF(input_data$FT)
      #   sol <- running_average_test(IF)
      # }
      # else{
      #   sol <- running_average_test(input_data$IF)
      # }
      
      
      
      # ------------------------------------------------------------
      plot_data <- sol
      names(plot_data) = c("Index","Running_Average")
      ##print(plot_data)
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
    q <- ggplot()
    p1 <- ggplot()
    p2 <- ggplot()
    if(input$runModels!=0){          ###################should think of isolate here
      plus <- 0
      if(length(input$modelResultChoice)>0){
        
          for(i in input$modelResultChoice){
            if(i==6){
            if(length(grep("IF",names(data)))){
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="lines and dots"))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="dots"))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure,color="lines"))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure,color="Original Data"))
                }
                p1 <- p1 + ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))#+ geomline(data=plot_data)
                p1 <- p1 + theme(legend.position = c(0.1, 0.9));
                p1 <- p1 + scale_color_manual(name = "Legend",  labels = c("MVF","Original Data"),values = c("blue","red"))
                #q <- q + p
              }

              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_T(frame_params,data)
                


                #data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                #mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_point(data=original_data,aes(Time,Failure))
                }
                p1 <- p1 + ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice))
                #q <- q + p
              }

              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))

                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure))
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice))
                #q <- q + p

              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))

                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure))
                # }
                p1 <- p1 + ggtitle(paste(c("Reliabililty function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1+ geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure))
                }
                p1 <- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1+ geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1+ geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1+ geom_line(data=original_data,aes(Time,Failure))
                }
                p1 <- p1+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure))
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure))
                # }
                p1 <- p1+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))


                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure))
                }
                p1 <- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))  
                #q <- q + p              
              }
              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure))
                }
                p1 <- p1+ggtitle(paste(c("Time Between Failure Function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure))
                # }
                p <- p+ggtitle(paste(c("Failure Intensity Function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- JM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1<- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure))
                # }
                p1 <- p1+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice))
                #q <- q + p
              }
            }
          }

          else if(i==3){
          if(length(grep("IF",names(data)))){
              if(input$modelPlotChoice==2){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="lines and dots"))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="dots"))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure,color="lines"))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p2 <- p2 + geom_line(data=original_data,aes(Time,Failure));
                }
                p2<- p2+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                p2 <- p2 + theme(legend.position = c(0.1, 0.9));
                p2 <- p2 + scale_color_manual(name = "Legend",  labels = c("GM_MVF","Original Data"),values = c("black","grey"))
                #q <- q + p
              }
              if(input$modelPlotChoice==1){

                p2 <- ggplot(,aes_string(x=Time,y=Failure));

                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_T(frame_params,data)
                
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p2 <- p2 + geom_point(data=original_data,aes(Time,Failure));
                }
                p2 <- p2+ggtitle(paste(c("Time Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){

                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2+ geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p2 <- p2+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p2 <- p2 + ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              if(input$modelPlotChoice==2){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p2 <- p2 + geom_line(data=original_data,aes(Time,Failure));
                }
                p2 <- p2+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=IF)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p2 <- p2 + geom_line(data=original_data,aes(Time,Failure));
                }
                p2 <- p2+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p2 <- p2+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p2 <- p2+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              if(input$modelPlotChoice==2){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p2 <- p2 + geom_line(data=original_data,aes(Time,Failure));
                }
                p2 <- p2 + ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p2 <- p2 + geom_line(data=original_data,aes(Time,Failure));
                }
                p2 <- p2+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2+ geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p2 <- p2+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p2 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p2 <- p2 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p2 <- p2 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p2 <- p2 +ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }          
        }
        else if(i==8){
          #print("Goel-okumoto");
        }
        else{
          #print("Other");
        }
        ##print("hello i am out");
        
        ##print("hello i am a out");
          
          
          ##print("i ==6")
        }
      ##print("model selections")
    }
    ##print("i am far out");
    ##print(input$runModels)
    #p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    ##print(input$modelResultChoice)
    }
    #plotties <- c(p1,p2)
    ##print(plotties$layers)
    #plotSet <- length(plotties$layers)>0
    ##print(length(plotSet))
     p <- multiplot(p1,p2,cols=1)
    p
    })
})

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    #print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      #print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

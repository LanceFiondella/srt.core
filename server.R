library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
#library(DT)

source("custom_functions.R")
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("JM_BM.R")
source("GO_EM_FT.R")
source("GO_BM_FT.R")
source("GM_BM.R")
source("Data_Format.R")
source("Laplace_trend_test.R")
source("RA_Test.R")
source("ErrorMessages.R")  # Text for error messages

# Initialize "constants" ------------------------------------

K_minDataModelIntervalWidth <- 5

K_CategoryFirst <- 1
K_CategoryLast <- 5

# Start main program ------------------------------------

openFileDatapath <- ""
#data_global <- data.frame()
data_original <- data.frame()

shinyServer(function(input, output, clientData, session) {
  
  output$sheetChoice <- renderUI({
    if(input$type==1){
      inFile <- input$file
      if(is.null(inFile)){
      return("Please upload an excel file")
    }
      sheets_present <- sheetNames(xls=inFile$datapath)
      # print(sheets_present)
      selectInput("dataSheetChoice","Choose Sheet", c(NULL,sheets_present))
    }
    else{
      #textInput("dataSheetChoice","Choose Sheet", c("test"))
      return("Please upload a csv file")
    }
    })

  #output$message <- renderUI({
  #    sliderInput('test', 'test_label', 0, 5, 3, step = 1, round = FALSE,  ticks = TRUE, animate = TRUE, width = NULL)
  #    animationOptions(interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL)
  #    p("HEllO")
  #  })
  
  
  # Select and read in a data file.  This is a reactive data item.
  
  data_global <- reactive({
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
      data_original <<- data
    } else if (input$type==2){
      if(length(grep(".xls",inFile$name))>0){
        print(inFile)
        return("Please upload excel sheet")
      }
      print(inFile)
      data <- read.csv(inFile$datapath, head = TRUE, sep = ',', quote = " % ")#same as before needs error handling
      data_original <<- data
      data_set <- inFile$filename
    }
    
    # Set up the initial values for modeling data range and the initial parameter
    # estimation range
    
    DataModelIntervalStart <- 1
    DataModelIntervalEnd <- length(data[,1])
    if((DataModelIntervalEnd - DataModelIntervalStart + 1) < K_minDataModelIntervalWidth){
      output$InputFileError <- renderText({msgDataFileTooSmall})
    } else {
      output$InputFileError <- renderText({""})
    }
    
    if((length(grep("FT",names(data)))>0) || (length(grep("IF",names(data)))>0)) {
      updateSelectInput(session, "dataPlotChoice",
                        choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "CF",
                                       "Failure Intensity" = "FI"), selected = "CF")
    } else if((length(grep("CFC",names(data)))>0) || (length(grep("FC",names(data)))>0)) {
      updateSelectInput(session, "dataPlotChoice",
                        choices = list("Failure Counts" = "FC", "Cumulative Failures" = "CF",
                                       "Failure Intensity" = "FI", "Times Between Failures" = "IF"), selected = "CF")
    }
    
    updateSliderInput(session, "modelDataRange",
                      min = DataModelIntervalStart, value = c(DataModelIntervalStart, DataModelIntervalEnd),
                      max = DataModelIntervalEnd)
    updateSliderInput(session, "parmEstIntvl",
                      min = DataModelIntervalStart, value = ceiling(DataModelIntervalStart + (DataModelIntervalEnd - DataModelIntervalStart - 1)/2),
                      max = DataModelIntervalEnd-1)
    
    data
  })
  
  # Draw the plot of input data or selected trend test
  
  output$distPlot <- renderPlot({ #reactive function, basically Main()
    
    data <- data.frame(x=data_global())
    DataColNames <- names(data)
    names(data) <- gsub("x.", "", DataColNames)
    if(length(names(data)) > 1) {
      Time <- names(data[1]) # generic name of column name of data frame (x-axis)
      Failure <- names(data[2]) # (y-axis)
      
      data_set <- input$dataSheetChoice
      if(input$PlotDataOrTrend == 1){
        
        # Plot the raw failure data
        
        q <- ggplot(,aes_string(x="Index",y="FailureDisplayType"))
        input_data <- data
        source("Plot_Raw_Data.R", local=TRUE)
      } else if (input$PlotDataOrTrend == 2) {
        
        # Plot the selected trend test
        
        q <- ggplot(,aes_string(x="index",y="trend_test_statistic"))
        input_data <- data
        source("Plot_Trend_Tests.R", local=TRUE)
      }
      
      q
      
      #plot(data) Leave this here to use if ggplot() stops working. 
    }
  })
  
  
  # Set up the data and trend test statistics tables for display
  
  FailureDataTable <- reactive ({
    tempDataMatrix <- matrix()
    if (!(is.null(input$file) && (input$type == 2)) || (!(is.null(input$dataSheetChoice)) && (input$type == 1))) {
      data <- data.frame(x=data_global())
      DataColNames <- names(data)
      names(data) <- gsub("x.", "", DataColNames)
      NameArray <- names(data)
      
      if(input$DataPlotAndTableTabset == "Data and Trend Test Table") {
        if(length(grep("IF",names(data))) || length(grep("FT",names(data)))) {
          FN <- data$FN
          if(input$PlotDataOrTrend == 1) {
            if(length(grep("IF", names(data)))){
              IF <- failureT_to_interF(data$FT)
              FT <- data$FT
            } else if(length(grep("FT", names(data)))) {
              FT <- interF_to_failureT(data$IF)
              IF <- data$IF
            }
            NameArray <- c("Failure Number", "Times Between Failures", "Failure Time")
            tempDataMatrix <- matrix(c(FN, IF, FT), ncol=3)
          } else if(input$PlotDataOrTrend == 2) {
            if(length(grep("IF", names(data)))){
              IF <- failureT_to_interF(data$FT)
            } else if(length(grep("FT", names(data)))) {
              IF <- data$IF
            }
            
            if (input$trendPlotChoice == "LP") {
              sol <- laplace_trend_test(IF)
              NameArray <- c("Failure Number", "Times Between Failures", "Laplace Test Statistic")
              tempDataMatrix <- matrix(c(FN, IF, sol$Laplace_factor), ncol=3)
            } else if (input$trendPlotChoice == "RA") {
              sol <- running_average_test(IF)
              NameArray <- c("Failure Number", "Times Between Failures", "Running Average IF Time")
              tempDataMatrix <- matrix(c(FN, IF, sol$Running_Average), ncol=3)
            }
          }
        } else if(length(grep("CFC",names(data))) || length(grep("FC",names(data)))) {
          if(input$PlotDataOrTrend == 1) {
            if(length(grep("CFC", names(data)))){
              FC <- CumulativeFailureC_to_failureC(data$CFC)
              CFC <- data$CFC
            } else if(length(grep("FC", names(data)))) {
              FC <- data$FC
              CFC <- FailureC_to_CumulativeFailureC(data$FC)
            }
            IntervalNum <- c(1:length(data$T))
            
            NameArray <- c("Test Interval", "Cumulative Test Time", "Failure Counts", "Cumulative Failure Count")
            tempDataMatrix <- matrix(c(IntervalNum, data$T, FC, CFC), ncol=4)
            
          } else if(input$PlotDataOrTrend == 2) {
            if(length(grep("CFC", names(data)))){
              FC <- CumulativeFailureC_to_failureC(data$CFC)
            } else if(length(grep("FC", names(data)))) {
              FC <- data$FC
            }
            
            FT <- failureC_to_failureT(data$T,FC)
            IF <- failureT_to_interF(failure_T = FT)
            FN <- c(1:length(FT))
            IntervalTime <- data$T
            
            if(input$trendPlotChoice == "LP") {
              sol <- laplace_trend_test(IF)
              NameArray <- c("Failure Number", "Times Between Failures", "Laplace Test Statistic")
              tempDataMatrix <- matrix(c(FN, IF, sol$Laplace_factor), ncol=3)
            } else if(input$trendPlotChoice == "RA") {
              sol <- running_average_test(IF)
              NameArray <- c("Failure Number", "Times Between Failures", "Running Average IF Time")
              tempDataMatrix <- matrix(c(FN, IF, sol$Running_Average), ncol=3)
            }
          }
        }
        colnames(tempDataMatrix) <- NameArray
      }
    }
    tempDataMatrix
  })
  
  # Display the input data or selected trend test in tabular form.
  
  output$dataAndTrendTable <- renderDataTable({
    OutputTable <- data.frame(x=FailureDataTable())
    if(length(OutputTable) > 1) {
      DataColNames <- names(OutputTable)
      names(OutputTable) <- gsub("x.", "", DataColNames)
    } else {
      OutputTable <- data.frame()
    }
    OutputTable
  })
  
  
  # Here we monitor the model configuration controls in the "Set Up and Apply Models"
  # tab.  We read the values from the controls, and adjust the controls to make
  # sure that the modeling intervals and lengths of the modeling data set don't
  # go below specified minimal values.
  
  output$ModelConfigError <- renderText({
    outputMessage <- ""
    
    # Read the slider for the categories to be retained when filtering the data.
    
    DataCategoryFirst <- input$sliderDataSubsetChoice[1]
    DataCategoryLast <- input$sliderDataSubsetChoice[2]
    
    # Set the slider for the initial parameter estimation range to be
    # consistent with the data range over which models are applied
    
    dataModelRange <- input$modelDataRange
    
    DataModelIntervalStart <- dataModelRange[1]
    DataModelIntervalEnd <- dataModelRange[2]
    
    # Keep the data interval used for modeling to 5 observations or more.
    
    if((DataModelIntervalEnd - DataModelIntervalStart + 1) < K_minDataModelIntervalWidth){
      outputMessage <- msgDataIntervalTooSmall
      while((DataModelIntervalEnd - DataModelIntervalStart + 1) < K_minDataModelIntervalWidth){
        if(DataModelIntervalStart > 1){
          DataModelIntervalStart <- DataModelIntervalStart - 1
        }
        if(DataModelIntervalEnd < length(data_global[,1])){
          DataModelIntervalEnd <- DataModelIntervalEnd + 1
        }
      }
      updateSliderInput(session, "modelDataRange", value = c(DataModelIntervalStart, DataModelIntervalEnd))
    }    
    updateSliderInput(session, "parmEstIntvl",
                      min = DataModelIntervalStart, value = ceiling(DataModelIntervalStart + (DataModelIntervalEnd - DataModelIntervalStart - 1)/2),
                      max = DataModelIntervalEnd-1)    
    
    outputMessage
  })
  
  
  
  
  output$ModelPlot <- renderPlot({
    data <- data.frame(x=data_global())
    DataColNames <- names(data)
    names(data) <- gsub("x.", "", DataColNames)
    data_set <- input$dataSheetChoice
    #data
    Time <- "Time"
    Failure <- "Failure"
    #Time <- names(data[1])#generic name of column name of data frame (x-axis)
    #Failure <- names(data[2])#(y-axis)
    #p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
    value <- c("blue","red")
    #p <- ggplot(,aes_string(x=Time,y=Failure))
    #mvf_plot_data <- data.frame()
    #p1 <- ggplot(,aes_string(x=Time,y=Failure));
    #q <- ggplot()
    #p1 <- ggplot()
    #p1 <- ggplot()
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
                p1 <- p1 + scale_color_manual(name = "JM",  labels = c("MVF","Original Data"),values = c("blue","red"))
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
                print(new_params)
                print(typeof(new_params))
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                if(typeof(new_params)=="double"){
                  print("I am 1")
                  
                  frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                  print(" I am 2")
                  mvf_plot_data <- JM_MVF(frame_params,data)
                  print("I am 3")
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
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                }
                else if(new_params=="nonconvergence"){

                  print("I am here so :")
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_point(data=original_data,aes(Time,Failure))
                  #c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
                  p1 + annotate("segment", x = 0, xend = length(original_data$Failure)/2, y = 0, yend = length(original_data$Time)/2,  colour = "red")
                  #p1 <- p1 + annotate("rect", xmin = length(original_data$Failure)/2 -50, xmax = length(original_data$Failure)/2 +50, ymin = length(original_data$Time)/2 -30, ymax = length(original_data$Time)/2 -30, alpha = .2)
                  p1 <- p1+ annotate("text", label = "Non-Convergence", x = length(original_data$Failure)/2, y = length(original_data$Time)/2, size = 8, colour = "red")
                }
                else{
                  print (" I am an else ")
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
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="lines and dots"))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="dots"))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure,color="lines"))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1<- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                p1 <- p1 + theme(legend.position = c(0.1, 0.9));
                p1 <- p1 + scale_color_manual(name = "Legend",  labels = c("GM_MVF","Original Data"),values = c("black","grey"))
                #q <- q + p
              }
              if(input$modelPlotChoice==1){

                p1 <- ggplot(,aes_string(x=Time,y=Failure));

                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_T(frame_params,data)
                
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_point(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("Time Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){

                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1+ geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1 + ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1 + ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1+ geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GM_BM_MLE(IF)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- GM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1 +ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }          
        }
        else if(i==7){
          print("Goel-okumoto");

          if(length(grep("IF",names(data)))){
              if(input$modelPlotChoice==2){
                FT <- interF_to_failureT(data$IF)
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(FT)
                print(new_params)
                data <- data.frame("FT"=FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="lines and dots"))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="dots"))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure,color="lines"))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1<- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                p1 <- p1 + theme(legend.position = c(0.1, 0.9));
                p1 <- p1 + scale_color_manual(name = "Legend",  labels = c("GO_MVF","Original Data"),values = c("black","grey"))
                #q <- q + p
              }
              if(input$modelPlotChoice==1){

                p1 <- ggplot(,aes_string(x=Time,y=Failure));

                new_params <- GO_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_T(frame_params,data)
                
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_point(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("Time Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){

                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1+ geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(data$IF)
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1 + ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(data$FT)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(data$FT)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(data$FT)
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              if(input$modelPlotChoice==2){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(FT)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1 + ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==1){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                mvf_plot_data <- data.frame("Time"=data$FT,"Failure"=data$IF)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time"=data$FN,"Failure" =data$IF)
                  p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
                }
                p1 <- p1+ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==3){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(FT)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_FR(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1+ geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
              if(input$modelPlotChoice==4){
                p1 <- ggplot(,aes_string(x=Time,y=Failure));
                new_params <- GO_BM_MLE(FT)
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("aMLE"=c(new_params[1]),"bMLE"=c(new_params[2]))
                mvf_plot_data <- GO_BM_R(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))#+ geomline(data=plot_data)
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
                }
                # if(input$checkboxDataOnPlot){
                #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                #   p <- p + geom_line(data=original_data,aes(Time,Failure));
                # }
                p1 <- p1 +ggtitle(paste(c("Reliability function plot of"),input$dataSheetChoice));
                #q <- q + p
              }
            }          
        
        }
        else{
          print("Other");
        }
        #print("hello i am out");
        
        #print("hello i am a out");
          
          
          #print("i ==6")
        }
      #print("model selections")
      p1
    }
    #print("i am far out");
    #print(input$runModels)
    #p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
    #print(input$modelResultChoice)
    
    }
    #plotties <- c(p1,p1)
    #print(plotties$layers)
    #plotSet <- length(plotties$layers)>0
    #print(length(plotSet))
     #p <- multiplot(p1,p1,cols=1)
    
    } ) 
  
  output$mytable1 <- renderDataTable({

    inFile <- input$file
    table_t <- data.frame()
    

    if(is.null(inFile)){
      return("Please upload an a file")
    }

    data <- data_react()
    
    # frame_params <- "EMPTY"
    #  frame_params <- data.frame("N0"=c(0.001),"Phi"=c(9.8832))
    frame_params <- data.frame()
    #if(input$runModels!=0){          ###################should think of isolate here
      plus <- 0

      ###################################################
      if(!is.numeric(input$modelDetailPredTime)){
        return(data)
      }
      if(!is.numeric(input$modelDetailPredFailures)){
        return(data)
      }
      ###################################################

      if(length(input$modelDetailChoice)>0){
        count <- 0
        for(i in input$modelDetailChoice){
          if(i=="Jelinski-Moranda"){
            if(length(grep("IF",names(data)))){
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(data$IF)
              print(new_params)
              if(typeof(new_params)=="double"){
                print("Entered the double")
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredTime,length(data$IF))
                time_fails  <- get_prediction_n(frame_params,input$modelDetailPredFailures,length(data$IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
              }
              else if(new_params=="nonconvergence"){
                print("Entered the Non-conv")
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                
              }
              else{
                # to be programmed
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredTime,length(IF))
                time_fails  <- get_prediction_n(frame_params,input$modelDetailPredFailures,length(IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
                
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
              
              }
              else{
                # to be programmed
              }

              
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredTime,length(IF))
                time_fails  <- get_prediction_n(frame_params,input$modelDetailPredFailures,length(IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
              
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                
              }
              else{
                # to be programmed
              }  
            }

          }

          else if(i=="Geometric"){
            if(length(grep("IF",names(data)))){
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- GM_BM_MLE(data$IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
               number_fails  <- get_prediction_t(frame_params,input$modelDetailPredTime,length(data$IF))
                time_fails  <- get_prediction_n(frame_params,input$modelDetailPredFailures,length(data$IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
                #t
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                #t
              }
              else{
                # to be programmed
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- GM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredTime,length(IF))
                time_fails  <- get_prediction_n(frame_params,input$modelDetailPredFailures,length(IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                #t
              }
              else{
                # to be programmed
              }
              
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- GM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredTime,length(IF))
                time_fails  <- get_prediction_n(frame_params,input$modelDetailPredFailures,length(IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
                #t
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                #t
              }
              else{
                # to be programmed
              }  
            }
          }
          else{
            count <- count + 1
            table_t[count,1] <- i
            table_t[count,2] <- "Given Model not defined"
            table_t[count,3] <- "Given Model not defined"

          }

      }
      table_t <- data.frame(table_t[1],table_t[2],table_t[3])
      names(table_t) <- c("Model",paste("Expected # of failure for next", input$modelDetailPredTime ,"time units"), paste("Expected time for next", input$modelDetailPredFailures ,"failures"))
    }
    #table_t <- data.frame(table_t[1],table_t[2],table_t[3])
    #names(table_t) <- c("Model","N0","Time-remaining")
    table_t
  #data_global
  })
  
output$mytable2 <- renderDataTable({
    source("GOF.R")
    inFile <- input$file
    table_t <- data.frame("Model"=NULL, "N0"=NULL, "Time-remaining"=NULL)
    

    if(is.null(inFile)){
      return("Please upload an a file")
    }

    data <- data_react()
     if(!is.numeric(input$modelDetailPredTime)){
        return(data)
      }
      if(!is.numeric(input$modelDetailPredFailures)){
        return(data)
      }
    # frame_params <- "EMPTY"
    #  frame_params <- data.frame("N0"=c(0.001),"Phi"=c(9.8832))
    frame_params <- data.frame()
    #if(input$runModels!=0){          ###################should think of isolate here
      plus <- 0
      if(length(input$modelEvalChoice)>0){
        count <- 0
        for(i in input$modelEvalChoice){
          if(i=="Jelinski-Moranda"){
            if(length(grep("IF",names(data)))){
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(data$IF)
              print(new_params)
              if(typeof(new_params)=="double"){
                print("Entered the double")
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(data$IF))

                table_t[count,1] <- i
                table_t[count,2] <- length(data$IF)
                table_t[count,3] <- number_fails
                
              }
              else if(new_params=="nonconvergence"){
                print("Entered the Non-conv")
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"             
              }
              else{
                # to be programmed
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,lenght(IF))

                table_t[count,1] <- i
                table_t[count,2] <- length(IF)
                table_t[count,3] <- number_fails
                
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
              
              }
              else{
                # to be programmed
              }

              
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(IF))

                table_t[count,1] <- i
                table_t[count,2] <-length(IF)
                table_t[count,3] <- number_fails
              
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                
              }
              else{
                # to be programmed
              }  
            }

          }

          else if(i=="Geometric"){
            if(length(grep("IF",names(data)))){
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- GM_BM_MLE(data$IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(data$IF))

                table_t[count,1] <- i
                table_t[count,2] <- length(data$IF)
                table_t[count,3] <- number_fails
                #t
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                #t
              }
              else{
                # to be programmed
              }
            }
            else if(length(grep("FT",names(data)))){
              IF <- failureT_to_interF(data$FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- GM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=data$FT,"IF"=IF,"FN"=1:length(data$FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails_t  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(IF))

                table_t[count,1] <- i
                table_t[count,2] <- length(IF)
                table_t[count,3] <- number_fails_t
                #t
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                #t
              }
              else{
                # to be programmed
              }
              
            }
            else if(length(grep("CFC",names(data)))){

              FC <- CumulativeFailureC_to_failureC(data$CFC)
              FT <- failureC_to_failureT(data$T,FC)
              IF <- failureT_to_interF(failure_T = FT)
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- GM_BM_MLE(IF)
              if(typeof(new_params)=="double"){
                data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
                frame_params <- data.frame("D0"=c(new_params[1]),"Phi"=c(new_params[2]))
                number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(IF))

                table_t[count,1] <- i
                table_t[count,2] <- length(IF)
                table_t[count,3] <- number_fails
                #t
              }
              else if(new_params=="nonconvergence"){
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                #t
              }
              else{
                # to be programmed
              }  
            }
          }
          else{
            count <- count + 1
            table_t[count,1] <- i
            table_t[count,2] <- "Given Model not defined"
            table_t[count,3] <- "Given Model not defined"

          }

      }
      table_t <- data.frame(table_t[1],table_t[2],table_t[3])
      names(table_t) <- c("Model","Present # of faults",paste("Expected # of faults after", input$modelDetailPredFailures, "from now"))
    }
    #table_t <- data.frame(table_t[1],table_t[2],table_t[3])
    #names(table_t) <- c("Model","N0","Time-remaining")
    table_t
  #data_global
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
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


custom_tabPanel <- function(){
  br <- tags$br()
  divtag <- tags$div(class='tab-pane',title=title,'data-value'=value, 'data-icon-class' = iconClass(icon),...)

  r <- paste(br,divtag,sep="")
  r

}
library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
#library(DT)
source("utility.R")
source("Model_specifications.R")
source("custom_functions.R")
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("JM_BM.R")
source("GO_BM_FT.R")
source("GM_BM.R")
source("DSS_BM_FT.R")
source("Wei_NM_FT.R")
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

shinyServer(function(input, output, clientData, session) {#reactive shiny function
  
  output$sheetChoice <- renderUI({ # ------ > Should fix empty data_set name for .csv files
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
      data_original <<- data # ----? should think of its usage 'data_original'
      data_set <- inFile$filename
    }
      #data
      print(data)
      if(dataType(names(data))=="FR"){
        data_generated <- generate_dataFrame(data)
        print(data_generated)
        data_generated
      }
      else if(dataType(names(data))=="FC"){
        data_intermediate <<- generate_dataFrame(data)
        data_generated <- data_intermediate$FRate
      }
      data_generated
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
  
  # There is a serious flaw in tracking the models selected
  # But there is a strong necessity to track the models 
  # selected.


  # track_models <- reactive({
  #   tracked_models <- c()
  #   if(!is.null(input$modelResultChoice)) {
  #     tracked_models <- input$modelResultChoice
  #   }
  #   else{
  #     if(!is.null(input$modelDetailChoice)){
  #       tracked_models <- input$modelDetailChoice
  #     }
      
  #   }
  #   print(tracked_models)
  #   tracked_models

  #   # Returns indeces of the models selected
  #   # The indices should be same throughout the
  # })
  
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







# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   PLOTS CONSTRUCT    ----------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

plot_construct <- function(model,data){
  
  # ----> ! print(dataType(names(data)))
  if(dataType(names(data))=="FR"){

    model_params <- get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(get(paste("data"))[[get(paste(model,"input",sep="_"))]])
    # assign(paste(input$modelPlotChoice,"plot_data",sep="_"),get(paste(model,input$modelPlotChoice,sep="_"))(model_params,data))

    # MLE_construct <- get(paste(model,get(paste(model,method,sep="_"))[1],c("MLE"),sep="_"))
    # model_params <- MLE_construct(data[[model_method[1]]]) # ---- > Should be from model specifications 'JM_input'
    # print(model_params)
    # model_params <- JM_BM_MLE(data$IF)


  if(input$modelPlotChoice=="MVF"){
     
    
     
    print(model_params)
    if(typeof(model_params)!="character"){
      MVF_construct <- get(paste(model,input$modelPlotChoice,sep="_"))
      mvf_plot_data <- MVF_construct(model_params,data)
      if(input$ModelDataPlotType=="points_and_lines"){
        p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color=Model))+ geom_line(data=mvf_plot_data, aes(Time,Failure,color=Model,linetype=Model))
      }
      if(input$ModelDataPlotType=="points"){
        p1 <- p1 + geom_point(data = mvf_plot_data, aes(Time,Failure, color=Model))
      }
      if(input$ModelDataPlotType=="lines"){
        p1 <- p1 + geom_line(data = mvf_plot_data, aes(Time, Failure, color=Model))
      }
      if(input$checkboxDataOnPlot){
        original_data <- data.frame("Time" = data$FT, "Failure" = data$FN)
        p1 <- p1 + geom_step(data = original_data,aes( Time, Failure),color='gray')
      }
      p1 <- p1 + ggtitle(paste(c("Mean Value function plot of"), input$dataSheetChoice))#+ geomline(data=plot_data)
      #p1 <- p1 + theme(legend.position = c(0.1, 0.9));
      #p1 <- p1 + scale_color_manual(name = "JM", labels = c("MVF","Original Data"),values = c("blue","red"))
      #q <- q + p
    }
    else if(model_params=="nonconvergence"){
      original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
      p1 <- p1 + geom_point(data=original_data,aes(Time,Failure))
      #p1 + annotate("segment", x = 0, xend = length(original_data$Failure)/2, y = 0, yend = length(original_data$Time)/2,  colour = "red")
      p1 <- p1+ annotate("text", label = "Non-Convergence", x = length(original_data$Failure)/2, y = length(original_data$Time)/2, size = 8, colour = "red")
    }
}

  if(input$modelPlotChoice=="MTTF"){
    #assign(paste(input$modelPlotChoice,"construct",sep="_"), get(paste(i,input$modelPlotChoice,sep="_")))
    assign(paste(input$modelPlotChoice,"plot_data",sep="_"),get(paste(model,input$modelPlotChoice,sep="_"))(model_params,data))
    

    if(input$ModelDataPlotType=="points_and_lines"){
      p1 <- p1 + geom_point(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Failure_Number,MTTF,color=Model))+ geom_line(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")), aes(Failure_Number,MTTF,color=Model))
    }
    if(input$ModelDataPlotType=="points"){
      p1 <- p1 + geom_point(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Failure_Number,MTTF,color=Model))
    }
    if(input$ModelDataPlotType=="lines"){
      p1 <- p1 + geom_line(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Failure_Number,MTTF,color=Model))
    }
    if(input$checkboxDataOnPlot){
      original_data <- data.frame("Failure_Number"=data$FN,"MTTF"=data$IF)
      print(original_data)
      p1 <- p1 + geom_step(data=original_data,aes(Failure_Number,MTTF))
    }

    
    p1 <- p1 + ggtitle(paste(c("TIme Between Failure function plot of"),input$dataSheetChoice))
    #q <- q + p
  }

  if(input$modelPlotChoice=="FI"){
    #assign(paste(input$modelPlotChoice,"construct",sep="_"), get(paste(i,input$modelPlotChoice,sep="_")))
    assign(paste(input$modelPlotChoice,"plot_data",sep="_"),get(paste(model,input$modelPlotChoice,sep="_"))(model_params,data))

    # -----> assign('x',5) Good example to create dynamic variables
    if(input$ModelDataPlotType=="points_and_lines"){
      p1 <- p1 + geom_point(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Failure_Count,Failure_Rate,color=Model))+ geom_line(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")), aes(Failure_Count,Failure_Rate,color=Model)) # ----? can we use "Failure Count" without underscore
    }
    if(input$ModelDataPlotType=="points"){
      p1 <- p1 + geom_point(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Failure_Count,Failure_Rate,color=Model))
    }
    if(input$ModelDataPlotType=="lines"){
      p1 <- p1 + geom_line(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Failure_Count, Failure_Rate,color=Model))

    }
    # if(input$checkboxDataOnPlot){
    #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
    #   p <- p + geom_line(data=original_data,aes(Time,Failure))
    # }
    if(is.null(input$dataSheetChoice)){
      p1 <- p1+ggtitle("Failure Intensity function plot")
    }
    else{
       p1 <- p1+ggtitle(paste(c("Failure Intensity function plot ["),input$dataSheetChoice,"]"))
    }
    p1
  }
  if(input$modelPlotChoice=="R"){
    #R_construct <- get(paste(i,input$modelPlotChoice,sep="_"))
    assign(paste(input$modelPlotChoice,"plot_data",sep="_"),get(paste(model,input$modelPlotChoice,sep="_"))(model_params,data))

    if(input$ModelDataPlotType=="points_and_lines"){
      p1 <- p1 + geom_point(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Time,Reliability,color=Model))+ geom_line(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")), aes(Time,Reliability))# + ggtitle(paste(c("Laplace trend of "),data_set))
    }
    if(input$ModelDataPlotType=="points"){
      p1 <- p1 + geom_point(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Time,Reliability,color=Model))
    }
    if(input$ModelDataPlotType=="lines"){
      p1 <- p1 + geom_line(data=get(paste(input$modelPlotChoice,"plot_data",sep="_")),aes(Time, Reliability,color=Model))

    }
    # if(input$checkboxDataOnPlot){
    #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
    #   p <- p + geom_line(data=original_data,aes(Time,Failure))
    # }
    if(is.null(input$dataSheetChoice)){
      p1 <- p1+ggtitle("Reliabililty function plot")
    }
    else{
       p1 <- p1+ggtitle(paste(c("Reliabililty function plot ["),input$dataSheetChoice,"]"))
    }
  }
  if(input$modelPlotChoice=="R_growth"){

  }
  }

  else if(dataType(names(data))=="FC"){
  # To be programmed


  }
  p1          
 

}
  

# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------- Display the input data or selected trend test in tabular form  ---------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

  
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



# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   Model Plot   ----------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------




  output$ModelPlot <- renderPlot({

    data <- data_global()
    data_set <- input$dataSheetChoice  
    if(is.null(input$modelResultChoice) || (length(input$modelResultChoice)==0)){
      return
    }
  
  # ----------------------------------- model run starts here -----------------------------
      
    if(input$runModels!=0){          
    # -----> should think of isolate here
    # -----> should think of not rerunning models

      if(length(input$modelResultChoice)>0){

        p1 <<- ggplot()
        # Plot initializations above
        
          for(i in input$modelResultChoice){
            p1 <<-  plot_construct(i,data)
          }
        p1
      }
     }
    }) 

# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   TAB2 Table  ----------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

  
  output$mytable1 <- renderDataTable({

    inFile <- input$file
    table_t <- data.frame()
    

    if(is.null(inFile)){
      return("Please upload an a file")
    }

    data <- data_global()
    if(is.null(input$modelDetailChoice)){
        return
      }
    
      ###################################################
      if(!is.numeric(input$modelDetailPredTime)){
        return(data)
      }
      if(!is.numeric(input$modelDetailPredFailures)){
        return(data)
      }
      ###################################################
      #input$modelDetailChoice <- track_models()
      if(length(input$modelDetailChoice)>0){
        source("Detailed_prediction.R")
        model_params <- JM_BM_MLE(data$IF)
      #if(length(track_models())>0) {
        count <- 0
        for(i in input$modelDetailChoice){
          if(i=="JM"){
            if(dataType(names(data))=="FR"){
              count <- count + 1              
              
              print(model_params)
              if(typeof(model_params)!="character"){
                print("Entered the double")
                number_fails  <- get_prediction_t(model_params,input$modelDetailPredTime,length(data$IF))
                time_fails  <- get_prediction_n(model_params,input$modelDetailPredFailures,length(data$IF))
                table_t[count,1] <- i
                table_t[count,2] <- number_fails
                table_t[count,3] <- time_fails
              }
              else if(model_params=="nonconvergence"){
                print("Entered the Non-conv")
                table_t[count,1] <- i
                table_t[count,2] <- "NON-CONV"
                table_t[count,3] <- "NON-CONV"
                
              }
              else{
                # -------> to be programmed
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
    #}
    #table_t <- data.frame(table_t[1],table_t[2],table_t[3])
    #names(table_t) <- c("Model","N0","Time-remaining")
    table_t
  }
  #data_global
  })

tracked_models <- reactive({
  input$modelDetailChoice
})
  
output$mytable2 <- renderDataTable({
    source("GOF.R")
    inFile <- input$file
    table_t <- data.frame("Model"=NULL, "N0"=NULL, "Time-remaining"=NULL)
    

    if(is.null(inFile)){
      return("Please upload an a file")
    }

    if(is.null(tracked_models())){
        return
      }
      print(tracked_models())
    data <- data_global()
     # if(!is.numeric(input$modelDetailPredTime)){
     #    return(data)
     #  }
      # if(!is.numeric(input$modelDetailPredFailures)){
      #   return(data)
      # }
    # frame_params <- "EMPTY"
    #  frame_params <- data.frame("N0"=c(0.001),"Phi"=c(9.8832))
    frame_params <- data.frame()
    #if(input$runModels!=0){          ###################should think of isolate here
      plus <- 0
      if(length(tracked_models)>0){
        count <- 0
        for(i in tracked_models()){
          if(i=="Jelinski-Moranda"){
            if(length(grep("IF",names(data)))){
              count <- count + 1
              source("Detailed_prediction.R")
              new_params <- JM_BM_MLE(data$IF)
              print(new_params)
              if(typeof(new_params)=="double"){
                print("Entered the double")
                data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
                frame_params  <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                Max_lnl           <- JM_BM_lnl(data$IF,frame_params$N0,frame_params$Phi)
                PSSE          <- psse_times(data, frame_params)
                print(Max_lnl)
                AIC           <- aic(2,Max_lnl)
                #print(AIC)
                #PSSE          <- psse()
                #number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(data$IF))

                table_t[count,1] <- i
                table_t[count,2] <- AIC
                table_t[count,3] <- PSSE          
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
                Max_lnl           <- JM_BM_lnl(data$IF,frame_params$N0,frame_params$Phi)
                AIC           <- aic(2,Max_lnl)
                #PSSE          <- psse()
                #number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(data$IF))

                table_t[count,1] <- i
                table_t[count,2] <- AIC
                table_t[count,3] <- "PSSE" 
                
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
                Max_lnl           <- JM_BM_lnl(data$IF,frame_params$N0,frame_params$Phi)
                PSSE          <- psse_times(data, frame_params)
                print(Max_lnl)
                AIC           <- aic(2,Max_lnl)
                #print(AIC)
                #PSSE          <- psse()
                #number_fails  <- get_prediction_t(frame_params,input$modelDetailPredFailures,length(data$IF))

                table_t[count,1] <- i
                table_t[count,2] <- AIC
                table_t[count,3] <- PSSE
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
      print(table_t)
      names(table_t) <- c("Model","AIC","PSSE")
    }
    #table_t <- data.frame(table_t[1],table_t[2],table_t[3])
    #names(table_t) <- c("Model","N0","Time-remaining")
    table_t
  #data_global
  })

})


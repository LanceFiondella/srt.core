DataIntervalStart <- input$modelDataRange[1]
DataIntervalEnd <- input$modelDataRange[2]
InitialParmEndObs <- input$parmEstIntvl

# These two lists are used to keep track of models
# that executed successfully and those that did not.

ModelsExecutedList <<- list()
ModelsFailedExecutionList <<- list()


if((DataIntervalEnd - DataIntervalStart + 1) >= K_minDataModelIntervalWidth) {
  # The first order of business is to run all of the models.
  
  ModelResults <- reactive({
    RunModels <- input$runModels  # Wait for the run models button to be pressed.
    
    # ResultsList is the list that will hold the model results.
    # Each model's results will be a data frame containing
    # the model's estimates and predictions as well as the
    # parameter values.  If a model doesn't converge, the
    # value NaN will be entered into its results frame
    
    ResultsList <- list()
    tempResultsFrame <- data.frame()
    
    # The results list will also hold the subsetted data on
    # which the models are run.
    
    input_data <- data.frame(x=data_global())
    names(input_data) <- gsub("x.", "", names(input_data))
    
    EmptyDataEntries <- rep(NA, input$modelNumPredSteps)
    
    # These are the data sets that will be input directly into the models.
    
    if((length(grep("FT",names(input_data)))>0) || (length(grep("IF",names(input_data)))>0)) {
      IF <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = IF)), use.names=FALSE)
      FT <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FT)), use.names=FALSE)
      FN <- c(unlist(subset(subset(input_data, input_data$FN >= DataIntervalStart, select = c(FN, IF, FT)), FN <= DataIntervalEnd, select = FN)), use.names=FALSE)
      tempResultsFrame <- data.frame("FN"=c(FN, EmptyDataEntries), "IF"=c(IF, EmptyDataEntries), "FT"=c(FT, EmptyDataEntries))
      ResultsList[["Data"]] <- tempResultsFrame
      
      ModelsToRunList <- K_IF_ModelsList
    } else if((length(grep("CFC",names(input_data)))>0) || (length(grep("FC",names(input_data)))>0)) {
      FC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = FC)), use.names=FALSE)
      CFC <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = CFC)), use.names=FALSE)
      CumT <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = T)), use.names=FALSE)
      TI <- c(unlist(subset(subset(input_data, input_data$TI >= DataIntervalStart, select = c(TI, T, FC, CFC)), TI <= DataIntervalEnd, select = TI)), use.names=FALSE)

      # Do in-place conversion of FC to IF data - for the time being we'll run the IF/FT models on the converted data.
      # Later, as models are added, we can add models that use the FC data explicitly.
      
      FN <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FN)), use.names=FALSE)
      IF <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_IF)), use.names=FALSE)
      FT <- c(unlist(subset(subset(FC_to_IF_data, FC_to_IF_data$FC_TI >= DataIntervalStart, select = c(FC_FN, FC_TI, FC_IF, FC_FT)), FC_TI <= DataIntervalEnd, select = FC_FT)), use.names=FALSE)
      
      tempResultsFrame <- data.frame("FN"=c(FN, EmptyDataEntries), "IF"=c(IF, EmptyDataEntries), "FT"=c(FT, EmptyDataEntries))
      ResultsList[["Data"]] <- tempResultsFrame
      
      ModelsToRunList <- K_FC_ModelsList
    }
    
    # Now run all of the models for the current data type and put the results
    # the list of results.
    
    for(ModelName in 1:length(ModelsToRunList)) {
      for (index in InitialParmEndObs:length(ResultsList$Data[,1]-length(EmptyDataEntries))) {
        
      }
    }
    
    # Clean up
    
    tempResultsFrame <- data.frame()
    
    # Now present the model results for use by
    # model display and evaluation functionality.
    
    ResultsList
  })
  
}
  
  

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
  #tracked_models <- track_models()
  #input$modelResultChoice <- track_models()
  if(input$runModels!=0){          ###################should think of isolate here
    plus <- 0
    if(is.null(input$modelResultChoice)){
      return
    }
    if(length(track_models())>0){
      
      for(i in input$modelResultChoice){
        if(i=="Jelinski-Moranda"){
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
        
        else if(i=="Geometric"){
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
        else if(i=="Goel-okumoto"){
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
  
}) 
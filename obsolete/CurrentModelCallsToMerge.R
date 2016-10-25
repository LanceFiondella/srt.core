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
  
  count <-0
  # while(is.null(input$modelResultChoice) || length(input$modelResultChoice)==0 ){
  #   count <- count+1
  #   #print(count)
  #   return
  # }
  
  
  if(is.null(input$modelResultChoice) || (length(input$modelResultChoice)==0)) {
    return
  }
  
  
  if(input$runModels!=0){          ###################should think of isolate here
    plus <- 0
    
    
    if(length(input$modelResultChoice)>0){
      
      
      
      # Plot initializations
      
      
      p1 <- ggplot(,aes_string(x=Time,y=Failure));
      
      # Plot initializations above
      
      for(i in input$modelResultChoice){
        if(i=="Jelinski-Moranda"){
          
          if(length(grep("IF",names(data)))){
            
            
            # if(input$modelPlotChoice==2){
            #  #p1 <- ggplot(,aes_string(x=Time,y=Failure));
            #  new_params <- JM_BM_MLE(IF)
            #  #print(new_params)
            #  #print(typeof(new_params))
            #  data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
            #  if(typeof(new_params)=="double"){
            #    ##print("I am 1")
            
            #    frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
            #    ##print(" I am 2")
            #    mvf_plot_data <- JM_MVF(frame_params,data)
            #    ##print("I am 3")
            #    if(input$ModelDataPlotType==1){
            #      p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
            #    }
            #    if(input$ModelDataPlotType==2){
            #      p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure))
            #    }
            #    if(input$ModelDataPlotType==3){
            #      p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time,Failure))
            
            #    } 
            #    if(input$checkboxDataOnPlot){
            #    original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
            #    p1 <- p1 + geom_line(data=original_data,aes(Time,Failure));
            #  }
            #  }
            #  else if(new_params=="nonconvergence"){
            
            #    ##print("I am here so :")
            #    original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
            #    p1 <- p1 + geom_point(data=original_data,aes(Time,Failure))
            #    #c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
            #    p1 + annotate("segment", x = 0, xend = length(original_data$Failure)/2, y = 0, yend = length(original_data$Time)/2,  colour = "red")
            #    #p1 <- p1 + annotate("rect", xmin = length(original_data$Failure)/2 -50, xmax = length(original_data$Failure)/2 +50, ymin = length(original_data$Time)/2 -30, ymax = length(original_data$Time)/2 -30, alpha = .2)
            #    p1 <- p1+ annotate("text", label = "Non-Convergence", x = length(original_data$Failure)/2, y = length(original_data$Time)/2, size = 8, colour = "red")
            #  }
            #  else{
            #    #print (" I am an else ")
            #  }
            
            #  p1 <- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))  
            
            
            
            
            
            
            
            if(input$modelPlotChoice==2){
              
              #p1 <- ggplot(,ae_string(x=Time,y=Failure));
              new_params <- JM_BM_MLE(data$IF)
              data <- data.frame("FT"=data$FT,"IF"=data$IF,"FN"=1:length(data$FT))
              if(typeof(new_params)=="double"){
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                mvf_plot_data <- JM_MVF(frame_params,data)
                if(input$ModelDataPlotType==1){
                  p1 <- p1 + geom_point(data=mvf_plot_data,aes(Time,Failure,color="lines and dots"))+ geom_line(data=mvf_plot_data)# + ggtitle(paste(c("Laplace trend of "),data_set))
                }
                if(input$ModelDataPlotType==2){
                  p1 <- p1 + geom_point(data = mvf_plot_data, aes(Time,Failure, color="dots"))
                }
                if(input$ModelDataPlotType==3){
                  p1 <- p1 + geom_line(data = mvf_plot_data, aes(Time, Failure, color="lines"))
                }
                if(input$checkboxDataOnPlot){
                  original_data <- data.frame("Time" = data$FT, "Failure" = data$FN)
                  p1 <- p1 + geom_line(data = original_data,aes( Time, Failure, color = "Original Data"))
                }
                p1 <- p1 + ggtitle(paste(c("Mean Value function plot of"), input$dataSheetChoice))#+ geomline(data=plot_data)
                p1 <- p1 + theme(legend.position = c(0.1, 0.9));
                p1 <- p1 + scale_color_manual(name = "JM", labels = c("MVF","Original Data"),values = c("blue","red"))
                #q <- q + p
              }
              else if(new_params=="nonconvergence"){
                original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                p1 <- p1 + geom_point(data=original_data,aes(Time,Failure))
                #p1 + annotate("segment", x = 0, xend = length(original_data$Failure)/2, y = 0, yend = length(original_data$Time)/2,  colour = "red")
                p1 <- p1+ annotate("text", label = "Non-Convergence", x = length(original_data$Failure)/2, y = length(original_data$Time)/2, size = 8, colour = "red")
              }
            }
            
            if(input$modelPlotChoice==1){
              
              #p1 <- ggplot(,aes_string(x=X_label,y=Y_label));
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
              X_label <- c("Time")
              Y_label <- c("Failure")
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
                p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time, Failure))
                
              }
              # if(input$checkboxDataOnPlot){
              #   original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
              #   p <- p + geom_line(data=original_data,aes(Time,Failure))
              # }
              p1 <- p1+ggtitle(paste(c("Failure Intensity function plot of"),input$dataSheetChoice))
              #q <- q + p
              
            }
            if(input$modelPlotChoice==4){
              X_label <- c("Time")
              Y_label <- c("Failure")
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
                p1 <- p1 + geom_line(data=mvf_plot_data,aes(Time, Failure))
                
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
              new_params <- JM_BM_MLE(IF)
              #print(new_params)
              #print(typeof(new_params))
              data <- data.frame("FT"=FT,"IF"=IF,"FN"=1:length(FT))
              if(typeof(new_params)=="double"){
                ##print("I am 1")
                
                frame_params <- data.frame("N0"=c(new_params[1]),"Phi"=c(new_params[2]))
                ##print(" I am 2")
                mvf_plot_data <- JM_MVF(frame_params,data)
                ##print("I am 3")
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
                
                ##print("I am here so :")
                original_data <- data.frame("Time"=data$FT,"Failure" =data$FN)
                p1 <- p1 + geom_point(data=original_data,aes(Time,Failure))
                #c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
                p1 + annotate("segment", x = 0, xend = length(original_data$Failure)/2, y = 0, yend = length(original_data$Time)/2,  colour = "red")
                #p1 <- p1 + annotate("rect", xmin = length(original_data$Failure)/2 -50, xmax = length(original_data$Failure)/2 +50, ymin = length(original_data$Time)/2 -30, ymax = length(original_data$Time)/2 -30, alpha = .2)
                p1 <- p1+ annotate("text", label = "Non-Convergence", x = length(original_data$Failure)/2, y = length(original_data$Time)/2, size = 8, colour = "red")
              }
              else{
                #print (" I am an else ")
              }
              
              p1 <- p1+ggtitle(paste(c("Mean Value function plot of"),input$dataSheetChoice))  
              #q <- q + p              
            }
            if(input$modelPlotChoice==1){
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
            #p1 <- ggplot(,aes_string(x=Time,y=Failure));
            if(input$modelPlotChoice==2){
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
              
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
              
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
          #print("Goel-okumoto");
          #p1 <- ggplot(,aes_string(x=Time,y=Failure))
          if(length(grep("IF",names(data)))){
            if(input$modelPlotChoice==2){
              FT <- interF_to_failureT(data$IF)
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
              new_params <- GO_BM_MLE(FT)
              #print(new_params)
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
              
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
              
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
              
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
              #p1 <- ggplot(,aes_string(x=Time,y=Failure));
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
          #print("Other");
        }
        ##print("hello i am out");
        
        ##print("hello i am a out");
        
        
        ##print("i ==6")
      }
      p1
      
      ##print("model selections")
      
    }
    
    
  }
  
  
  
  
  ##print("i am far out");
  ##print(input$runModels)
  #p <- p + scale_color_manual(name = "Legend",  labels = c("Original Data"),values = c("blue"))
  ##print(input$modelResultChoice)
  
  
  #plotties <- c(p1,p1)
  ##print(plotties$layers)
  #plotSet <- length(plotties$layers)>0
  ##print(length(plotSet))
  #p <- multiplot(p1,p1,cols=1)
  
} ) 
# Plot model results (and raw data, if specified)

plot_model_prediction_results <- function(modelchoicelist,plotType,data,C1,C2,C3,lifecycle) {
  #print(data)
  localResultsPlot <- ggplot()
  n <- length(data[[1]]$FT)
  #Change to T
  tn <- 2*data[[1]]$FT[n]
  t_opt <- 0
  predictionPlotdataframe <- data.frame("time"=double(),"cost"=double())
  names(predictionPlotdataframe) <- c("time","cost")
  # print(modelchoicelist)
  # if(is.null(modelchoicelist)){
  print("___printing model choice list")
  print(modelchoicelist)
  for (model in modelchoicelist){
    #model_params <- try(get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(get(paste("data"))[[get(paste(model,"input",sep="_"))]]),silent=TRUE)
    model_params <- try(get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(data[[1]][[get(paste(model,"input",sep="_"))[1]]]),silent=TRUE)
    print(model_params)
    print(c(C1,C2,C3))
    print(paste(model,"OR_CC",sep="_"))
    #t_opt_model <- get(paste(model,"OR_CC", sep="_"))(model_params,C1,C2,C3)
    #t_opt <- max(t_opt_model, t_opt)
    print("________printing optimal t*")  
    # prediction <- data.frame(c(1:(2*t_opt)), get(paste(model,"cost",sep="_"))(model_params,C1,C2,C3,c(1:(2*t_opt)),t_opt_model),rep(model))
    prediction <- data.frame(seq(1,tn,length.out=1000), get(paste(model,"cost",sep="_"))(model_params,C1,C2,C3,seq(1,tn,length.out=1000),lifecycle),rep(model))
    names(prediction ) <- c("time","cost","Model")
    predictionPlotdataframe <- rbind(predictionPlotdataframe, prediction )  
  }
    
  #print(prediction)
  if(plotType == "points_and_lines"){
      localResultsPlot <- localResultsPlot + geom_point(data=predictionPlotdataframe,aes(time,cost,color=Model)) + geom_step(data=predictionPlotdataframe,aes(time,cost,color=Model))
    }
    else if (plotType == "points"){
      localResultsPlot <- localResultsPlot + geom_point(data=predictionPlotdataframe,aes(time,cost,color=Model)) 
    }
    else {
      localResultsPlot <- localResultsPlot + geom_step(data=predictionPlotdataframe,aes(time,cost,color=Model)) 
    }

    localResultsPlot <- localResultsPlot + theme(legend.position = "bottom", text = element_text(size=14)) + xlab("Time")+ylab("Cost")
  return(localResultsPlot)
}


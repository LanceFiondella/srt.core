library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
source("GO_BM.R")
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({ #reactive function, basically Main()

  inFile <- input$file #Read of input file
    if (is.null(inFile))#error handling for null file pointer
      return("Please Upload a CSV File")
    if (input$type==1)
      data <- read.xls(inFile$datapath,sheet=1)#Reads xls and xlsx files. Error handling needed
    if (input$type==2)
      data <- read.csv(inFile$datapath, header = input$header, sep = input$sep , quote = " % ")#same as before needs error handling

  #if (data[1] =="FC")
    #two coumns
  #else
    #one column

   Time <- names(data[1])#generic name of column name of data frame (x-axis)
   Failure <- names(data[2])#(y-axis)
   p <- ggplot(,aes_string(x=Time,y=Failure))#This function needs aes_string() to work
   value <- c("red","blue") 
   model <- ""
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
      newdata <- GO_BM_MLE(data)
      print(newdata)
      p <- p + geom_point(data=newdata,aes(color="red",group="Geol-Okumoto Model"))
      p <- p + geom_line(data=newdata,aes(color="red",group="Geol-Okumoto Model"))
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


library(shiny)#I wonder why this is here?
library(gdata) #Used for read.xls function
library(ggplot2)#ggplot function

#library(DT)
#<<<<<<< HEAD
#
#=======
sys.source("utility.R")
sys.source("Model_specifications.R")
#>>>>>>> 96a7378e8c6ea79df90ed837bcb95531d9c2d251
sys.source("custom_functions.R")
source("model.R")#Source for our reliabilty models
source("JMmodel.R")
sys.source("JM_BM.R")
sys.source("GO_BM_FT.R")
sys.source("GM_BM.R")
sys.source("DSS_BM_FT.R")
source("Wei_NM_FT.R")
source("Data_Format.R")
source("Laplace_trend_test.R")
source("DataAndTrendTables.R")
source("RA_Test.R")
source("RunModels.R")
source("PlotModelResults.R")
source("ModelResultTable.R")
source("ErrorMessages.R")  # Text for error messages

# Initialize global variables -------------------------------

openFileDatapath <- ""
#data_global <- data.frame()
data_set_global <- ""
data_set_global_type <- ""
FC_to_IF_data <- data.frame()

DataModelIntervalStart <<- 1
DataModelIntervalEnd <<- 5

# These two data frames hold model results as
# well as the data to which models were applied.

ModelResults <- data.frame()
ModeledData <- data.frame()

# These two vectors identify the models that
# ran successfully and those that did not.

SuccessfulModels <- c()
FailedModels <- c()

# These two lists are used to keep track of models
# that executed successfully and those that did not.

#ModelsExecutedList <- list()
#ModelsFailedExecutionList <- list()

# This is a list that hold the list of model evaluations.
# Each set of model evaluations is a data frame - there's a
# separate data frame for each model evaluation that's done.

ModelEvalsList <- list()

# Initialize "constants" ------------------------------------

K_minDataModelIntervalWidth <- 5

K_CategoryFirst <- 1
K_CategoryLast <- 5

# These lists identify the models used for each data type

K_IF_ModelsList <- list("Delayed S-Shaped"="DSS", "Geometric Model"="GM", "Goel-Okumoto"="GO", "Jelinski-Moranda"="JM", "Weibull"="Wei")
K_FC_ModelsList <- list("Delayed S-Shaped"="DSS", "Geometric Model"="GM", "Goel-Okumoto"="GO", "Jelinski-Moranda"="JM", "Weibull"="Wei")

# Colors that will be used in plotting model results

K_ModelResultColors <- list("JM"="red", "GM"="blue", "GO"="green", "DSS"="yellow", "Wei"="orange")

# Tolerance used in determining whether a value is a whole number.

K_tol <- .Machine$double.eps^0.5

# Start main program ------------------------------------

openFileDatapath <- ""
#data_global <- data.frame()
data_original <- data.frame()

shinyServer(function(input, output, clientData, session) {#reactive shiny function
  
  #source("utility.R")
  
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
        #print(inFile)
        return("Please upload excel sheet")
      }
      #print(inFile)
      data <- read.csv(inFile$datapath, head = TRUE, sep = ',', quote = " % ")#same as before needs error handling
      data_original <<- data # ----? should think of its usage 'data_original'
      data_set <- inFile$filename
    }
    data_set_global <<- data_set
    #data
    #print(data)
    if(dataType(names(data))=="FR"){
      data_generated <- generate_dataFrame(data)
      #print(data_generated)
      data_generated
    }
    else if(dataType(names(data))=="FC"){
      data_intermediate <<- generate_dataFrame(data)
      data_generated <- data_intermediate$FRate
    }
    
    # Set up the initial values for modeling data range and the initial parameter
    # estimation range
    
    # Set up the initial values for modeling data range and the initial parameter
    # estimation range
    
    DataModelIntervalStart <<- 1
    DataModelIntervalEnd <<- length(data_generated[,1])
    if((DataModelIntervalEnd - DataModelIntervalStart + 1) < K_minDataModelIntervalWidth){
      output$InputFileError <- renderText({msgDataFileTooSmall})
    } else {
      output$InputFileError <- renderText({""})
    }
    
    # Complete all columns for FT/IF data, including failure number.
    # This information will be used later for subsetting the data.
    
    if(dataType(names(data))=="FR") {
      data_set_global_type <<- "IFTimes"

      # Update the selection list for the models that can be run.
      
      updateSelectInput(session, "modelsToRun", choices = K_IF_ModelsList, selected = K_IF_ModelsList)
      
      # Update failure data view choices for IF/FT data and model result views.
      
      updateSelectInput(session, "dataPlotChoice",
                        choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "CF",
                                       "Failure Intensity" = "FI"), selected = "CF")
      updateSelectInput(session, "modelPlotChoice",
                        choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "MVF",
                                       "Failure Intensity" = "FI", "Reliability" = "REL"), selected = "MVF")
      
    } else if(dataType(names(data))=="FC") {
      data_set_global_type <<- "FailureCounts"

      # Add a column for test intervals.
      
      data_generated$TI <- c(1:length(data$FC))
      
      FC_to_IF_data <<- FCFrame_to_IFFrame(data$T, data$FC)
      
      # Update the selection list for the models that can be run.
      
      updateSelectInput(session, "modelsToRun", choices = K_FC_ModelsList, selected = K_FC_ModelsList)
      
      # Update failure data view choices for CFC/FC data/model views.
      # Includes a "failure counts" view which IF/FT data does not.
      
      updateSelectInput(session, "dataPlotChoice",
                        choices = list("Failure Counts" = "FC", "Cumulative Failures" = "CF",
                                       "Failure Intensity" = "FI", "Times Between Failures" = "IF"), selected = "CF")
      updateSelectInput(session, "modelPlotChoice",
                        choices = list("Failure Counts" = "FC", "Cumulative Failures" = "MVF",
                                       "Failure Intensity" = "FI", "Times Between Failures" = "IF", "Reliability" = "REL"), selected = "MVF")
      
    }
    
    updateSliderInput(session, "modelDataRange",
                      min = DataModelIntervalStart, value = c(DataModelIntervalStart, DataModelIntervalEnd),
                      max = DataModelIntervalEnd)
    updateSliderInput(session, "parmEstIntvl",
                      min = DataModelIntervalStart, value = ceiling(DataModelIntervalStart + (DataModelIntervalEnd - DataModelIntervalStart - 1)/2),
                      max = DataModelIntervalEnd-1)
    
    
    # Finally, output data set
    
    data_generated
}) 

  # A reactive data item that is used to control the height of the raw data and trend
  # plot.  The height is computed based on the width - it the plot is not as high
  # as it is wide, and if the width exceeds a minimum, then the height catches up with
  # the width to make a square plot.
  
  DTP_height <- reactive({
    Width <- session$clientData$output_DataAndTrendPlot_width
    Height <- session$clientData$output_DataAndTrendPlot_height
    if((Width > Height) && (Width > 400)) {
      Height <- Width
    }
    Height
  })
  
  # Read the position of the mouse for the data and trend plot
  
  DTPranges <- reactiveValues(x = NULL, y = NULL)
  
  # Event observer for double-click on data and trend plot.
  # Double click and brush zooms in and out.
  
  observeEvent(input$DTPdblclick, {
    DTPbrush <- input$DTP_brush
    if (!is.null(DTPbrush)) {
      DTPranges$x <- c(DTPbrush$xmin, DTPbrush$xmax)
      DTPranges$y <- c(DTPbrush$ymin, DTPbrush$ymax)
      
    } else {
      DTPranges$x <- NULL
      DTPranges$y <- NULL
    }
  })

  # A reactive data item that is used to control the height of the model results
  # plot.  The height is computed based on the width - it the plot is not as high
  # as it is wide, and if the width exceeds a minimum, then the height catches up with
  # the width to make a square plot.

  MP_height <- reactive({
    Width <- session$clientData$output_ModelPlot_width
    Height <- session$clientData$output_ModelPlot_height
    if((Width > Height) && (Width > 400)) {
      Height <- Width
    }
    Height
  })

  # Read the position of the mouse for the model results plot
  
  MPranges <- reactiveValues(x = NULL, y = NULL)
  
  # Event observer for double-click on model results plot.
  # Double click and brush zooms in and out.
  
  observeEvent(input$MPdblclick, {
    MPbrush <- input$MP_brush
    if (!is.null(MPbrush)) {
      MPranges$x <- c(MPbrush$xmin, MPbrush$xmax)
      MPranges$y <- c(MPbrush$ymin, MPbrush$ymax)
      
    } else {
      MPranges$x <- NULL
      MPranges$y <- NULL
    }
  })
  

  # Draw the plot of input data or selected trend test
  
  output$DataAndTrendPlot <- renderPlot({ #reactive function, basically Main()
    
    DataAndTrendPlot <- NULL   # Set the plot object to NULL to prevent error messages.
    data <- data.frame(x=data_global())
    DataColNames <- names(data)
    names(data) <- gsub("x.", "", DataColNames)
    if(length(names(data)) > 1) {
      Time <- names(data[1]) # generic name of column name of data frame (x-axis)
      Failure <- names(data[2]) # (y-axis)
      
      data_set <- input$dataSheetChoice
      if(input$PlotDataOrTrend == 1){
        
        # Plot the raw failure data
        
        input_data <- data
        source("Plot_Raw_Data.R", local=TRUE)
      } else if (input$PlotDataOrTrend == 2) {
        
        # Plot the selected trend test
        
        input_data <- data
        source("Plot_Trend_Tests.R", local=TRUE)
      }
      
      DataAndTrendPlot <- DataAndTrendPlot + coord_cartesian(xlim = DTPranges$x, ylim = DTPranges$y)
      DataAndTrendPlot
      
      #plot(data) Leave this here to use if ggplot() stops working. 
    }
  }, height=DTP_height)
  
  
  # Download handler for saving data and trend plots or tables.
  
  output$saveDataOrTrend <- downloadHandler(
    filename = function() {
      if(input$PlotDataOrTrend == 1) {
        paste(paste0(data_set_global, "_Data_", input$dataPlotChoice), input$saveDataFileType, sep=".")
      } else if(input$PlotDataOrTrend == 2) {
        paste(paste0(data_set_global, "_Trend_", input$trendPlotChoice), input$saveDataFileType, sep=".")
      }
    },
    content = function(filespec) {
      ggsave(filespec)
    }
  )
  
  
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
    DataTrendTable <- NULL
    if (!(is.null(input$file) && (input$type == 2)) || (!(is.null(input$dataSheetChoice)) && (input$type == 1))) {
      if (input$DataPlotAndTableTabset == "Data and Trend Test Table") {
        data <- data.frame(x=data_global())
        DataTrendTable <- data_or_trend_table(data, input$modelDataRange, input$PlotDataOrTrend, input$trendPlotChoice)
      }
    }
    DataTrendTable
  })


  # Here we monitor the data subset and model configuration controls in the
  # "Select, Analyze, and Subset Failure Data" and "Set Up and Apply Models"
  # tabs.  We read the values from the controls, and adjust the controls to make
  # sure that the modeling intervals and lengths of the modeling data set don't
  # go below specified minimal values.
    
  output$DataSubsetError <- renderText({
    data_local <- data.frame(x=data_global())
    DataColNames <- names(data_local)
    names(data_local) <- gsub("x.", "", DataColNames)
    
    outputMessage <- ""
    
    # Read the slider for the categories to be retained when filtering the data.
    
    # DataCategoryFirst <- input$sliderDataSubsetChoice[1]
    # DataCategoryLast <- input$sliderDataSubsetChoice[2]
    
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
        if(DataModelIntervalEnd < length(data_local[,1])){
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

  
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------   Run Models    -------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  
  
  # Run the models for the data type of the input file.
  
  observeEvent(input$runModels, {
    if(((input$modelDataRange[2] - input$modelDataRange[1] + 1) >= K_minDataModelIntervalWidth) && (length(as.list(input$modelsToRun)) > 0)) {
      
      # Create temporary storage to hold model results and related modeling values.
      tempResultsList <- list()
      
      updateSelectInput(session, "modelResultChoice", choices=list("No model results to display"="None"), selected="None")
      updateSelectInput(session, "modelDetailChoice", choices=list("No model results to display"="None"), selected="None")
      updateSelectInput(session, "modelResultsForEval", choices=list("No model results to display"="None"), selected="None")
      updateSelectInput(session, "AllModelsRun", choices=list("No model results to display"="None"), selected="None")
      
      # Subset the data according to the range we've specified.
      
      ModeledData <<- tail(head(data_global(), input$modelDataRange[2]), (input$modelDataRange[2]-input$modelDataRange[1]+1))
      
      if(input$modelDataRange[1] == 1) {
        TimeOffset <- 0
      } else {
        TimeOffset <- tail(head(data_global(), input$modelDataRange[1]-1), 1)[["FT"]]
      }
      tempResultsList <- run_models(ModeledData, input$modelDataRange, input$parmEstIntvl, TimeOffset, input$modelNumPredSteps, input$modelsToRun, K_tol)
      ModelResults <<- tempResultsList[["Results"]]
      SuccessfulModels <<- tempResultsList[["SuccessfulModels"]]
      FailedModels <<- tempResultsList[["FailedModels"]]
      
      # Update the model results selection pull-downs with the names of the
      # models that have been successfully run.
      
      ModelsToShow <- as.list(SuccessfulModels)
      ModelsToShowNames <- c()
      for (ModelsToShowIndex in 1:length(ModelsToShow)) {
        ModelsToShowNames <- c(ModelsToShowNames, get(paste(SuccessfulModels[ModelsToShowIndex], "fullname", sep="_"))) 
      }
      names(ModelsToShow) <- ModelsToShowNames
      
      updateSelectInput(session, "modelResultChoice", choices = ModelsToShow, selected=ModelsToShow[1])
      updateSelectInput(session, "modelDetailChoice", choices = ModelsToShow, selected=ModelsToShow[1])
      updateSelectInput(session, "modelResultsForEval", choices = ModelsToShow, selected=ModelsToShow[1])
      
      AllModelsRunNames <- c()
      AllModelsRun <- sort(c(SuccessfulModels, FailedModels))
      for (ModelsToShowIndex in 1:length(AllModelsRun)) {
        AllModelsRunNames <- c(AllModelsRunNames, get(paste(AllModelsRun[ModelsToShowIndex], "fullname", sep="_"))) 
      }
      names(AllModelsRun) <- AllModelsRunNames
      
      updateSelectInput(session, "AllModelsRun", choices = AllModelsRun, selected=AllModelsRun[1])
      
      # Release temporary storage of model results
      tempResultsList <- list()
    }
  })
  
  


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
  }, options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))


  
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# --------------------------- Display selected model results in tabular form  --------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
  
    output$ModelResultTable <- renderDataTable({
      MR_Table <- NULL
      if(!is.null(ModelResults)) {
        if(length(input$AllModelsRun) > 0) {
          
          # User has selected at one model to display as a table.
          
          MR_Table <- model_result_table(ModelResults, input$AllModelsRun)
        }
      }
      if (length(MR_Table) <= 1) {
        MR_Table <- data.frame()
      }
      MR_Table
      #MR_Table[,1:(length(names(MR_Table)))]
    }, options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  

# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   Model Plot   ----------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


  output$ModelPlot <- renderPlot({
    MRPlot <- NULL
    if((length(SuccessfulModels) > 0) && (!is.null(ModelResults)) && (!is.null(ModeledData))) {
      MRPlot <- plot_model_results(ModelResults, ModeledData, data_set_global, input$modelResultChoice, input$modelPlotChoice, input$ModelDataPlotType, input$checkboxDataOnPlot)
      if(!is.null(MRPlot)) {
        MRPlot <- MRPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
      }
    }
    MRPlot
  }, height=MP_height) 

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


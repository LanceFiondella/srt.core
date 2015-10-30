library(shiny)
library(DT)
library(gdata) 
library(ggplot2)
library(knitr)

# Contributors guide step - 1
# source models here. Put models in the models/'model_name' folder
sys.source("models/Model_specifications.R")
sys.source("models/JM/JM_BM.R")
sys.source("models/GO/GO_BM_FT.R")
sys.source("models/GM/GM_BM.R")
sys.source("models/DSS/DSS_BM_FT.R")
sys.source("models/Wei/Wei_NM_FT.R")

# Trend tests utility functions
source("trend_tests/RA_Test.R")
source("trend_tests/Laplace_trend_test.R")

# Plots utility function
source("utility/plots/Plot_Raw_Data.R")
source("utility/plots/Plot_Trend_Tests.R")
source("utility/plots/PlotModelResults.R")

# Data utility functions
sys.source("utility/data/Data_Tools.R")

# Tables utility functions
source("utility/tables/DataAndTrendTables.R")
source("utility/tables/ModelResultTable.R")

# Other utilities
source("utility/RunModels.R")      # Models run flow
source("utility/ErrorMessages.R")  # Text for error messages

openFileDatapath <- ""
#data_global <- data.frame()
data_set_global <- ""
data_set_global_type <- ""
FC_to_IF_data <- data.frame()

DataModelIntervalStart <- 1
DataModelIntervalEnd <- 5

# These two data frames hold model results as
# well as the data to which models were applied.

ModelResults <- data.frame()
ModeledData <- data.frame()
ModeledDataName <- ""

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

K_IF_ModelsList <- list("Delayed S-Shaped"="DSS", "Geometric"="GM", "Goel-Okumoto"="GO", "Jelinski-Moranda"="JM", "Weibull"="Wei")
K_FC_ModelsList <- list("Delayed S-Shaped"="DSS", "Geometric"="GM", "Goel-Okumoto"="GO", "Jelinski-Moranda"="JM", "Weibull"="Wei")

# Colors that will be used in plotting model results

K_ModelResultColors <- list("JM"="red", "GM"="blue", "GO"="green", "DSS"="yellow", "Wei"="orange")

# Tolerance used in determining whether a value is a whole number.

K_tol <- .Machine$double.eps^0.5

# Start main program ------------------------------------

openFileDatapath <- ""
# data_global <- data.frame()
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

  # output$message <- renderUI({
  #    sliderInput('test', 'test_label', 0, 5, 3, step = 1, round = FALSE,  ticks = TRUE, animate = TRUE, width = NULL)
  #    animationOptions(interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL)
  #    p("HEllO")
  # })

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
    
    if(dataType(names(data_generated))=="FR") {
      data_set_global_type <<- "IFTimes"

      # Update the selection list for the models that can be run.
      
      updateSelectInput(session, "modelsToRun", choices = K_IF_ModelsList, selected = K_IF_ModelsList)
      
      # Update failure data view choices for IF/FT data and model result views.
      
      updateSelectInput(session, "dataPlotChoice",
                        choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "CF",
                                       "Failure Intensity" = "FI"), selected = "CF")
      # updateSelectInput(session, "modelPlotChoice",
      #                  choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "MVF",
      #                                 "Failure Intensity" = "FI", "Reliability" = "R","Reliability Growth"="R_growth"), selected = "MVF")
      updateSelectInput(session, "modelPlotChoice",
                        choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "MVF",
                                       "Failure Intensity" = "FI", "Reliability Growth"="R_growth"), selected = "MVF")
      

      # Update the default mission time for computing reliability
      # on both Tab 2 and Tab 3.  Also update the default time on
      # Tab 3 for which we want to know how many failures we'll
      # observe in the future.  We choose the most recent IF time
      # that is greater than 0.
      
      for (dataIndex in length(data_generated$IF):1) {
        if(data_generated$IF[dataIndex] > 0) {break}
      }
      updateSliderInput(session, "modelRelMissionTime",
                        min=0, value=data_generated$IF[length(data_generated$IF)])
      updateSliderInput(session, "modelDetailPredTime",
                        min=0, value=data_generated$IF[length(data_generated$IF)])
      updateSliderInput(session, "modelRelMissionTime2",
                        min=0, value=data_generated$IF[length(data_generated$IF)])

    } else if(dataType(names(data_generated))=="FC") {
      data_set_global_type <<- "FailureCounts"

      # Add a column for test intervals.
      
      data$TI <- c(1:length(data$FC))
      
      FC_to_IF_data <<- FCFrame_to_IFFrame(data$T, data$FC)
      
      # Update the selection list for the models that can be run.
      
      updateSelectInput(session, "modelsToRun", choices = K_FC_ModelsList, selected = K_FC_ModelsList)
      
      # Update failure data view choices for CFC/FC data/model views.
      # Includes a "failure counts" view which IF/FT data does not.
      
      updateSelectInput(session, "dataPlotChoice",
                        choices = list("Failure Counts" = "FC", "Cumulative Failures" = "CF",
                                       "Failure Intensity" = "FI", "Times Between Failures" = "IF"), selected = "CF")
      # updateSelectInput(session, "modelPlotChoice",
      #                   choices = list("Failure Counts" = "FC", "Cumulative Failures" = "MVF",
      #                                  "Failure Intensity" = "FI", "Times Between Failures" = "IF", "Reliability" = "R","Reliability Growth"="R_growth"), selected = "MVF")
      updateSelectInput(session, "modelPlotChoice",
                        choices = list("Failure Counts" = "FC", "Cumulative Failures" = "MVF",
                                       "Failure Intensity" = "FI", "Times Between Failures" = "IF", "Reliability Growth"="R_growth"), selected = "MVF")
      
      
    }
    
    updateSliderInput(session, "modelDataRange",
                      min = DataModelIntervalStart, value = c(DataModelIntervalStart, DataModelIntervalEnd),
                      max = DataModelIntervalEnd)

    # Finally, output data set
    
    data_generated
}) 

  # This slider that controls the end of the initial parameter estimation interval
  # is dynamically created to ensure that its value is always in sync with those of
  # the start and end points of the current data range.
  
#  output$ParameterInterval <- renderUI({
#    intervalStart <- input$modelDataRange[1]
#    intervalEnd <- input$modelDataRange[2]
#    initParmIntervalEnd <- ceiling(intervalStart + (intervalEnd - intervalStart - 1)/2)
#    sliderInput("parmEstIntvl", h6("Specify the last data point for the initial parameter estimation interval."),
#                min=intervalStart, max=intervalEnd-1, value=initParmIntervalEnd, step=1)
#  })

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
  
  
  LPTestStatistic <- reactive({
    if(input$trendPlotChoice=="LP") {
      testStat <- qnorm(1-input$confidenceLP)
    } else {
      testStat <- 0
    }
    testStat
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
        
        DataAndTrendPlot <- plot_failure_data(data, FC_to_IF_data, data_set, input$modelDataRange, input$dataPlotChoice, input$DataPlotType, K_minDataModelIntervalWidth)
      } else if (input$PlotDataOrTrend == 2) {
        
        # Plot the selected trend test
        
        DataAndTrendPlot <- plot_trend_tests(data, FC_to_IF_data, data_set, input$modelDataRange, input$trendPlotChoice, input$confidenceLP, LPTestStatistic(), input$DataPlotType, K_minDataModelIntervalWidth)
      }
      
      DataAndTrendPlot <- DataAndTrendPlot + coord_cartesian(xlim = DTPranges$x, ylim = DTPranges$y)
      DataAndTrendPlot
      
      #plot(data) Leave this here to use if ggplot() stops working. 
    }
  }, height=DTP_height)
  
  
  # Download handler for saving data and trend plots or tables.
  
  output$saveDataOrTrend <- downloadHandler(
    filename = function() {
      if(input$DataPlotAndTableTabset == "Plot") {
        if(input$PlotDataOrTrend == 1) {
          paste(paste0(data_set_global, "_Data_", input$dataPlotChoice), input$saveDataFileType, sep=".")
        } else if(input$PlotDataOrTrend == 2) {
          paste(paste0(data_set_global, "_Trend_", input$trendPlotChoice), input$saveDataFileType, sep=".")
        }
      } else { # Save data table
        if(input$PlotDataOrTrend == 1) {
          paste(paste0(data_set_global, "_Data"), "csv", sep=".")
        } else if(input$PlotDataOrTrend == 2) {
          paste(paste0(data_set_global, "_Trend_", input$trendPlotChoice), "csv", sep=".")
        }
      }
    },
    content = function(filespec) {
      if(input$DataPlotAndTableTabset == "Plot") {
        ggsave(filespec)
      } else {
        OutputTable <- data.frame(x=FailureDataTable())
        if(length(OutputTable) > 1) {
          DataColNames <- names(OutputTable)
          names(OutputTable) <- gsub("x.", "", DataColNames)
        } else {
          OutputTable <- data.frame()
        }
        utils::write.csv(OutputTable, file=filespec)
      }
    }
  )

    
  # Download handler for saving model result plots or tables.
  
  output$saveModelResults <- downloadHandler(
    filename = function() {
      if(input$ModelPlotAndTableTabset == "Model Result Plot") {
        
        # Save model results plot
        
        paste(paste0(ModeledDataName, "_Results_", input$modelPlotChoice), input$saveModelResultsType, sep=".")
      } else {
        
        # Save model results table
        
        paste(paste0(ModeledDataName, "_Results"), "csv", sep=".")
      }
    },
    content = function(filespec) {
      if(input$ModelPlotAndTableTabset == "Model Result Plot") {
        ggsave(filespec)
      } else {
        OutputTable <- ModelResults
        
        # For the time being, we're dropping the column that would
        # reliability compoutations.  We still keep reliability growth.
        
        TableNames <- names(OutputTable)
        ColsToDrop <- c()
        for (colIndex in 1:length(TableNames)) {
          if(length(grep("_Rel", TableNames[colIndex])) > 0) {
            ColsToDrop <- c(ColsToDrop, TableNames[colIndex])
          }
        }
        OutputTable <- OutputTable[,!(names(OutputTable) %in% ColsToDrop)]
        
        # Turn OutputTable to character representations to avoid
        # difficulties with NA, Inf, and NaN.
        
        TableNames <- names(OutputTable)
        for (nameIndex in TableNames) {
          OutputTable[[nameIndex]] <- as.character(OutputTable[[nameIndex]])
        }
        
        if(length(OutputTable) > 1) {
        } else {
          OutputTable <- data.frame()
        }
        utils::write.csv(OutputTable, file=filespec, quote=TRUE, na="NA")
      }
    }
  )
  

  
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
    
    DataModelIntervalStart <<- dataModelRange[1]
    DataModelIntervalEnd <<- dataModelRange[2]
    
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
    # updateSliderInput(session, "parmEstIntvl",
    #                  min = DataModelIntervalStart, value = ceiling(DataModelIntervalStart + (DataModelIntervalEnd - DataModelIntervalStart - 1)/2),
    #                  max = DataModelIntervalEnd-1)    
    
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
      ModeledDataName <<- data_set_global
      
      if(input$modelDataRange[1] == 1) {
        TimeOffset <- 0
      } else {
        TimeOffset <- tail(head(data_global(), input$modelDataRange[1]-1), 1)[["FT"]]
      }
      # tempResultsList <- run_models(ModeledData, input$modelDataRange, input$parmEstIntvl, TimeOffset, input$modelNumPredSteps, input$modelsToRun, input$modelRelMissionTime, K_tol)
      tempResultsList <- run_models(ModeledData, input$modelDataRange, length(ModeledData[,1]), TimeOffset, input$modelNumPredSteps, input$modelsToRun, input$modelRelMissionTime, K_tol)
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

  
  # If one or more of the models didn't complete successfully, display a message
  # notifying the user of that fact.
  
#  UnsuccessfulModelsMessage <- reactive({
#    outputMessage <- ""
#    if((length(input$modelsToRun) > 0) && (input$ModelsToRun[1] != "None") && (length(FailedModels) > 0)) {
#      outputMessage <- paste0(msgUnsuccessfulModels, get(paste0(FailedModels[1], "_fullname")))
#      if (length(FailedModels) > 1) {
#        for (FailedModelsIndex in 2:length(FailedModels)) {
#          outputMessage <- paste0(outputMessage, paste0(", ", get(paste0(FailedModels[FailedModelsIndex], "_fullname"))))
#        }
#      }
#    }
#    outputMessage
#  })
  
#  output$UnsuccessfulModels <- renderText({
#    UnsuccessfulModelsMessage
#  })
 
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------- Display the input data or selected trend test in tabular form  ---------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

  
  output$dataAndTrendTable <- DT::renderDataTable({
    OutputTable <- data.frame(x=FailureDataTable())
    if(length(OutputTable) > 1) {
      DataColNames <- names(OutputTable)
      names(OutputTable) <- gsub("x.", "", DataColNames)
    } else {
      OutputTable <- data.frame()
    }
    OutputTable
  }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))


  
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# --------------------------- Display selected model results in tabular form  --------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
  
    output$ModelResultTable <- DT::renderDataTable({
      MR_Table <- NULL

      # Check if modelResultChoice is None and return NULL if true
      if(length(input$modelResultChoice)==0){
        return(MR_Table)
      }
      if(input$modelResultChoice=="None"){
        return(MR_Table)
      }
      print(ModelResults)
      if(is.null(ModelResults)){
        print("NO results to display.")
        return
      }
      else if(!is.null(ModelResults)) {
        if(length(input$AllModelsRun) > 0) {
          
          # User has selected at one model to display as a table.
          
          MR_Table <- model_result_table(ModelResults, length(ModeledData[,1]), input$AllModelsRun, input$modelRelMissionTime)
        }
      }
      if (length(MR_Table) <= 1) {
        MR_Table <- data.frame()
      } else {
        # Set column names for the model results table
        
        MR_Table_Names <- c("Failure")
        for (modelName in input$AllModelsRun) {
          for (modelParmNum in 1:length(get(paste0(modelName, "_params")))) {
            MR_Table_Names <- c(MR_Table_Names, paste(modelName, get(paste0(modelName, "_params"))[modelParmNum], sep="_"))
          }
          MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Cum_Time"))
          MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Cum_Fails"))
          MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_IF_Times"))
          MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Fail_Intensity"))
          # MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Reliability"))
          MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Rel_Growth"))
          names(MR_Table) <- MR_Table_Names
        }
      }
      MR_Table
    }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))
  

# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   Model Plot   ----------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


  output$ModelPlot <- renderPlot({
    MRPlot <- NULL
    if((length(input$modelResultChoice) > 0) && (input$modelResultChoice[1] != "None") && (!is.null(ModelResults)) && (!is.null(ModeledData))) {
      MRPlot <- plot_model_results(ModelResults, ModeledData, ModeledDataName, input$modelResultChoice, input$modelPlotChoice, input$ModelDataPlotType, input$checkboxDataOnPlot, input$checkboxDataEndOnPlot, input$modelRelMissionTime)
      if(!is.null(MRPlot)) {
        MRPlot <- MRPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
      }
    }
    MRPlot
  }, height=MP_height)


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   TAB3 Table   ----------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


tab3_table1_construct <- function(model,data,input){
  if(dataType(names(data))=="FR"){
    model_params <- try(get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(get(paste("data"))[[get(paste(model,"input",sep="_"))]]),silent=TRUE)
    # ----> ! print("Table1 construct: ")
    # ----> ! print(model_params)
    # ----> ! print(data)
    # ----> ! print(count)
    print("==============")
    print(model_params)
    #print()
    if(typeof(model_params)!="character"){
      number_fails <- get_prediction_k( model,
                                        model_params, 
                                        input$modelDetailPredTime, 
                                        data$FT[length(get("data")[[get(paste(model,"input",sep="_"))]])],
                                        length(get("data")[[get(paste(model,"input",sep="_"))]]))
      
      time_fails <- get_prediction_t( model,
                                      model_params, 
                                      input$modelDetailPredFailures,
                                      data$FT[length(get("data")[[get(paste(model,"input",sep="_"))]])],
                                      length(get("data")[[get(paste(model,"input",sep="_"))]]))
      rel_time <- get_reliability_t(model,
                                    model_params, 
                                    input$modelTargetReliability, input$modelRelMissionTime2, 
                                    data$FT[length(get("data")[[get(paste(model,"input",sep="_"))]])],
                                    length(get("data")[[get(paste(model,"input",sep="_"))]]))
      
      print(time_fails)
      print(number_fails)
      print(rel_time)
      ExpectedNumFailuresExceeded <- FALSE
      for( i in 1:length(time_fails)){
        if(!ExpectedNumFailuresExceeded){
          count <<- count+1
          tab3_table1[count,1]<<- get(paste0(model, "_fullname"))
          if(i == 1) {
            tab3_table1[count,2]<<- as.character(rel_time)
            tab3_table1[count,3]<<- number_fails
          } else {
            tab3_table1[count,2]<<- " "
            tab3_table1[count,3]<<- " "
          }
          tab3_table1[count,4]<<- i
          tab3_table1[count,5]<<- time_fails[i]

          #  Create Row of NA only once logic
          if(time_fails[i]=="NA"){
             ExpectedNumFailuresExceeded <- TRUE
             break
           }
         }
      }
    }
    else if(typeof(model_params)=="character"){
      if(length(grep("not found",model_params))){
        count<<-count+1
        tab3_table1[count,1] <<- model
        tab3_table1[count,2] <<- "Given-model not defined"
        tab3_table1[count,3] <<- "Given-model not defined"
        tab3_table1[count,4] <<- "Given-model not defined"
        tab3_table1[count,5] <<- "Given-model not defined"
      }
      else{
        count<<-count+1
        tab3_table1[count,1] <<- model
        tab3_table1[count,2] <<- "NON-CONV"
        tab3_table1[count,3] <<- "NON-CONV"
        tab3_table1[count,4] <<- "NON-CONV"
        tab3_table1[count,5] <<- "NON-CONV"
      }
    }
  }
  else{
    # ----> FC data should be handled here
  }
}

output$downloadData <- downloadHandler(
    filename <- function() {
      if (input$saveModelDetailsType == "PDF") {
        paste(paste0(ModeledDataName, "_Model_Queries"), "pdf", sep=".")
      } else {
        paste(paste0(ModeledDataName, "_Model_Queries"), "csv", sep=".")
      }
    },
    content <- function(filename) {
      tab3_table1_2_save <<- subset(tab3_table1, tab3_table1$Model != "<NA>")

      if (input$saveModelDetailsType == "PDF") {
        names(tab3_table1_2_save) <- c("Model", paste0("Time to R=", as.character(input$modelTargetReliability)), paste("Num failures in", as.character(input$modelDetailPredTime)), paste0("Failure"), paste0("Times to failures"))
        out_put = knit2pdf('Tab3ReportTemplate.Rnw', clean = TRUE)
        file.rename(out_put, filename) # move pdf to file for downloading
      } else {
        write.csv(tab3_table1_2_save, filename)
      }
    }
)
  
output$mytable1 <- DT::renderDataTable({

    inFile <- input$file
    table_t <- data.frame()
    

    if(is.null(inFile)){
      return("Please upload a file")
    }

    # Use the subset of data to which models were applied
    # to do the model evaluation.
    
    in_data_tab3 <- ModeledData
    timeOffset <- ModeledData$FT[1] - ModeledData$IF[1]
    in_data_tab3$FT <- in_data_tab3$FT - timeOffset
    
    ModelsToQuery <- input$modelDetailChoice
    if(length(ModelsToQuery)<=0) {
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
      if(length(ModelsToQuery)>0){
        source("utility/prediction/Detailed_prediction.R")

        count <<- 0
        tab3_table1<<- data.frame()
        for(i in ModelsToQuery){
          count <<- count+1
          tab3_table1_construct(i,in_data_tab3,input)
        }
      tab3_table1 <<- data.frame(tab3_table1[1],tab3_table1[2],tab3_table1[3], tab3_table1[4], tab3_table1[5])
      names(tab3_table1) <<- c("Model",paste("Time to achieve R =", as.character(input$modelTargetReliability), "for mission of length", as.character(input$modelRelMissionTime2)) ,paste("Expected # of failures for next", as.character(input$modelDetailPredTime) ,"time units"), paste0("Nth failure"), paste("Expected times to next", as.character(input$modelDetailPredFailures),"failures"))
    tab3_table1
  }
}, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))

tracked_models <- reactive({
  input$modelDetailChoice
})



# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ----------------------------------------   TAB4 Table   ----------------------------------------------
# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------


tab4_table1_construct <- function(model,data,input){
  if(dataType(names(data))=="FR"){
    model_params <- try(get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(get(paste("data"))[[get(paste(model,"input",sep="_"))]]),silent=TRUE)

    # ----> ! print("Table1 construct: ")
    print(model_params)
    # ----> ! print(data)
    # ----> ! print(count)
    if(typeof(model_params)!="character"){
      # number_fails <- get_prediction_n(model_params,input$modelDetailPredTime,length(get("data")[[get(paste(model,"input",sep="_"))]]))
      max_lnL <- try(get(paste(model,"lnL",sep="_"))(get("data")[[get(paste(model,"input",sep="_"))]],model_params),silent=TRUE)
      # time_fails <- get_prediction_t(model_params, input$modelDetailPredFailures, length(get("data")[[get(paste(model,"input",sep="_"))]]))
      #----> !  print(time_fails)
      #----> !  print(number_fails)
      print("Log LIkehood -----------------------------------------")
      print(max_lnL)
      if(length(grep("not found",max_lnL))) {
        count<<-count+1
        tab4_table1[count,1] <<- get(paste0(model, "_fullname"))
        tab4_table1[count,2] <<- "Given model lnL not defined to compute AIC"
        tab4_table1[count,3] <<- "Given model lnL not defined to compute AIC" 
      }
      else if(typeof(max_lnL)!='double') {
        count<<-count+1
        tab4_table1[count,1] <<- get(paste0(model, "_fullname"))
        tab4_table1[count,2] <<- "Non numeral value. Something is not right"
        tab4_table1[count,3] <<- "Non numeral value. Something is not right" 
      }
      else {
        AIC <- aic(length(get(paste(model,"params",sep="_"))),max_lnL)
        # print(data)
        PSSE <- psse(model,data$FT,model_params,input$percentData)
        print("PSSE -----------------------------------------")
        print(PSSE)
        count <<- count+1
        tab4_table1[count,1]<<- get(paste0(model, "_fullname"))
        tab4_table1[count,2]<<- AIC
        tab4_table1[count,3]<<- PSSE
      }
    }
    else if(typeof(model_params)=="character"){
      if(length(grep("not found",model_params))) {
        count<<-count+1
        tab4_table1[count,1] <<- model
        tab4_table1[count,2] <<- "Given-model not defined"
        tab4_table1[count,3] <<- "Given-model not defined" 
      }
      else {
        count<<-count + 1
        tab4_table1[count,1] <<- get(paste0(model, "_fullname"))
        tab4_table1[count,2] <<- "NON-CONV"
        tab4_table1[count,3] <<- "NON-CONV"
      }
    }
  }
  else{
    # -----> FC data should be handled here
  }
}

# Download handler for saving model result evaluation tables.

output$saveModelEvals <- downloadHandler(
  filename = function() {
    if(input$saveModelEvalType == "PDF") {
      paste(paste0(ModeledDataName, "_Model_Evals"), "pdf", sep=".")
    } else {
      paste(paste0(ModeledDataName, "_Model_Evals"), "csv", sep=".")
    }
  },
  content = function(filespec) {
    tab4_table1_2_save <- tab4_table1
    
    # Turn OutputTable to character representations to avoid
    # difficulties with NA, Inf, and NaN.
    
    TableNames <- names(tab4_table1_2_save)
    for (nameIndex in TableNames) {
      tab4_table1_2_save[[nameIndex]] <- as.character(tab4_table1_2_save[[nameIndex]])
    }
    names(tab4_table1_2_save) <- c("Model", "AIC", "PSSE")
    
    if(length(tab4_table1_2_save) <= 1) {
      tab4_table1_2_save <- data.frame()
    }
    
    if(input$saveModelEvalType == "PDF") {
      out_put = knit2pdf('Tab4ReportTemplate.Rnw', clean = TRUE)
      file.rename(out_put, filespec) # move pdf to file for downloading
    } else {
      utils::write.csv(tab4_table1_2_save, file=filespec, quote=TRUE, na="NA")
    }
  }
)


output$mytable2 <- DT::renderDataTable({
    source("utility/metrics/GOF.R")
    inFile <- input$file
    if(is.null(inFile)){
      return("Please upload a file")
    }
    
    ModelsToEval <- input$modelResultsForEval

    if(length(ModelsToEval)<=0) {
        return
    }
    
    print(ModelsToEval)
    tab4_table1 <<- data.frame()
    
    # Use the subset of data to which models were applied
    # to do the model evaluation.
    
    in_data_tab4 <- ModeledData
    timeOffset <- ModeledData$FT[1] - ModeledData$IF[1]
    in_data_tab4$FT <- in_data_tab4$FT - timeOffset
    
      if(length(ModelsToEval)>0){
        count <<- 0
        
        for(i in ModelsToEval){
          tab4_table1_construct(i,in_data_tab4,input)
        }

      tab4_table1 <<- data.frame(tab4_table1[1],tab4_table1[2],tab4_table1[3])
      names(tab4_table1) <<- c("Model","AIC","PSSE")
    }

    tab4_table1
  }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))

})

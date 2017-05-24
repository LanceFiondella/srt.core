library(shiny)
library(DT)
library(gdata) 
library(ggplot2)
library(knitr)
  source("utility/sources.R")
# Contributors guide step - 1
  # Text for error messages




shinyServer(function(input, output, clientData, session) {#reactive shiny function
  
  fileType <- NA
  openFileDatapath <- ""
  data_set_global <- ""
  FC_to_IF_data <- data.frame()
  DataModelIntervalStart <- 1
  DataModelIntervalEnd <- 5
  currentDatasetType <- ""

  # These two data frames hold model results as
  # well as the data to which models were applied.

  ModelResults <- data.frame()
  ModeledData <- data.frame()
  ModeledDataName <- ""

  # These two vectors identify the models that
  # ran successfully and those that did not.

  SuccessfulModels <- c()
  FailedModels <- c()

  # This is a list that hold the list of model evaluations.
  # Each set of model evaluations is a data frame - there's a
  # separate data frame for each model evaluation that's done.

  ModelEvalsList <- list()

  # Initialize "constants" ------------------------------------

  K_minDataModelIntervalWidth <- 5
  K_CategoryFirst <- 1
  K_CategoryLast <- 5

  # These lists identify the models used for each data type

  K_IF_ModelsList <- UI_modelList
  K_FC_ModelsList <- UI_modelList

  # Colors that will be used in plotting model results

  K_ModelResultColors <- list("JM"="red", "GM"="blue", "GO"="green", "DSS"="yellow", "Wei"="orange")

  # Tolerance used in determining whether a value is a whole number.

  K_tol <- .Machine$double.eps^0.5

  # Start main program ------------------------------------

  openFileDatapath <- ""
  source("serverTab1.R", local=TRUE)
  source("serverTab2.R", local=TRUE)
  source("serverTab3.R", local=TRUE)
  source("serverTab4.R", local=TRUE)

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
    
    LPTestStatistic <- reactive({
      if(input$trendPlotChoice=="LP") {
        testStat <- qnorm(1-input$confidenceLP)
      } else {
        testStat <- 0
      }
      testStat
    })


    # Here we monitor the data subset and model configuration controls in the
    # "Select, Analyze, and Subset Failure Data" and "Set Up and Apply Models"
    # tabs.  We read the values from the controls, and adjust the controls to make
    # sure that the modeling intervals and lengths of the modeling data set don't
    # go below specified minimal values.
      
    output$DataSubsetError <- renderText({
      #data_local <- data.frame(x=data_global())
      data_local <- data_global()
      data_local <- data_local$FRate
      
      if(!is.null(data_local)){
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
    }
    
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
        raw_data <<- data_global()
        ModeledDataName <<- data_set_global
        
        
        tempResultsList <- run_models(raw_data, input, K_tol)
        #print(tempResultsList[["Results"]])
        ModelResults <<- tempResultsList[["Results"]]
        #print("----------------------------------")
        #print("model results")
        #print(ModelResults)
        SuccessfulModels <<- tempResultsList[["SuccessfulModels"]]
        FailedModels <<- tempResultsList[["FailedModels"]]
        print("Failed Models")
        print(FailedModels)
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

})
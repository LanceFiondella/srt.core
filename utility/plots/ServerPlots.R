 #This file defines all the server definitions related to plots in the Shiny app. This includes all plots in all tabs!
 # This includes : Reactive data, Event Observers, Download Handlers and Plot renderers
 
 
 # A reactive data item that is used to control the height of the raw data and trend
    # plot.  The height is computed based on the width - it the plot is not as high
    # as it is wide, and if the width exceeds a minimum, then the height catches up with
    # the width to make a square plot.
    
    DTP_height <- reactive({
      Width <- session$clientData$output_DataAndTrendPlot_width
      Height <- session$clientData$output_DataAndTrendPlot_height
      if((Width > Height) && (Width > 400)) {
        Height <- Width*0.75
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
        Height <- Width*0.75
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
      
      DataAndTrendPlot <<- NULL   # Set the plot object to NULL to prevent error messages.
      #data <- data.frame(x=data_global())
      data <- data_global()
      
      if(!is.null(data)){
        data <- data$FRate
        DataColNames <- names(data)
        names(data) <- gsub("x.", "", DataColNames)
        if(length(names(data)) > 1) {
            Time <- names(data[1]) # generic name of column name of data frame (x-axis)
            Failure <- names(data[2]) # (y-axis)
            
            data_set <- input$dataSheetChoice
            if(input$PlotDataOrTrend == 1){
            
            # Plot the raw failure data
            
            DataAndTrendPlot <<- plot_failure_data(data, FC_to_IF_data, data_set, input$modelDataRange, input$dataPlotChoice, input$DataPlotType, K_minDataModelIntervalWidth)
            } else if (input$PlotDataOrTrend == 2) {
            
            # Plot the selected trend test
            
            DataAndTrendPlot <<- plot_trend_tests(data, FC_to_IF_data, data_set, input$modelDataRange, input$trendPlotChoice, input$confidenceLP, LPTestStatistic(), input$DataPlotType, K_minDataModelIntervalWidth)
            }
            
            DataAndTrendPlot <<- DataAndTrendPlot + coord_cartesian(xlim = DTPranges$x, ylim = DTPranges$y)
            
            DataAndTrendPlot
            
            #plot(data) Leave this here to use if ggplot() stops working. 
            } 
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
          ggsave(filespec,plot=DataAndTrendPlot,width=20,height=15)
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

    output$saveQueryResults <- downloadHandler(
      filename = function() {
        paste(paste0(ModeledDataName, "_Query_", input$modelPlotChoice), input$saveQueryResultsType, sep=".")
      },
      content = function(filespec) {
        ggsave(filespec,width=20,height=15)
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
          ggsave(filespec, plot=MRPlot, width=20,height=15)
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

      # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ----------------------------------------   Model Plot   ----------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------


    output$ModelPlot <- renderPlot({
      MRPlot <<- NULL
      if((length(input$modelResultChoice) > 0) && (input$modelResultChoice[1] != "None") && (!is.null(ModelResults)) && (!is.null(ModeledData))) {
        #MRPlot <- plot_model_results(ModelResults, ModeledData, ModeledDataName, input$modelResultChoice, input$modelPlotChoice, input$ModelDataPlotType, input$checkboxDataOnPlot, input$checkboxDataEndOnPlot, input$modelRelMissionTime, MPranges$x, MPranges$y, session$clientData$output_ModelPlot_width, input$modelCurveAdditionalTime)
        
        MRPlot <<- plot_model_results(ModelResults, SuccessfulModels, ModeledData, ModeledDataName, input, MPranges$x, MPranges$y, session$clientData$output_ModelPlot_width)
        if(!is.null(MRPlot)) {
          MRPlot <<- MRPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
        }
      }
      MRPlot
    }, height=MP_height)

    output$ModelPredictionPlot <- renderPlot({
      MRPlot <- NULL
      if((length(input$modelDetailChoice) > 0) && (input$modelDetailChoice[1] != "None") && (!is.null(ModelResults)) && (!is.null(ModeledData))) {

        MRPlot <- plot_model_prediction_results(input$modelDetailChoice, input$queryResultsPlotType, data_global(), input$C0, input$C1, input$C2, input$T)
      #   if(!is.null(MRPlot)) {
      #     MRPlot <- MRPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
      #   }
      }
      MRPlot
    }, height=MP_height)
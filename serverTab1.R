# Each tab has its own server file. Usually split into 3 sections: Data, Table and Plot

###############################################################################
#Tab1 Data Section
###############################################################################
output$sheetChoice <- renderUI({ 
      inFile <- input$file
      fileType <- getFileType(inFile)
      if(is.na(fileType)){
        return("Please upload an excel or csv file")
      } else 
      if(fileType == "xls"){
        sheets_present <- sheetNames(xls=inFile$datapath)
        selectInput("dataSheetChoice","Choose Sheet", c(NULL,sheets_present))
      }
      else {
        return("This file does not have multiple sheets")
      }
      })

 getInputFileData <- function(inFile, fileType, dataSheet){
    if(is.na(fileType)){
        return(NA)
    } else if(fileType == "csv"){
        return(read.csv(inFile$datapath, head = TRUE, sep = ',', quote = " % "))
    } else if(fileType == "xls" && !is.null(dataSheet)){
        return(read.xls(inFile$datapath, sheet=dataSheet))
    }
}

getFileType <- function(inFile){
    if(is.null(inFile)){
        return(NA)
        } else 
    if (length(grep(".csv",inFile$name))>0){
        return("csv")
    } else 
    if (length(grep(".xls",inFile$name))>0){
        return("xls")
    }
}



# Select and read in a data file.  This is a reactive data item.
    data_global <- reactive({
      data_generated <- NULL
      inFile <- input$file
      fileType <- getFileType(inFile)
      dataSheet <- input$dataSheetChoice
      data <- getInputFileData(inFile, fileType, dataSheet)
      #if(dataType(names(data))=="FR" || dataType(names(data))=="FC") {
      if(!is.na(data) && !is.null(data)){
        data_generated <- generateDataFrame(data)
       
        # Set up the initial values for modeling data range and the initial parameter
        # estimation range
        
        DataModelIntervalStart <<- 1
        if ("FRate" %in% names(data_generated) && !("FCount" %in% names(data_generated))) {
            DataModelIntervalEnd <<- length(data_generated$FRate[,1])
        } else {
            DataModelIntervalEnd <<- length(data_generated$FRate[,1])
            #DataModelIntervalEnd <<- length(data_generated$FCount[,1])
        }

        if((DataModelIntervalEnd - DataModelIntervalStart + 1) < K_minDataModelIntervalWidth){
            output$InputFileError <- renderText({msgDataFileTooSmall})
        } else {
            output$InputFileError <- renderText({""})
        }
        print("Setting plotchoices and models2run")
        
        # Complete all columns for FT/IF data, including failure number.
        # This information will be used later for subsetting the data.
        
        if("FCount" %in% names(data_generated) || "FRate" %in% names(data_generated)){
            
            print(names(data_generated))
            

            # Add a column for test intervals.
            
            #data_generated$TI <- c(1:length(data$FC))
            
            #FC_to_IF_data <<- FCFrame_to_IFFrame(data$T, data$FC)
            
            # Update the selection list for the models that can be run.
            
            updateSelectInput(session, "modelsToRun", choices = K_FC_ModelsList, selected = K_FC_ModelsList)
            
            # Update failure data view choices for CFC/FC data/model views.
            # Includes a "failure counts" view which IF/FT data does not.
            
            updateSelectInput(session, "dataPlotChoice",
                            choices = list( "Cumulative Failures" = "CF",
                                            "Failure Intensity" = "FI", "Times Between Failures" = "IF"), selected = "CF")
            # updateSelectInput(session, "modelPlotChoice",
            #                   choices = list("Failure Counts" = "FC", "Cumulative Failures" = "MVF",
            #                                  "Failure Intensity" = "FI", "Times Between Failures" = "IF", "Reliability" = "R","Reliability Growth"="R_growth"), selected = "MVF")
            updateSelectInput(session, "modelPlotChoice",
                            choices = list( "Cumulative Failures" = "MVF",
                                            "Failure Intensity" = "FI", "Times Between Failures" = "IF", "Reliability Growth"="R_growth"), selected = "MVF")
            
            
        
        
        updateSliderInput(session, "modelDataRange",
                            min = DataModelIntervalStart, value = c(DataModelIntervalStart, DataModelIntervalEnd),
                            max = DataModelIntervalEnd)
        
        } 
        

      }  
      # Finally, output data set
      
      data_generated
  }) 

###############################################################################
#Tab1 Table Section
###############################################################################


# Set up the data and trend test statistics tables for display
    
    FailureDataTable <- reactive ({
      DataTrendTable <- NULL
      #if (!(is.null(input$file) && (input$type == 2)) || (!(is.null(input$dataSheetChoice)) && (input$type == 1))) {
      if(!(is.null(input$file))){
        if (input$DataPlotAndTableTabset == "Data and Trend Test Table") {
          data <- data.frame(x=data_global()$FRate)
          DataTrendTable <- data_or_trend_table(data, input$modelDataRange, input$PlotDataOrTrend, input$trendPlotChoice)
        }
      }
      DataTrendTable
    })

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

###############################################################################
#Tab1 Plot Section
###############################################################################

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
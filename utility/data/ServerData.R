#This file includes all the reactive data, functions etc. related to handling the input data to the Shiny App

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
        DataModelIntervalEnd <<- length(data_generated$FRate[,1])
        if((DataModelIntervalEnd - DataModelIntervalStart + 1) < K_minDataModelIntervalWidth){
            output$InputFileError <- renderText({msgDataFileTooSmall})
        } else {
            output$InputFileError <- renderText({""})
        }
        print("Setting plotchoices and models2run")
        
        # Complete all columns for FT/IF data, including failure number.
        # This information will be used later for subsetting the data.
        
        if("FCount" %in% names(data_generated) || "FRate" %in% names(data_generated)){
            print("FC data detected")
            

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
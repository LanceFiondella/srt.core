##########
#UI Elements
##########

output$tab2UI <- renderUI({
  
  fluidRow(
    # column(12,
    #       uiOutput("ParameterInterval")
    # ),
    
    h4(translate("str_configure")),
    h5(translate("str_specfail")),
    
    column(12,
           numericInput("modelNumPredSteps", 
                        label = h6(translate("str_howmany")),
                        min = 1, value = 1)
    ),
    
    column(12, 
           selectInput(
             "modelsToRun", label = h6(translate("str_choosemodels")), 
             choices=list("Open a data set to run models"="None"),
             multiple=TRUE, selected="None"
           )
    ),
    
    column(12,
           actionButton("runModels", label = translate("x_runmodels"))
    ),
    
    #Heading is to close to button, needs formatting
      
    h4(translate("str_display")),
    
    column(12, 
           #<<<<<<< HEAD
           selectInput(
             "modelResultChoice", label = h6(translate("str_chooseresult")), 
             choices=list("No model results to display"="None"),
             multiple=TRUE, selected="None"
           )
           #=======
           #           br(),
           #           h5("Choose the model results to display."),
           #           selectInput("modelResultChoice", label = h6("Choose one or more sets of model results"), 
           #                       choices= models,
           #                       multiple=TRUE
           
           #           )
           ##>>>>>>> lfiondella/master
     ),
    
    #column(12, 
    #       h5("Choose the type of plot for model results."),
    #      selectInput("modelPlotChoice", label = h6("Choose a plot type"), 
    #                   choices = list("Times Between Failures" = "MTTF", "Cumulative Failures" = "MVF",
    #                                 "Failure Intensity" = "FI", "Reliability" = "R","Reliability Growth"="R_growth"), selected = "MVF")
    #),
    column(12, 
           h5(translate("str_chooseplot")),
           selectInput("modelPlotChoice", label = h6(translate("str_plottype")), 
                       choices = list("Times Between Failures" = "MTTF", "Cumulative Failures" = "MVF",
                                      "Failure Intensity" = "FI", "Reliability Growth"="R_growth"), selected = "MVF")
    ),
    column(12,
           conditionalPanel(
             condition = "input.modelPlotChoice == 'R_growth'",
             numericInput("modelRelMissionTime", 
                          label = h6(translate("str_interval")),
                          min = 0, value = 1)
             #                                                      numericInput("modelTargetReliability",
             #                                                                   label=h6("Specify the reliability to be achieved"),
             #                                                                   min=0, max=1, step=0.01, value=0.9) 
             
           )
    ),
    column(12,
           numericInput("modelCurveAdditionalTime",
                        label=h6(translate("str_duration")),
                        min=0, value=100, step=1000)
    ),
    
    column(12, 
           checkboxInput("checkboxDataOnPlot", label = translate("chk_showdata"), value = TRUE)
    ),
    column(12, 
           checkboxInput("checkboxDataEndOnPlot", label = translate("chk_showend"), value = TRUE)
    ),
    
    column(12, 
           radioButtons("ModelDataPlotType", label = h6(translate("str_draw")),
                        choices = list("Both" = "points_and_lines", "Points" = "points", "Lines" = "lines"),
                        inline=TRUE,
                        selected = "points_and_lines")
    ),
    
    column(12, 
           radioButtons("saveModelResultsType", label = h6(translate("str_filetype")),
                        choices = list("JPEG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                        selected = "JPG")
    ),
    column(8, downloadButton(outputId = "saveModelResults", label = "Save"))
  )

})

###############################################################################
#Tab2 Table Section
###############################################################################


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
        if(input$modelResultChoice[1]=="None"){
          return(MR_Table)
        }
        if(is.null(ModelResults)){
          return
        }
        else if(!is.null(ModelResults)) {
          if(length(input$AllModelsRun) > 0) {
            
            # User has selected at one model to display as a table.
            
            #MR_Table <- model_result_table(ModelResults, length(ModeledData[,1]), input$AllModelsRun, input$modelRelMissionTime)
            MR_Table <- model_result_table(ModelResults, length(ModelResults), input$AllModelsRun, input$modelRelMissionTime)
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
            #MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Reliability"))
            MR_Table_Names <- c(MR_Table_Names, paste0(modelName, "_Rel_Growth"))
            names(MR_Table) <- MR_Table_Names
            
          }
        }
        #MR_Table = round_table(MR_Table, 6)
        MR_Table
      }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))


###############################################################################
#Tab2 Plot Section
###############################################################################

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
        
        MRPlot <<- plot_model_results(ModelResults, ModeledData, ModeledDataName, input, MPranges$x, MPranges$y, session$clientData$output_ModelPlot_width)
        if(!is.null(MRPlot)) {
          MRPlot <<- MRPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
        }
      }
      MRPlot
    }, height=MP_height)

    
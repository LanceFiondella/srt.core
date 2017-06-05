 #This file defines all the server definitions related to tables in the Shiny app. This includes all tables in all tabs!


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

    # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # --------------------------- Display selected model results in tabular form  --------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
    
      output$ModelResultTable <- DT::renderDataTable({
        SuffixConfInt <- c("Low", "MLE", "High")
        ModelResultType <- c("CumTime", "MVF", "IF", "FI", "R_growth")
        
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
            MR_Table <- model_result_table(ModelResults, length(ModeledData[[1]][,1]), input, input$AllModelsRun, input$modelRelMissionTime)
            #MR_Table <- model_result_table(ModelResults, length(ModelResults), input, input$AllModelsRun, input$modelRelMissionTime)
          }
        }
        
        
        if (length(MR_Table) <= 1) {
          MR_Table <- data.frame()
        } else {
          # Set column names for the model results table
          
          TableModelParms <- NULL
          TableModelParms[["Low"]] <- input$LowConfOnTable
          TableModelParms[["MLE"]] <- input$MLEOnTable
          TableModelParms[["High"]] <- input$HighConfOnTable
          
          MR_Table_Names <- c("Failure")
          for (modelName in input$AllModelsRun) {
            
            for (modelParmNum in 1:length(get(paste0(modelName, "_params")))) {
              for (SuffixTag in SuffixConfInt) {
                if (TableModelParms[[SuffixTag]]) {
                  MR_Table_Names <- c(MR_Table_Names, paste(modelName, get(paste0(modelName, "_params"))[modelParmNum], SuffixTag, sep="_"))
                }
              }
            }
            
            for (ResultType in ModelResultType) {
              for (SuffixTag in SuffixConfInt) {
                if (TableModelParms[[SuffixTag]]) {
                  MR_Table_Names <- c(MR_Table_Names, paste(modelName, ResultType, SuffixTag, sep="_"))
                }
              }
            }
          }
          names(MR_Table) <- MR_Table_Names
          
        }
        #MR_Table = round_table(MR_Table, 6)
        MR_Table
      }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))


        # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ----------------------------------------   TAB3 Table   ----------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------


  tab3_table1_construct <- function(model,data,input){
    if(dataType(names(data))=="FR"){
      print("Constructing table 3")
      last_row <- length(ModelResults[,1]) - PredAheadSteps
      #model_params <- as.data.frame(matrix(0, ncol=length(ModelResults[1,]), nrow = 1))
      #colnames(model_params) <- colnames(ModelResults)
      model_params_label <- paste(model,"params",sep="_")
      model_params <- as.data.frame(matrix(0, ncol=length(get(model_params_label)), nrow = 1))
      
      #Generating model_params from ModelResults. If the column is a list, it is converted to numeric
      #for(i in 1:length(model_params[1,])){
      #  if (typeof(model_params[1,i]) == "list"){
      #      model_params[1, i] <- as.numeric(model_params[1,i][[1]])
      #  }
      #  else{
      #      model_params[1, i] <- ModelResults[last_row, i]
      #  }
      #}
      
      parmNames <- c()
      for (paramNum in 1:length(get(model_params_label))) {
        model_parm_num <- paste0(model, "_", get(model_params_label)[paramNum], "_MLE")
        parmNames <- c(parmNames, model_parm_num)
        model_params[1,paramNum] <- ModelResults[[model_parm_num]][last_row]
      }
      colnames(model_params) <- parmNames
      
      # Debug code
      #print("Data type")
      #print(dataType(names(data)))
      #print("ModelResults last row")
      #print(ModelResults[last_row,])
      #print("Model parameters")
      #print(model_params)
      #print("Length of model_params")
      #print(length(ModelResults[1,]))
      #print("Number of steps ahead to predict")
      #print(PredAheadSteps)
      
      
      if(typeof(model_params)!="character"){
        number_fails <- get_prediction_k( model,
                                          model_params, 
                                          input$modelDetailPredTime, 
                                          #data$FT[length(get("data")[[get(paste(model,"input",sep="_"))]])],
                                          data$FRate$FT[last_row],
                                          #length(get("data")[[get(paste(model,"input",sep="_"))]])
                                          last_row)
        
        time_fails <- get_prediction_t( model,
                                        model_params, 
                                        input$modelDetailPredFailures,
                                        #data$FT[length(get("data")[[get(paste(model,"input",sep="_"))]])],
                                        data$FRate$FT[last_row],
                                        #length(get("data")[[get(paste(model,"input",sep="_"))]]))
                                        last_row)
        rel_time <- get_reliability_t(model,
                                      model_params, 
                                      input$modelTargetReliability, input$modelRelMissionTime2, 
                                      #data$FT[length(get("data")[[get(paste(model,"input",sep="_"))]])],
                                      data$FRate$FT[last_row],
                                      #length(get("data")[[get(paste(model,"input",sep="_"))]]))
                                      last_row)

        opt_release_time <- get_optimal_release_time_CC(model, model_params, input$C0, input$C1, input$C2)
        cost_at_rel_time <- get_cost_at_time(model,model_params, rel_time, input$T, input$C0, input$C1, input$C2)
        #cost_at_rel_time <- 0
        reliability_at_opt_release_time <- get_rel_at_opt_release_time(model, model_params, opt_release_time, input$modelRelMissionTime)
        cost_at_opt_release_time <- get_cost_at_time(model,model_params, opt_release_time, input$T, input$C0, input$C1, input$C2)
        

        # opt_release_time <- input$C0 + input$C1 + input$C2
        #print(time_fails)
        #print(number_fails)
        #print(rel_time)
        #print(opt_release_time)

      

        ExpectedNumFailuresExceeded <- FALSE
        for( i in 1:length(time_fails)){
          if(!ExpectedNumFailuresExceeded){
            count <<- count+1
            
            if(i == 1) {
              tab3_table1[count,1]<<- get(paste0(model, "_fullname"))
              tab3_table1[count,2]<<- as.character(rel_time)
              tab3_table1[count,3]<<- number_fails
              
            } else {
              tab3_table1[count,2]<<- " "
              tab3_table1[count,3]<<- " "
            }
            tab3_table1[count,4]<<- i
            tab3_table1[count,5]<<- time_fails[i]
            
            if(i==1) {
              tab3_table1[count,6]<<- opt_release_time
              tab3_table1[count,7]<<- cost_at_rel_time
              tab3_table1[count,8]<<- reliability_at_opt_release_time
              tab3_table1[count,9]<<- cost_at_opt_release_time
            }

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
          tab3_table1[count,6] <<- "Given-model not defined"
          tab3_table1[count,7] <<- "Given-model not defined"
          tab3_table1[count,8] <<- "Given-model not defined"
          tab3_table1[count,9] <<- "Given-model not defined"
        }
        else{
          count<<-count+1
          tab3_table1[count,1] <<- model
          tab3_table1[count,2] <<- "NON-CONVERGENCE"
          tab3_table1[count,3] <<- "NON-CONVERGENCE"
          tab3_table1[count,4] <<- "NON-CONVERGENCE"
          tab3_table1[count,5] <<- "NON-CONVERGENCE"
          tab3_table1[count,6] <<- "NON-CONVERGENCE"
          tab3_table1[count,7] <<- "NON-CONVERGENCE"
          tab3_table1[count,8] <<- "NON-CONVERGENCE"
          tab3_table1[count,9] <<- "NON-CONVERGENCE"
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
          out_put = knit2pdf('./utility/tables/Tab3ReportTemplate.Rnw', clean = TRUE)
          file.copy(out_put, filename) # move pdf to file for downloading
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
            count <<- count
            tab3_table1_construct(i,in_data_tab3,input)
          }

        tab3_table1 <<- data.frame(tab3_table1[1],tab3_table1[2],tab3_table1[3], tab3_table1[4], tab3_table1[5], tab3_table1[6], tab3_table1[7], tab3_table1[8], tab3_table1[9])
        names(tab3_table1) <<- c("Model",paste("Time to achieve R =", as.character(input$modelTargetReliability), "for mission of length", as.character(input$modelRelMissionTime2)) ,paste("Expected # of failures for next", as.character(input$modelDetailPredTime) ,"time units"), paste0("Nth failure"), paste("Expected times to next", as.character(input$modelDetailPredFailures),"failures"), "Optimal release time","Cost to achieve tR*", "Reliability at Optimal Release Time", "Cost to achieve tC*")
        
        tab3_table1 = round_table(tab3_table1, 6) #Rounding values in the table to 6 decimal places

      tab3_table1
    }
  }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                                  rowCallback = JS("
                                      //This callback function is meant to display only 6 decimals of precision. col_list keeps an array of columns which will be affected. 
                                      function( row, data, index) {
                                          var col_list = [2,3,5,6,7,8,9]
                                          for (var i =0; i < col_list.length; i++) {
                                              if(parseFloat(data[col_list[i]]))
                                                  $('td:eq('+ col_list[i].toString() +')',row).html(parseFloat(data[col_list[i]]).toFixed(6));
                                      }
                                      
                                  }
                                  ")))

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
      #model_params <- try(get(paste(model,get(paste(model,"methods",sep="_"))[1],"MLE",sep="_"))(get(paste("data"))[[get(paste(model,"input",sep="_"))]]),silent=TRUE)
      last_row <- length(ModelResults[,1]) - PredAheadSteps
      model_params <- as.data.frame(matrix(0, ncol=length(ModelResults[1,]), nrow = 1))
      colnames(model_params) <- colnames(ModelResults)
      
      #Generating model_params from ModelResults. If the column is a list, it is converted to numeric
      for(i in 1:length(model_params[1,])){
        if (typeof(model_params[1,i]) == "list"){
            model_params[1, i] <- as.numeric(model_params[1,i][[1]])
        }
        else{
            model_params[1, i] <- ModelResults[last_row, i]
        }
      }
      if(typeof(model_params)!="character"){
        # number_fails <- get_prediction_n(model_params,input$modelDetailPredTime,length(get("data")[[get(paste(model,"input",sep="_"))]]))
        #max_lnL <- try(get(paste(model,"lnL",sep="_"))(get("data")[[get(paste(model,"input",sep="_"))]],model_params),silent=TRUE)
        #max_lnL <- try(get(paste(model,"lnL",sep="_"))(ModelResults[1:last_row,1],model_params),silent=TRUE)
        max_lnL <- try(get(paste(model,"lnL",sep="_"))(model_params,names(model_params),FALSE,get("data")[[get(paste(model,"input",sep="_"))]]),silent=TRUE)
        # time_fails <- get_prediction_t(model_params, input$modelDetailPredFailures, length(get("data")[[get(paste(model,"input",sep="_"))]]))
      
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
          
          PSSE <- psse(model,ModelResults[1:last_row,1],model_params,input$percentData)
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
      tab4_table1 = round_table(tab4_table1, 6)

      tab4_table1
    }, filter="top", options = list(scrollX=TRUE, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))))



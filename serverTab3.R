###############################################################################
#Tab3 Table Section
###############################################################################

  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ----------------------------------------   TAB3 Table   ----------------------------------------------
  # ------------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------


  tab3_table1_construct <- function(model,data,input){
    if(dataType(names(data))=="FR"){
      #print("Constructing table 3")
      last_row <- length(ModelResults[,1]) - PredAheadSteps
      model_params <- as.data.frame(matrix(0, ncol=length(ModelResults[1,]), nrow = 1))
      colnames(model_params) <- colnames(ModelResults)
      
      #Generating model_params from ModelResults. If the column is a list, it is converted to numeric
      for(i in 1:length(model_params[1,])){
        if (typeof(model_params[1,i]) == "list"){
            model_params[1, i] <- as.numeric(ModelResults[1,i][[1]])
        }
        else{
            model_params[1, i] <- ModelResults[last_row, i]
        }
      }
      
      
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
          names(tab3_table1_2_save) <- c("Model", 
                                        paste0("Time to R=", as.character(input$modelTargetReliability)), 
                                        paste("Num failures in", as.character(input$modelDetailPredTime)), 
                                        "Failure", 
                                        "Times to failures")

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
        names(tab3_table1) <<- c("Model",
                                paste("Time to achieve R =", as.character(input$modelTargetReliability), "for mission of length", as.character(input$modelRelMissionTime2)),
                                paste("Expected # of failures for next", as.character(input$modelDetailPredTime) ,"time units"), 
                                "Nth failure", 
                                paste("Expected times to next", as.character(input$modelDetailPredFailures),"failures"),
                                "Optimal release time",
                                "Cost to achieve tR*",
                                "Reliability at Optimal Release Time",
                                "Cost to achieve tC*")
        
        tab3_table1 <<- round_table(tab3_table1, 6) #Rounding values in the table to 6 decimal places

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

###############################################################################
#Tab3 Plot Section
############################################################################### 
output$ModelPredictionPlot <- renderPlot({
      MRPlot <- NULL
      if((length(input$modelDetailChoice) > 0) && (input$modelDetailChoice[1] != "None") && (!is.null(ModelResults)) && (!is.null(ModeledData))) {
        last_row <- length(ModelResults[,1]) - PredAheadSteps
        model_params <- as.data.frame(matrix(0, ncol=length(ModelResults[1,]), nrow = 1))
        colnames(model_params) <- colnames(ModelResults)
        #Generating model_params from ModelResults. If the column is a list, it is converted to numeric
      for(i in 1:length(model_params[1,])){
        if (typeof(model_params[1,i]) == "list"){
            model_params[1, i] <- as.numeric(ModelResults[1,i][[1]])
        }
        else{
            model_params[1, i] <- ModelResults[last_row, i]
        }
      }
      
        #MRPlot <- plot_model_prediction_results(input$modelDetailChoice, input$queryResultsPlotType, data_global(), input$C0, input$C1, input$C2, input$T)
        MRPlot <- plot_model_prediction_results(input$modelDetailChoice, input$queryResultsPlotType, ModeledData, model_params, input$C0, input$C1, input$C2, input$T)
      #   if(!is.null(MRPlot)) {
      #     MRPlot <- MRPlot + coord_cartesian(xlim = MPranges$x, ylim = MPranges$y)
      #   }
      }
      MRPlot
    }, height=MP_height)

    output$saveQueryResults <- downloadHandler(
      filename = function() {
        paste(paste0(ModeledDataName, "_Query_", input$modelPlotChoice), input$saveQueryResultsType, sep=".")
      },
      content = function(filespec) {
        ggsave(filespec,width=20,height=15)
      }

    )
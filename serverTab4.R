###############################################################################
#Tab4 Table Section
###############################################################################

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
            model_params[1, i] <- as.numeric(ModelResults[1,i][[1]])
        }
        else{
            model_params[1, i] <- ModelResults[last_row, i]
        }
      }
      if(typeof(model_params)!="character"){
        # number_fails <- get_prediction_n(model_params,input$modelDetailPredTime,length(get("data")[[get(paste(model,"input",sep="_"))]]))
        #max_lnL <- try(get(paste(model,"lnL",sep="_"))(get("data")[[get(paste(model,"input",sep="_"))]],model_params),silent=TRUE)
        
        #print(data_global()$FRate['FT'])

        if ("FT" %in% get(paste(model,"input",sep="_"))){
            max_lnL <- try(get(paste(model,"lnL",sep="_"))(data_global()$FRate$FT,model_params),silent=FALSE)
            
        } else if ("IF" %in% get(paste(model,"input",sep="_"))){
            
            max_lnL <- try(get(paste(model,"lnL",sep="_"))(data_global()$FRate$IF,model_params),silent=FALSE)
            
        } else {
            print("NOTHING found!")
        }
            
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
            #print(paste0("Length of model_params = ", length(get(paste(model,"params",sep="_")))))
            
          AIC <- aic(length(get(paste(model,"params",sep="_"))),max_lnL)
          
          PSSE <- psse(model,data_global()$FRate$FT,model_params,input$percentData)
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

###############################################################################
#Tab4 Plot Section
###############################################################################

model_specifications_test <- function(directory, model_file){
  test_results <- data.frame()
  #names(test_results) <- c("Test Name", "Passed", "Reason")

  #Check model input
  curr_model_finite <- get(paste(directory,"_Finite",sep=""))
  test_results <- rbind(test_results, test_finite(curr_model_finite))
  
  curr_model_input <- get(paste(directory,"_input",sep=""))
  test_results <- rbind(test_results, test_input(curr_model_input))
  
  curr_model_methods <- get(paste(directory,"_methods",sep=""))
  test_results <- rbind(test_results, test_methods(curr_model_methods))
  
  curr_model_fullname <- get(paste(directory,"_fullname",sep=""))
  test_results <- rbind(test_results, test_fullname(curr_model_fullname))
    
  curr_model_plotcolor <- get(paste(directory,"_plotcolor",sep=""))
  test_results <- rbind(test_results, test_plotcolor(curr_model_plotcolor))
  
  #print(all(isTRUE(test_results[,"Result"])))
  if (FALSE %in% test_results[,"Passed"]){
    cat('Model Specifications Test : FAILED\n')
    cat('The results of the test are shown below : \n')
    print(test_results)
    quit("no",status = -1)
  }
  else{
    cat('Model Specifications Test : PASSED\n')
    
  }

  #if(!ret) quit("no",status = -1)
}



model_functions_test <- function(directory,model_file){
   # TODO:
  #  @model_file -> ?
  needed_params(directory)
  needed_MVF(directory)
  needed_MVF_inv(directory)
  needed_MVF_cont(directory)
  needed_MTTF(directory)
  needed_FI(directory)
  #needed_R(directory)
  needed_lnL(directory)
  needed_R_growth(directory)
  # needed_FaultsRemaining(directory)
  needed_R_delta(directory)
  needed_Target_T(directory)

}

modelspecifications_test <- function(directory,model_file){
  # TODO:
  #  @model_file -> ?
  curr_model_input <- get(paste(directory,"_input",sep=""))
  needed_input(curr_model_input)
  
  curr_model_methods <- get(paste(directory,"_methods",sep=""))
  needed_methods(curr_model_methods)
  
  curr_model_fullname <- get(paste(directory,"_fullname",sep=""))
  needed_fullname(curr_model_fullname)
  
  curr_model_plotcolor <- get(paste(directory,"_plotcolor",sep=""))
  needed_plotcolor(curr_model_plotcolor)
  
  curr_model_Finite <- get(paste(directory,"_Finite",sep=""))
  needed_Finite(curr_model_Finite)
  cat("|TEST     --> |PASSED \n")
   
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

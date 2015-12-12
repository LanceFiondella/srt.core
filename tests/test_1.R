curr_model_test <- function(directory,model_file){
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
  print("Complete!")  
}

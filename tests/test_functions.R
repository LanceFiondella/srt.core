is_valid_input <- function(d){
  # input is names of input data file
  # d <- names(data)
  if("FT" %in% d){
    return(TRUE)
  }
  else if("IF" %in% d){
    return(TRUE)
  }
  else if("FN" %in% d){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

is_valid_result <- function(d){
  if(is.data.frame(d)){
    return(TRUE)
  }else return(FALSE)
}

is_finite <- function(d){
  if(d==TRUE||d==FALSE){
   return(TRUE) 
  }else return(FALSE)
}

is_valid_string <- function(d){
  if(is.character(d)){
    return(TRUE)
  }else return(FALSE)
}

is_valid_MVF <- function(d){
  
}

findFunction <- function(s){
  return(s %in% eval(listfunctions))
}
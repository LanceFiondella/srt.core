source('tests/test_functions.R')
needed_Finite <- function(obj){
	ret <- tryCatch(is.logical(obj),
			warning = function(w){
				print(w)
				quit("no",status = -1)
			},
			error = function(e){
				print(e)
				quit("no",status = -1)
			}
			)
	if(!ret) quit("no",status = -1)
	return(ret)
}

needed_input <- function(obj){
	ret <- tryCatch(is_valid_input(obj),
			warning = function(w){
				print(w)
				quit("no",status = -1)
			},
			error = function(e){
				print(e)
				quit("no",status = -1)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}

needed_fullname <- function(obj){
	ret <- tryCatch(is_valid_string(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}
needed_plotcolor <- function(obj){
	ret <- tryCatch(is_valid_string(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}
needed_methods <- function(obj){
	#TODO:
	#   User can mention many methods with prority order
	ret <- tryCatch(is_valid_string(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	if(!ret) quit("no",status = -1)
	return(ret)
}

needed_MLE <- function(model,obj){
	found_MLE <- tryCatch(as.logical(
						length(findFunction(
							paste(model,get(
								paste(model,"method",sep="_")),"MLE",sep="_")))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MLE){
		quit("no",status = -1)
	}
	else{
		
	}
	return(found_MLE)
}

needed_MVF <- function(model, obj){
	found_MVF <- tryCatch(as.logical(
						length(findFunction(
							paste(model,"MVF",sep="_")))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MVF){
		quit("no",status = -1)
	}
	else{

	}
	return(found_MVF)
}

needed_MTTF <- function(model, obj){
	found_MTTF <- tryCatch(as.logical(
						length(findFunction(
							paste(model,"MTTF",sep="_")))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_MTTF){
		quit("no",status = -1)
	}
	else{

	}

	return(found_MTTF)
}

needed_FI <- function(model, obj){
	found_FI <- tryCatch(as.logical(
						length(findFunction(
							paste(model,"FI",sep="_")))),

			warning = function(w){
				print(w)
				#stop("Something went wrong!",model)
				return(FALSE)
			},
			error = function(e){
				print(typeof(e))
				#stop("Something went wrong!",model)
				return(FALSE)
			}
		)

	if(!found_FI)quit("no",status = -1)
	return(found_FI)
}



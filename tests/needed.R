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

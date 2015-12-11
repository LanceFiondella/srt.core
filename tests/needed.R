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
	return(ret)
}

needed_input <- function(){
	ret <- tryCatch(is_valid_input(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			})
	return(ret)
}

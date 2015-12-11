needed_Bool <- function(obj){
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

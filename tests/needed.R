needed_Bool <- function(obj){
	ret <- tryCatch(is.logical(obj),
			warning = function(w){
				print(w)
			},
			error = function(e){
				print(e)
			}
			)
	return(ret)
}

#A factory to trycatch functions and return their output, warning and errors
factory <- function(fun, name)
    function(...) {
        warn <- err <- NULL
        res <- withCallingHandlers(
            tryCatch(fun(...), error=function(e) {
                err <<- conditionMessage(e)
                NULL
            }), warning=function(w) {
                warn <<- append(warn, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
        data.frame("Test Name" = name, "Passed" = res )
    }

#Tests for model specifications************************

#Test for finiteness
test_finite <- function(obj){
    factory(is.logical, "Finite")(obj)
}

#Test input
test_input <- function(obj){
	factory(is_valid_input, "Input")(obj)
}

#Test full name
test_fullname <- function(obj){
	factory(is_valid_string, "Full Name")(obj)
}

#Test Plot colors
test_plotcolor <- function(obj){
	factory(is_valid_string, "Plot Color")(obj)
}

#Test Methods
test_methods <- function(obj){
	factory(is_valid_string, "Methods")(obj)
}

#Tests for function definitions *************************
test_MLE <- function(obj){

	
}
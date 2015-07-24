# Detailed prediction

get_prediction_n <- function(x,steps){
	t <-0
	if('N0' %in% names(x)){
		t <- 0
		for(i in 1:steps){
			t <- (1/(x$Phi*(x$N0-(i-1)))) + t
		}
	}
	else if('D0' %in% names(x)){
		t <- 0
		for(i in 1:steps){
			t <- 1/(x$D0*(x$Phi)^i)+ t
		}
	}
	
	t
}

get_prediction_t <- function(){
	
}
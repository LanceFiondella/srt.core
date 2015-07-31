# Detailed prediction
get_prediction_t <- function(x,time,n){
	total_time <- 0
	i <- 0
	if('N0' %in% names(x)){
		while( total_time < time){
			i <- i + 1
			total_time <- (1/(x$Phi*(x$N0-(time+i-1)))) + total_time
			if(total_time > time){
				i <- i - 1 
			}			
		}
	}
	else if('D0' %in% names(x)){
		while( total_time < time){
			i <- i +1
			total_time <- (1/(x$D0*(x$Phi)^(time+i-1))) + total_time
			if(total_time > time){
				i <- i - 1 
			}	
		}
	}
	i
	
}

get_prediction_n <- function(x,steps,n){
	t <-0
	if('N0' %in% names(x)){
		t <- 0
		if(steps!=0){
			for(i in 1:steps){
				t <- (1/(x$Phi*(x$N0-(n+i-1)))) + t
			}
		}
	}
	else if('D0' %in% names(x)){
		t <- 0
		if(steps!=0){
			for(i in 1:steps){
				t <- (1/(x$D0*(x$Phi)^(n+i-1)))+ t
			}
		}
	}
	else{
		return("No parameter defined")
	}
	t
}


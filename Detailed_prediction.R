# Detailed prediction
get_prediction_n <- function(x,t,n){

	total_time <- 0
	print("Inside function")
	print(t)
	i <- 0
	if('JM_N0' %in% names(x)){
		while( total_time < t){
			i <- i + 1
			total_time <- (1/(x$JM_Phi*(x$JM_N0-(t+i-1)))) + total_time
			if(total_time > t){
				i <- i - 1 
			}			
		}
	}
	else if('GM_D0' %in% names(x)){
		while( total_time < t){
			i <- i +1
			total_time <- (1/(x$GM_D0*(x$GM_Phi)^(t+i-1))) + total_time
			if(total_time > t){
				i <- i - 1 
			}	
		}
	}
	else if('GO_aMLE' %in% names(x)){
			while(total_time < t){
				i <- i +1
				total_time <- (1/(x$GO_aMLE*(x$GO_bMLE)^(t+i-1))) + total_time
				if(total_time > t){
					i <- i - 1 
				}	
			}
		}
	else{
		i <- "Model parameter not defined"
	}
	i	
}

get_prediction_t <- function(x,steps,n){
	t <-0
	time_indexes <- c()
	if('JM_N0' %in% names(x)){
		t <- 0
		if(steps!=0){
			for(i in 1:steps){
				if((x$JM_N0 - n)>=i){
					t <- (1/(x$JM_Phi*(x$JM_N0-(n+i-1)))) + t
					time_indexes[i] <- t
				}
				else{
					time_indexes[i] <- "NA"
				}
			}
		}
	}
	else if('GM_D0' %in% names(x)){
		t <- 0
		if(steps!=0){
			for(i in 1:steps){
				t <- (1/(x$GM_D0*(x$GM_Phi)^(n+i-1)))+ t
				time_indexes[i] <- t
				# if((x$GM_N0 - n)>=i){
				# 	t <- (1/(x$GM_D0*(x$GM_Phi)^(n+i-1)))+ t
				# 	time_indexes[i] <- t
				# }
				# else{
				# 	time_indexes[i] <- "NA"
				# }
			}
		}
	}
	else if("GO_aMLE" %in% names(x)){
		t <- 0
		if(steps!=0){
			t_prev <- 0
			for(i in 1:steps){
				t_now <- (1/(x$GO_aMLE*(x$GO_bMLE)^(n+i-1)))
				t <- t_now + t_prev
				time_indexes[i] <- t
				t_prev <-t_now
	 				}
	 			}
	 		}
	else{
		return("Model Parameter not defined")
	}
	time_indexes

}


mvf_nhpp <- function(a,b,t){
	return(a*(1-exp(-b*t)))
}

reliability_nhpp <- function(a,b,cur_time,delta){
	return(exp(-(mvf_nhpp(a,b,(cur_time+delta)) - mvf_nhpp(a,b,cur_time))))
}

	
reliability_nhpp_mle <- function(a,b,cur_time,delta, reliability){
	target_mission_time <- reliability - exp(a*(1-exp(-b*cur_time)) -a*(1-exp(-b*(cur_time+delta))))
	return(target_mission_time)
}


maxiter <- 1000
reliability_target_time <- function(a,b,cur_time,delta, reliability){

	f <- function(t){
		return(reliability_nhpp_mle(a,b,t,delta, reliability))
	}

	current_rel <- reliability_nhpp(a,b,cur_time,delta)
	if(current_rel < reliability){
	    sol <- tryCatch(
	      uniroot(f, c(cur_time,cur_time + 50),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
	      warning = function(w){
	      #print(f.lower)
	        if(length(grep("_NOT_ converged",w[1]))>0){
	          maxiter <<- maxiter+10
	          print(paste("recursive", maxiter,sep='_'))
	          reliability_target_time(a,b,cur_time,delta, reliability)
	        }
	      },
	      error = function(e){
	        print(e)
	        #return(e)
	      })
	}
	else {
		sol <- "Target reliability already achieved"
	}
    sol
  }

reliability_target_time_plot <- function(a,b,cur_time,delta, reliability){	
	
	r <-data.frame()
	tt_index <- seq(0,cur_time,cur_time/1000)
  	for(i in 1:length(tt_index)){	  
	    r[i,1] <- tt_index[i]
	    temp <- reliability_nhpp(a,b,tt_index[i],delta)
	    #print(typeof(temp))
	    if(typeof(temp) != typeof("character")){
	    	r[i,2] <- temp
	    }
	    else{
	    	r[i,2] <- "NA"
	    }	    
	  }
	  g <- data.frame(r[1],r[2])
	  names(g) <- c("Time","Reliability")
	  #print(g)
	  g
	    
}
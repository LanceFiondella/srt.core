# # Detailed prediction


get_prediction_k <- function(model,params, t_offset,tn,n){
	est_faults <- get(paste(model,"MVF_cont",sep="_"))(params,tn+t_offset) - get(paste(model,"MVF_cont",sep="_"))(params,tn)
	
  # est_faults is an expected value, so it may not be a
  # whole number.  Nevertheless, we return the unaltered
  # value.
  
  return(est_faults)
}


get_prediction_t <- function(model, params, faults, tn, n){
        
	time_indexes <- c()
	if(faults!=0){
		for(i in 1: faults){
			est_faults <- get(paste(model,"MVF_cont",sep="_"))(params,tn)

				tn_n <- try(est_t(model,params,tn,i),silent=TRUE)
				
				if((typeof(tn_n)=="double") && (tn_n >= 0)){
					t <- tn_n - tn
					time_indexes[i] <- t
				}
				else{
					time_indexes[i] <- "NA"
				}
		}
	}
	time_indexes
}

# Predict the additional testing time that will be required
# to achieve the target reliability.

get_reliability_t <- function(model, params, targetRel, missionTime, tn, numFails) {
  if ((targetRel > 0) && (targetRel < 1) && (missionTime > 0)) {
    relTime <- get(paste(model,"Target_T",sep="_"))(params,tn,missionTime,targetRel)
    if(!is.null(relTime) && !is.na(relTime)) {
      if(is.numeric(relTime)) {
        if (is.finite(relTime)) {
          if(relTime > tn) {
            relTime <- relTime-tn
          } else {
            relTime <- paste("R =", as.character(targetRel), "achieved")
          }
        }
      } else if (relTime == "Target reliability already achieved") {
        relTime <- paste("R =", as.character(targetRel), "achieved")
      } else {
        relTime <- NA
      }
    } else {
      relTime <- NA
    }
  } else {
    if(targetRel < 0) {
      relTime <- NA
    } else if(targetRel == 0) {
      relTime <- "Reliability target of 0 achieved"
    } else if (targetRel >= 1) {
      relTime <- Inf
    } else if(missionTime <= 0) {
        relTime <- "Choose a mission length > 0"
    }
  }
  return(relTime)
}


est_t <- function(model,params,tn,steps){
	est_faults <- get(paste(model,"MVF_cont",sep="_"))(params,tn) # ? ----> Should use floor or not

	est_time_root <- function(tn){
		return (get(paste(model,"MVF_cont",sep="_"))(params, tn) -(est_faults+steps))
	}
	# sol <- uniroot(est_time_root, c(tn,tn+50), extendInt="yes", maxiter=1000, tol=1e-10)$root
	
	# Bound the estimation interval
	
	sol <- 0
	interval_left <- tn
	interval_right <- 2*interval_left
	local_MVF <- get(paste(model,"MVF_cont",sep="_"))(params,interval_right)
	while (local_MVF <= (est_faults+steps)) {
	  interval_right <- 2*interval_right
	  if(local_MVF == (est_faults+steps)) {
	    interval_right <- 2.25*interval_right
	  }
	  if (is.infinite(interval_right)) {
	    break
	  }
	  local_MVF <- get(paste(model,"MVF_cont",sep="_"))(params,interval_right)
	}
	if(is.finite(interval_right)) {
	  while (get(paste(model,"MVF_cont",sep="_"))(params,(interval_left + (interval_right-interval_left)/2)) < (est_faults+steps)) {
	    interval_left <- interval_left + (interval_right-interval_left)/2
	  }
	} else {
	  sol <- Inf
	}
	
	maxiter <- 10000
	
	if(is.finite(interval_right) && is.finite(sol)) {
	  sol <- tryCatch(
	    stats::uniroot(est_time_root, c(interval_left,interval_right),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
	    warning = function(w){
	      ##print(f.lower)
	      if(length(grep("_NOT_ converged",w[1]))>0){
	        maxiter <<- maxiter+10
	        #print(paste("recursive", maxiter,sep='_'))
	        est_t(model,params,tn)
	      }
	    },
	    error = function(e){
	      return("NA")
	      #return(e)
	    })
	} else {
	  sol <- NA
	}
	
	##print("Estimated time for next failure")
  ##print(sol)
  sol
}

get_optimal_release_time_CC <- function(model, params, c1,c2,c3){
	return(get(paste(model,"OR_CC",sep="_"))(params,c1,c2,c3))
}

#Get the cost at a specified time
get_cost_at_time <- function(model, params, time, t_lifecycle, c1,c2,c3){
	if(is.numeric(time)){
		return(get(paste(model,"cost",sep="_"))(params,c1,c2,c3,time,t_lifecycle))
	}
	else{
		return(0)
	}
    
}

get_rel_at_opt_release_time <- function(model, params, opt_release_time, delta){
    return(get(paste(model,"R_delta",sep="_"))(params,opt_release_time,delta))
}

get_release_release_time_RC <- function(model, params){
	return(0)
}




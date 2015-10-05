# # Detailed prediction


get_prediction_k <- function(model,params, t_offset,tn,n){
	# ! -----> print(">>>>>>>>>")
	# ! -----> print(params)
	if("JM_N0" %in% names(params)){
		est_faults <- JM_MVF_cont(params,tn + t_offset) - JM_MVF_cont(params,tn)
	}
	else if("GM_D0" %in% names(params)){
		est_faults <- GM_MVF_cont(params,tn + t_offset) - GM_MVF_cont(params,tn)
	}
	else if("GO_aMLE" %in% names(params)){
		est_faults <- GO_MVF_cont(params,tn + t_offset) - GO_MVF_cont(params,tn)
	}
	else if("DSS_aMLE" %in% names(params)){
		est_faults <- DSS_MVF_cont(params,tn + t_offset) - DSS_MVF_cont(params,tn)
	}
	else if("Wei_aMLE" %in% names(params)){
		est_faults <- Wei_MVF_cont(params,tn + t_offset) - Wei_MVF_cont(params,tn)
	}
	else{
		est_faults <- "Not Implemented"
	}
  return(floor(est_faults))
}


get_prediction_t <- function(model, params, faults, tn, n){
	time_indexes <- c()
	if(faults!=0){
		for(i in 1: faults){
			est_faults <- get(paste(model,"MVF_cont",sep="_"))(params,tn)

			print("Estimated_faults")
			print(est_faults)
			# if(est_faults - n >= i){
				tn_n <- try(est_t(model,params,tn,i),silent=TRUE)
				print("_")
				print("_")
				print("_")
				print("_")
				print("_")
				print("_")
				print("_")
				
				print(tn_n)

				print("_")
				print("_")
				print("_")
				print("_")
				print("_")
				print("_")
				
				if(typeof(tn_n)=="double"){
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


est_t <- function(model,params,tn,steps){
	est_faults <- get(paste(model,"MVF_cont",sep="_"))(params,tn) # ? ----> Should use floor or not
	print("Estimated Faults:")
	print(est_faults)
	print("")
	print("")
	est_time_root <- function(tn){
		return (get(paste(model,"MVF_cont",sep="_"))(params, tn) -(est_faults+steps))
	}
	# sol <- uniroot(est_time_root, c(tn,tn+50), extendInt="yes", maxiter=1000, tol=1e-10)$root
	maxiter <- 10000
	sol <- tryCatch(
    			uniroot(est_time_root, c(tn,tn + 50),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
    			warning = function(w){
    #print(f.lower)
      				if(length(grep("_NOT_ converged",w[1]))>0){
        				maxiter <<- maxiter+10
        				print(paste("recursive", maxiter,sep='_'))
        				est_t(model,params,tn)
      				}
    			},
    			error = function(e){
      				return("NA")
      #return(e)
    			})
	print("Estimated time for next failure")
  	print(sol)
    sol
}





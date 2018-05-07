#Akaike Information Criterion (p -> number of model parameters and lnL -> log-likelihood value)

#AIC <- 2*p - 2*lnL

aic <- function(p,lnL){
	return (2*p - 2*lnL)
}

# psse_times <- function(model, d, model_params){
# 	t <- 0
# 	mvf_data <- get(paste(model,"MVF",sep="_"))(model_params, d)
# 	for(i in 1:length(d$FT)){
# 		t <- (d$FT[i] - mvf_data$Time[i])^2 + t
# 	}
	
# 	#print(paste("PSSE: ",t))
# 	t
# }


# psse_failures <- function(d,model_params){
#  	n <- length(data$FT)
#  	r <- data.frame()
#  	cumulr <- data.frame()
#  	cumulr[i,1] <- 0
#  	cumulr[i,2] <- 0
#  	for(i in 1:n){
#  		next_delta <- data$IF[i]
#  		r[i,1] <- i
#  		for(j in 1:next_delta){

#  		}
#  	}
# }


#PSSE

psse <- function(model, d, model_params,percent){
	
	t <- 0
	n <- length(d)
	k <- floor(percent*n)
	k <- max(k,1)
	failNums <- c((k+1):n)
	failTimes <- tail(d,length(failNums))
	t <- sum((failNums - get(paste(model,"_MVF_cont",sep=""))(model_params,failTimes))^2)
	# for( i in (k+1):n){
	#	 t <- (i - get(paste(model,"_MVF_cont",sep=""))(model_params,d[i]))^2 +t
	# }
	t
}

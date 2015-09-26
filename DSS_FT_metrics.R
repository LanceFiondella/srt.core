DSS_MVF <- function(aMLE,bMLE, data){
	return(aMLE*(1-(1+bMLE*data)*exp(-bMLE*data)))
}

DSS_failureIntensity <- function(aMLE,bMLE, data){
	return(aMLE*(bMLE^2)*exp(-bMLE*data)*data)
}

DSS_FT_lnL <- function(aMLE,bMLE,data){
	n <- length(data)
	tn <- data[n]
	sum1=0
	sum2=0
	for(i in 1:n){
		sum1 = sum1+log(data[i])
		sum2 = sum2+data[i]
	}
	tmp <- -(aMLE*(1-(1+bMLE*tn)*exp(-bMLE*tn)))+n*log(aMLE)+2*n*log(bMLE)+sum1-bMLE*sum2
	return(tmp)
}

DSS_FT_Reliability <- function(bMLE,data){
	return((1+bMLE*data)*exp(-bMLE*data))
}

DSS_FT_MTTF <- function(bMLE){
	return(2/bMLE)
}

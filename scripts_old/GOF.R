#Akaike Information Criterion (p -> number of model parameters and lnL -> log-likelihood value)

AIC <- 2*p - 2*lnL

#PSSE


#data1 is tVecHoldOut and data is tVec

PSSE <- function(n,data,data1){
	MVF_PSSE <- aMLE*(1-exp(-bMLE*data1))
	n=length(data)
	
	mtFitSum=0
	for(i in 1:length(data1)){
		mtFitSum= mtFitSum+(mtFitSum-(n+i))^2
	}	
	
}

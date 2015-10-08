#Mean Value Function
GO_BM_FT_MVF <- function(aMLE,bMLE,data){
		tmp <- aMLE*(1-exp(-bMLE*data))
		return(tmp)
}

#log-likelihood function--Failure times

GO_BM_FT_LLF <- function(aMLE,bMLE,data){
 		lnl <- -aMLE*(1-exp(-bMLE*tn))+n*log(aMLE)+n*log(bMLE)-bMLE*sum(data)
 		return(lnl)
}

#log-likelihood function--Failure counts

GO_BM_FC_LLF<-function(aMLE,bMLE,data){
ln1 <- sum(kVec*log(aMLE))
ln2 <- numeric(0)
ln3 <- aMLE*(1-exp(-bMLE*tn))

for(i in 1:n+1){
ln2[i-1] <- exp(-bMLE*tVec[i-1])-exp(-bMLE*tVec[i])
}

lnl <- ln1+sum(kVec*log(ln2))-ln3
}

#Estimated number fo faults remaining

GO_BM_FT_FaultsRemaining <- function(aMLE,n){
	FaultsRemaining <- abs(aMLE-n)
	return(FaultsRemaining)
}
#Reliability
GO_BM_FT_Reliability <- function(bMLE,tn){
	Reliability <- exp(-bMLE*tn)
	return(Reliability)
}
#MTTF
GO_BM_FT_MTTF <- function(bMLE){
	MTTF <- 1/bMLE
	return(MTTF)
	}
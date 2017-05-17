#rm(list=ls())

#kVec <- c(0,0,1,0,0,0,0,0,0,1,0,0,1,2,2,0,1,2,0,0,2,1,3,0,0,5,2,0,0,1,3,2,0,2,0,0,2,1,0,0,0,0,0,0,0,0,0,1,2,0,0,5,2,0,1,6,8,2,1,6,8,8,6,3,4,3,1,1,1,1,5,2,1,1,1,2,0,1,1,1,1,0,1,1,1,2,2,3,3,5,1,2,3,1,1,1,2,0,1,4,3,0,1,8,4,7,1,1,2,1,0,1,3,2)
#tVec <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114)

GO_BM_FC_MLE <- function(tVec, kVec){
	k <- length(kVec)
	K <- sum(kVec)

	tn <- tVec[length(tVec)]
	n<-k

	tVec <- c(0,tVec)

	#Define MLE of parameter 'b'

	MLEeq<-function(b){
		tmp_b <- numeric(0)
		x3 <- numeric(0)
		x4 <- numeric(0)
		x1 <- tn*exp(-b*tn)*sum(kVec)
		x2 <- 1-exp(-b*tn)
		rhs <- x1/x2
		
		for(j  in 1:n+1){
			x3[j-1] <- (tVec[j]*exp(-b*tVec[j]))-(tVec[j-1]*exp(-b*tVec[j-1]))
			x4[j-1] <- exp(-b*tVec[j-1])-exp(-b*tVec[j])	
		}
		lhs <- sum((kVec*x3)/x4)	
		tmp_b <- lhs-rhs
		return(tmp_b)
	}

	#Step-1: Determine initial parameter estimate for parameter 'b'

	b0 <- K/sum(tVec)

	#Step-2: Bracket root

	i <- 0 
	maxIterations <- 10
	leftEndPoint <- b0/2
	leftEndPointMLE <- MLEeq(leftEndPoint)
	rightEndPoint <- 2*b0
	rightEndPointMLE <- MLEeq(rightEndPoint)

	while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
		print('In Step 2 while loop of GO_BM_FC.R')
		#leftEndPoint <- leftEndPoint/2
		#leftEndPointMLE <- MLEeq(leftEndPoint)
		rightEndPoint <- 1.1*rightEndPoint
		rightEndPointMLE <- MLEeq(rightEndPoint)
		i <- i+1	
	}

	#Step-3: Invoke uniroot or report non convergence to calling environment

	#if(leftEndPointMLE*rightEndPointMLE > 0 ){
	#	return('nonconvergence')
	#} else {
		bMLE <- uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint,extendInt="yes", tol = 1e-10)$root
	#  return(bMLE)	
	#}

	print(bMLE)
	#Step-4
	#MLE of parameter 'a'
		aMLE <- sum(kVec)/(1-exp(-bMLE*tn))
		print(aMLE)
	sol <- data.frame("GO_aMLE"=aMLE,"GO_bMLE"=bMLE)
    # sol <- c(aMLE,bMLE)
    sol
}


#NHPP log-likelihood function

GO_FC_lnL <- function(kVec, tVec, params){
	aMLE <- params$GO_aMLE
	bMLE <- params$GO_bMLE
	ln1 <- sum(kVec*log(aMLE))
	ln2 <- numeric(0)
	ln3 <- aMLE*(1-exp(-bMLE*tn))
	for(i in 1:n+1){
	ln2[i-1] <- exp(-bMLE*tVec[i-1])-exp(-bMLE*tVec[i])
	}

	lnl <- ln1+sum(kVec*log(ln2))-ln3
	return(lnl)
}

#Mean Value function 
GO_MVF <- function(params, tVec){
	aMLE <- params$GO_aMLE
	bMLE <- params$GO_bMLE
	MVF <- aMLE*(1-exp(-bMLE*tVec))
	return(MVF)
}





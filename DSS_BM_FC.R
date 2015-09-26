kVec <- c(15, 29, 22, 37, 2, 5, 36, 29, 4, 27, 17, 32, 21, 22, 6, 7, 9, 5, 3)
k <- length(kVec)
K <- sum(kVec)

tVec <- c(2.45, 4.9, 6.86, 7.84, 9.52, 12.89, 17.1, 20.47, 21.43, 23.35, 26.23, 27.67, 30.93, 34.77, 38.61, 40.91, 42.67, 44.66, 47.65)

tn <- tVec[length(tVec)]


n<-length(tVec)

#Define MLE of parameter 'b'

MLEeq<-function(b){
	rhs=0
	
	for(i in 2:n){
		rhs=rhs + ((b*kVec[i]*(exp(-b*tVec[i])*tVec[i]^2 - exp(-b*tVec[i-1])*tVec[i-1]^2)) /  
		(exp(-b*tVec[i-1])-exp(-b*tVec[i]) + b*(exp(-b*tVec[i-1])*tVec[i-1]-exp(-b*tVec[i])*tVec[i])))	
	}
	
	kSum=0
	for(i in 1:n){
		kSum= kSum+kVec[i]
	}
	
	a <- kSum/(1-(1+b*tn)*exp(-b*tn))
	
	tmp_b <- (exp(-b*tn) / (exp(b*tVec[1])-1-b*tVec[1]))* (b*exp(b*tn)*kVec[1]*tVec[1]^2 + (1-exp(b*tVec[1])+b*tVec[1])*(a*b*tn^2-exp(b*tn)*rhs))
	
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
	print('In Step 2 while loop of DSS_BM_FC.R')   
	leftEndPoint <- leftEndPoint/2
	leftEndPointMLE <- MLEeq(leftEndPoint)
	rightEndPoint <- 1.1*rightEndPoint
	rightEndPointMLE <- MLEeq(rightEndPoint)
	i <- i+1	
}

#Step-3: Invoke uniroot or report non convergence to calling environment

if(leftEndPointMLE*rightEndPointMLE > 0 ){
	return('nonconvergence')
} else {
	bMLE <- uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint,extendInt="yes", tol = 1e-10)$root
	
}



print(bMLE)
#Step-4
#MLE of parameter 'a'

	kSum=0
	for(i in 1:n){
		kSum= kSum+kVec[i]
	}
	
	aMLE <- kSum/(1-(1+bMLE*tn)*exp(-bMLE*tn))
	
	 print(aMLE)
	 

#Mean Value function 

MVF <- aMLE*(1-(1+bMLE*tVec)*exp(-bMLE*tVec))



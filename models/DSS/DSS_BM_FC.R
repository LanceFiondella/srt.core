#rm(list=ls())

#kVec <- c(10,64,18,43,44,13,34,28,15,10)
#tVec <- c(1,1.5,2,3,4.5,6,8,11,12,13)

DSS_BM_FC_MLE <- function(tVec, kVec){
		k <- length(kVec)
		K <- sum(kVec)

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
	sol <- data.frame("DSS_aMLE"=aMLE,"DSS_bMLE"=bMLE)
    # sol <- c(aMLE,bMLE)
    sol


}	 

#Mean Value function 

#MVF <- aMLE*(1-(1+bMLE*tVec)*exp(-bMLE*tVec))




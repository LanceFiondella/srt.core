kVec <- c(15, 29, 22, 37, 2, 5, 36, 29, 4, 27, 17, 32, 21, 22, 6, 7, 9, 5, 3)
k <- length(kVec)
K <- sum(kVec)

tVec <- c(2.45, 4.9, 6.86, 7.84, 9.52, 12.89, 17.1, 20.47, 21.43, 23.35, 26.23, 27.67, 30.93, 34.77, 38.61, 40.91, 42.67, 44.66, 47.65)

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
  #print('In Step 2 while loop of GO_BM_FC.R')
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

#print(bMLE)
#Step-4
#MLE of parameter 'a'
aMLE <- sum(kVec)/(1-exp(-bMLE*tn))
#print(aMLE)

#NHPP log-likelihood function


ln1 <- sum(kVec*log(aMLE))
ln2 <- numeric(0)
ln3 <- aMLE*(1-exp(-bMLE*tn))
for(i in 1:n+1){
  ln2[i-1] <- exp(-bMLE*tVec[i-1])-exp(-bMLE*tVec[i])
}

lnl <- ln1+sum(kVec*log(ln2))-ln3

#Mean Value function 

MVF <- aMLE*(1-exp(-bMLE*tVec))



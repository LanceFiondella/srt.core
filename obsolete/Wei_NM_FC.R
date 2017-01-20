#Vector of failure times data
library(rootSolve)
rm(list=ls())
kVec <- c(15, 29, 22, 37, 2, 5, 36, 29, 4, 27, 17, 32, 21, 22, 6, 7, 9, 5, 3)
tVec <- c(2.45, 4.9, 6.86, 7.84, 9.52, 12.89, 17.1, 20.47, 21.43, 23.35, 26.23,27.67, 30.93, 34.77, 38.61, 40.91, 42.67, 44.66, 47.65)
#Define n, tn and sumT

#tVec <- c(0,tVec)
n <- length(kVec)



tn <- tVec[length(tVec)]
sumT <- sum(tVec)
sumK <- sum(kVec)

#estimate starting point for 'b'
b0 <- n/sumT

MLEeq<-function(b){
  sumil = 0
  sumil1=0
  for(i in 2:n)
  {
    sumil= sumil + ((((exp(-b*tVec[i])*tVec[i])-(exp(-b*tVec[i-1])*tVec[i-1]))*kVec[i])/(exp(-b*tVec[i-1])-exp(-b*tVec[i])))    
  }
  for(j in 1:n)
  {
  	sumil1 = sumil1 + kVec[j]
  }

  b_MLE <- ((kVec[1]*tVec[1])/(-1+exp(b*tVec[1]))) - (exp(-b*tn)*tn*sumil1) + sumil
  
  return(b_MLE)
}

i <- 0 
maxIterations <- 200
leftEndPoint <- b0/2
leftEndPointMLE <- MLEeq(leftEndPoint)
rightEndPoint <- 2*b0
rightEndPointMLE <- MLEeq(rightEndPoint)

while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
  leftEndPoint <- leftEndPoint/2
  leftEndPointMLE <- MLEeq(leftEndPoint)
  rightEndPoint <- 2*rightEndPoint
  rightEndPointMLE <- MLEeq(rightEndPoint)
  i <- i+1	
}

if(leftEndPointMLE*rightEndPointMLE > 0 ){
  return('nonconvergence')
} else {
  b_initial <- uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, extendInt="yes", tol = 1e-10)$root
}


b0 <- b_initial

a0 <- sumK
c0 <-  1.0


#(*MLE equation of x[1]*)
model <- function(x) {
  sumi <- c(0,0,0)
  for(i in 1:n)
  {
  	sumi[1] <- sumi[1] + (kVec[i]/x[1])   
  }
  for(i in 2:n)
  {
    sumi[2] <- sumi[2] + ((((exp(-x[2]*tVec[i]^x[3])*tVec[i]^x[3])-(exp(-x[2]*tVec[i-1]^x[3])*tVec[i-1]^x[3]))*kVec[i])/(exp(-x[2]*tVec[i-1]^x[3])-exp(-x[2]*tVec[i]^x[3])))
    sumi[3] <- sumi[3] + ((x[2]*(exp(-x[2]*tVec[i]^x[3])*tVec[i]^x[3] *log(tVec[i]) -  exp(-x[2]*tVec[i-1]^x[3])*tVec[i-1]^x[3] *log(tVec[i-1]))*kVec[i])/(exp(-x[2]*tVec[i-1]^x[3])-exp(-x[2]*tVec[i]^x[3])))
  }  
  
  c(F1 =(-1+exp(-x[2]*tn^x[3]))+sumi[1],
    F2 = (-x[1]*exp(-x[2]*tn^x[3])*tn^x[3]) + ((kVec[1]*tVec[1]^x[3])/(exp(x[2]*tVec[1]^x[3])-1)) + sumi[2],
    F3= (-x[1]*x[2]*exp(-x[2]*tn^x[3])*tn^x[3]*log(tn)) + (x[2]*log(tVec[1])*kVec[1]*tVec[1]^x[3])/(-1+exp(x[2]*tVec[1]^x[3]))+sumi[3])
}
abc <- multiroot(f=model,start=c(a0,b0,c0),maxiter = 10000,ctol = 1e-24)$root



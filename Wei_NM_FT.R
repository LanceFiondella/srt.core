#Vector of failure times data
library(rootSolve)
rm(list=ls())
tVec <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)
#Define n, tn and sumT
n <- length(tVec)
tn <- tVec[n]
sumT <- sum(tVec)

#estimate starting point for 'b'
b0 <- (n/sumT)

MLEeq<-function(b){
  c <- 1.0
  sumi = 0
  for(i in 1:n)
  {
    sumi= sumi + (1/b) - ((tVec[i]))    
  }

  b_MLE <- (((-n*(tn))/(exp(b*(tn))-1)) + sumi)
  
  return(b_MLE)
}

i <- 0 
maxIterations <- 200
leftEndPoint <- b0
leftEndPointMLE <- MLEeq(leftEndPoint)
rightEndPoint <- 2*b0
rightEndPointMLE <- MLEeq(rightEndPoint)

while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
  print('In Step 2 while loop of Wei_BM.R')
  leftEndPoint <- leftEndPoint/2
  leftEndPointMLE <- MLEeq(leftEndPoint)
  rightEndPoint <- 2*rightEndPoint
  rightEndPointMLE <- MLEeq(rightEndPoint)
  i <- i+1	
}

if(leftEndPointMLE*rightEndPointMLE > 0 ){
  return('nonconvergence')
} else {
  b_initial <- stats::uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, extendInt="yes", tol = 1e-10)$root
}
print(b_initial)


b0 <- b_initial

#Estimate starting point for 'c'
c0 <- 1.0
a0 <- n

#(*MLE equation of x[1]*)

model1 <- function(x) {
  sumi <- c(0,0)
  for(i in 1:n)
  {
    sumi[1] <- sumi[1] + (1/x[2]) - ((tVec[i])^x[3])   
    sumi[2] <- sumi[2] + (1/x[3]) - (((tVec[i])^x[3])*log(tVec[i])*x[2]) + (log(tVec[i]))      #calculating the values for the summation 
  }
  #print(x)

  c(F1 = -1 + exp(-x[2]*(tn^x[3])) + (n/x[1]),
    F2 = (-x[1]*(tn^x[3])*exp(-x[2]*(tn^x[3]))) + sumi[1],
    F3 = (-x[2]*x[1]*(tn^x[3])*exp(-x[2]*(tn^x[3]))*log(tn)) + sumi[2])
}
abc <- rootSolve::multiroot(f=model1,start=c(a0,b0,c0), ctol = 1e-24)$root
print(abc)


#Vector of failure times data
x <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)

#Define n, tn and sumT
n <- length(x)
tn <- x[n]
sumT <- sum(x)

#Define MLE of parameter 'b'
MLEeq<-function(b){
  (2/b)-((b*tn^2)/(exp(b*tn)-1-b*tn))-((sum(x))/(n)) 
}

#Step-1: Determine initial parameter estimate for parameter 'b'

b0 <- n/sumT

#Step-2: Bracket root

i <- 0 
maxIterations <- 10
leftEndPoint <- b0/2
leftEndPointMLE <- MLEeq(leftEndPoint)
rightEndPoint <- 2*b0
rightEndPointMLE <- MLEeq(rightEndPoint)

while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
  print('In Step 2 while loop of DSS_BM_FT.R')
  leftEndPoint <- leftEndPoint/2
  leftEndPointMLE <- MLEeq(leftEndPoint)
  rightEndPoint <- 2*rightEndPoint
  rightEndPointMLE <- MLEeq(rightEndPoint)
  i <- i+1	
}

#Step-3: Invoke uniroot or report non convergence to calling environment

if(leftEndPointMLE*rightEndPointMLE > 0 ){
  return('nonconvergence')
} else {
  bMLE <- uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, tol = 1e-10)$root
  #bMLE <- uniroot(MLEeq,c(leftEndPoint,rightEndPoint))$root
}

print(bMLE)
#Step-4
#MLE of parameter 'a'
aMLE <- n/(1-exp(-bMLE*tn)*(1+bMLE*tn))
print(aMLE)

#NHPP log-likelihood function

lnL <- -aMLE*(1-(1+bMLE*tn)*exp(-bMLE*tn))+n*log(aMLE)+2*n*log(bMLE)+sum(log(x))-bMLE*sum(x)
print(lnL)

#Mean Value function

MVF <- aMLE*(1-(1+bMLE*x)*exp(-bMLE*x))
print(MVF)

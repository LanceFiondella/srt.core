#x <- c(3, 33, 146, 227, 342, 351, 353, 444,556, 571, 709, 759, 836, 860, 968,1056, 1726, 1846, 1872, 1986, 2311,2366, 2608, 2676, 3098, 3278, 3288,4434, 5034, 5049, 5085, 5089, 5089,5097, 5324, 5389, 5565, 5623, 6080,6380, 6477, 6740, 7192, 7447, 7644,7837, 7843, 7922, 8738, 10089, 10237,10258, 10491, 10625, 10982, 11175,11411, 11442, 11811, 12559, 12559,12791, 13121, 13486, 14708, 15251,15261, 15277, 15806, 16185, 16229,16358, 17168, 17458, 17758, 18287,18568, 18728, 19556, 20567, 21012,21308, 23063, 24127, 25910, 26770,27753, 28460, 28493, 29361, 30085,32408, 35338, 36799, 37642, 37654,37915, 39715, 40580, 42015, 42045,42188, 42296, 42296, 45406, 46653,47596, 48296, 49171, 49416, 50145,52042, 52489, 52875, 53321, 53443,54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732,64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)
#require("Rmpfr")
require("utils")
#x is interfailure times
JM_BM_MLE<-function(interFail){
n <- length(interFail)

#Define MLE of parameter 'N0'

MLEeq<-function(N0){
leftTerm = 0
interFailSum = 0
rightTermDenominator = 0
for(i in 1:n){
	leftTerm =leftTerm+(1/(N0-(i-1)))
	interFailSum = interFailSum + interFail[i]
	rightTermDenominator = rightTermDenominator+((N0-(i-1))*interFail[i])
}
N0_MLE <- leftTerm-((n* interFailSum)/rightTermDenominator)
return(N0_MLE)
}

#Step-1: Determine initial parameter estimate for parameter 'b0'

#b0 <- n/sum(interFail)
b0 <- n
##print(paste("b): ",b0))
#Step-2: Bracket root

i <- 0 
maxIterations <- 50
leftEndPoint <- b0
leftEndPointMLE <- MLEeq(leftEndPoint)
rightEndPoint <- 1.1*b0
rightEndPointMLE <- MLEeq(rightEndPoint)

##print(paste("left:",leftEndPointMLE))
##print(paste("right:",rightEndPointMLE))

while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
	##print('In Step 2 while loop of JM_BM.R')
	#leftEndPoint <- leftEndPoint/2
	#leftEndPointMLE <- MLEeq(leftEndPoint)
	rightEndPoint <- 1.1*rightEndPoint
	rightEndPointMLE <- MLEeq(rightEndPoint)
	i <- i+1	
}

#Step-3: Invoke uniroot or report non convergence to calling environment

if(leftEndPointMLE*rightEndPointMLE > 0 ){
	return('nonconvergence')
} else {
	#N0_MLE <- unirootR(MLEeq,interval=c(mpfr(leftEndPoint,300),mpfr(rightEndPoint,300)),tol=1e-20)$root
	N0_MLE <- uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, extendInt="yes", tol = 1e-24)$root
	#N0_MLE <- uniroot(MLEeq,lower=mpfr(leftEndPoint,300),upper=mpfr(rightEndPoint,300), tol = 1e-40)$root
}
##print(N0_MLE)

#Step-4
#MLE of parameter '\phi'
tmp_phi <- numeric(0)
for(i in 1:n-1){
	tmp_phi[i] <- (N0_MLE-(i-1))*interFail[i]
}
phi <- n/sum(tmp_phi)

#         a     b
return(c(N0_MLE,phi))
}
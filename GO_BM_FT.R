#Vector of failure times data
# x <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)

#Define n, tn and sumT

GO_BM_MLE <- function(x){
n <- length(x)
tn <- x[n]
sumT <- sum(x)
#print("GO_BM_R\n")
#Define MLE of parameter 'b'
GO_MLEeq<-function(b){
	((n*tn*exp(-b*tn))/(1-exp(- b*tn)))+sumT - n/b 
}

#Step-1: Determine initial parameter estimate for parameter 'b'

b0 <- n/sumT

#Step-2: Bracket root

i <- 0 
maxIterations <- 100
leftEndPoint <- b0/2
leftEndPointMLE <- GO_MLEeq(leftEndPoint)
rightEndPoint <- 1.2*b0
rightEndPointMLE <- GO_MLEeq(rightEndPoint)

while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
	#print('In Step 2 while loop of GO_BM_FT.R')
	leftEndPoint <- leftEndPoint/2
	leftEndPointMLE <- GO_MLEeq(leftEndPoint)
	rightEndPoint <- 2*rightEndPoint
	rightEndPointMLE <- GO_MLEeq(rightEndPoint)
	i <- i+1	
}

#Step-3: Invoke uniroot or report non convergence to calling environment

if(leftEndPointMLE*rightEndPointMLE > 0 ){
	return('nonconvergence')
} else {

maxiter <<- 20
  soln <- function(maxiter){
    sol <- tryCatch(
      uniroot(GO_MLEeq, c(leftEndPoint,rightEndPoint), maxiter=maxiter, tol=1e-10, extendInt="yes")$root,
      warning = function(w){
      #print(f.lower)
        if(length(grep("_NOT_ converged",w[1]))>0){
          maxiter <<- maxiter+1 
          #print(paste("recursive", maxiter,sep='_'))
          soln(maxiter)
        }
      },
      error = function(e){
        #print(e)
        #return(e)
      })
    sol
  }
  bMLE <- soln(maxiter)




	#bMLE <- uniroot(GO_MLEeq,lower=leftEndPoint,upper=rightEndPoint, tol = 1e-10, maxiter=2000)$root
	#bMLE <- uniroot(GO_MLEeq,c(leftEndPoint,rightEndPoint))$root
}

#print(bMLE)
#Step-4
#MLE of parameter 'a'
	 aMLE <- n/(1-exp(-bMLE*(tn)))
	 #print(aMLE)

	 sol <- c(aMLE,bMLE)

	 sol
}

GO_MVF_er <- function(param,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
    r[i,2] <- 1/(param$aMLE*(param$N0-(i-1)))
    cumulr[i,1] <- i
    cumulr[i,2] <- 0    
    for(j in 1:length(r[[1]])){
      cumulr[i,2] <- cumulr[i,2]+r[j,2]
    }
  }
  g <- data.frame(cumulr[2],cumulr[1])
  names(g) <- c("Time","Failure")
  g
}

# GO_MVF_er <- function(param,d){
#   n <- length(d$FT)
#   r <-data.frame()
#   cumulr <-data.frame()
#   for(i in 1:n){
#     r[i,1] <- i
#     r[i,2] <- 1/(param$aMLE*(param$N0-(i-1)))
#     cumulr[i,1] <- i
#     cumulr[i,2] <- 0
#     for(j in 1:length(r[[1]])){
#       cumulr[i,2] <- cumulr[i,2]+r[j,2]
#     }
#   }
#   g <- data.frame(cumulr[2],cumulr[1])
#   names(g) <- c("Time","Failure")
#   g
# }
GO_BM_MVF <- function(param,d){
  #param$aMLE <- 100
  n <- length(d$FT)
  r <- data.frame()
  #print(param)
  #t_index <- seq(0,9000,1)
  # param$aMLE <- 142.8809
  # param$bMLE <- 3.420379e-05
  t_index <- seq(d$FT[1],d$FT[n],(d$FT[n]-d$FT[1])/100)
  for(i in 1:length(t_index)){
    r[i,1] <- t_index[i]
    r[i,2] <- param$aMLE*(1-exp(-1*t_index[i]*param$bMLE))
  }
  r <- data.frame(r[1],r[2])
  names(r) <- c("Time","Failure")
  r
}

GO_BM_MVF_alt1 <- function(param,d) {
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  for(i in 1:length(fail_number)){
    r[i,1] <- fail_number[i]
    r[i,2] <- -(log((param$aMLE-i)/param$aMLE))/param$bMLE
  }
  r <- data.frame(r[1],r[2])
  names(r) <- c("Failure","Time")
  r
}

# September 8, 2015
# Alternate method for computing interfailure times for GO model.
# Based on SMERFS Library Access manual, NSWCDD TR 84-371, Rev 3,
# September 1993.  Uses IF equations for NHPP times to failure
# model, Chapter 7, p 7-3.

GO_T_alt1 <- function(param,d) {
  n <- length(d$FT)
  r <- data.frame()
  r[1,1] <- 1
  r[1,2] <- ((d$FT[1])/(param$aMLE*(1-exp(-param$bMLE*d$FT[1]))))
  for(i in 2:n){
    r[i,1] <- i
    r[i,2] <- ((i*d$FT[i])/(param$aMLE*(1-exp(-param$bMLE*d$FT[i])))) - (((i-1)*d$FT[i-1])/(param$aMLE*(1-exp(-param$bMLE*d$FT[i-1]))))
  }
  r <- data.frame(r[1],r[2])
  names(r) <- c("Failure","Time")
  r
}


# Estimate and forecast failure intensities

GO_FR_alt1 <- function(param,d) {
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  for(i in 1:length(fail_number)){
    r[i,1] <- fail_number[i]
    r[i,2] <- param$aMLE*param$bMLE*exp(-param$bMLE*d$FT[i])
  }
  r <- data.frame(r[1],r[2])
  names(r) <- c("Failure","Time")
  r
}


#NHPP log-likelihood function

#lnl <- -aMLE*(1-exp(-bMLE*tn))+n*log(aMLE)+n*log(bMLE)-bMLE*sum(x)

#Mean Value function

#MVF <- aMLE*(1-exp(-bMLE*x))

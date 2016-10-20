#Vector of failure times data -- SYS1
#x <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)

GO_BM_MLE<-function(x){
  #Define n, tn and sumT
  n <- length(x)
  tn <- x[n]
  sumT <- sum(x)
  
  #Define MLE of parameter 'b'
  GO_BM_DDB<-function(b){
    ((n*tn*exp(-b*tn))/(1-exp(- b*tn)))+sumT - n/b 
  }
  
  
  
  #Step-1: Determine initial parameter estimate for parameter 'b'
  
  b0 <- n/sumT
  
  #Step-2: Bracket root
  
  i <- 0 
  maxIterations <- 10
  leftEndPoint <- b0/2
  leftEndPointMLE <- GO_BM_DDB(leftEndPoint)
  rightEndPoint <- 2*b0
  rightEndPointMLE <- GO_BM_DDB(rightEndPoint)
  
  while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
    #print('In Step 2 while loop of GO_BM_FT.R')
    leftEndPoint <- leftEndPoint/2
    leftEndPointMLE <- GO_BM_DDB(leftEndPoint)
    rightEndPoint <- 2*rightEndPoint
    rightEndPointMLE <- GO_BM_DDB(rightEndPoint)
    i <- i+1	
  }
  
  #Step-3: Invoke uniroot or report non convergence to calling environment
  f <- x
  if(leftEndPointMLE*rightEndPointMLE > 0 ){
    f[1] <- 0
    f[2] <- 0
   x <- f[1,(1:2)]
    f <- data.frame(x)
    return(x)
  } else {
    bMLE <- uniroot(GO_BM_DDB,lower=leftEndPoint,upper=rightEndPoint, tol = 1e-10)$root
  }
  
  ##print(bMLE)
  #Step-4
  #MLE of parameter 'a'
  aMLE <- n/(1-exp(-bMLE*(tn)))
  #	 #print(aMLE)
  f[1] <- aMLE
  f[2] <- bMLE
  x <- f[1:2]
  f <- data.frame(x)
  #print(x)
  return(x)
}	 
#NHPP log-likelihood function

#lnl  <- -aMLE*(1-exp(-bMLE*tn))+n*log(aMLE)+n*log(bMLE)-bMLE*sum(x)

#Mean Value function

#MVF <- aMLE*(1-exp(-bMLE*x))



#####Copied from master file

#Vector of failure times data -- SYS1
#x <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)

GO_BM_MLE<-function(x){
  #Define n, tn and sumT
  n <- length(x)
  tn <- x[n]
  sumT <- sum(x)
  
  #Define MLE of parameter 'b'
  GO_BM_DDB<-function(b){
    ((n*tn*exp(-b*tn))/(1-exp(- b*tn)))+sumT - n/b 
  }
  
  
  
  #Step-1: Determine initial parameter estimate for parameter 'b'
  
  b0 <- n/sumT
  
  #Step-2: Bracket root
  
  i <- 0 
  maxIterations <- 100
  leftEndPoint <- b0/2
  leftEndPointMLE <- GO_BM_DDB(leftEndPoint)
  rightEndPoint <- 2*b0
  rightEndPointMLE <- GO_BM_DDB(rightEndPoint)
  
  while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
    #print('In Step 2 while loop of GO_BM_FT.R')
    #leftEndPoint <- leftEndPoint/2
    #leftEndPointMLE <- GO_BM_DDB(leftEndPoint)
    rightEndPoint <- 2*rightEndPoint

    rightEndPointMLE <- GO_BM_DDB(rightEndPoint)
    i <- i+1	
  }
  
  #Step-3: Invoke uniroot or report non convergence to calling environment
  f <- x
  if(leftEndPointMLE*rightEndPointMLE > 0 ){
   #  f[1] <- 0
   #  f[2] <- 0
   # x <- f[1,(1:2)]
   #  f <- data.frame(x)
    return('nonconvergence')
  } else {
    bMLE <- uniroot(GO_BM_DDB,lower=leftEndPoint,upper=rightEndPoint, tol = 1e-10)$root
  }
  
  ##print(bMLE)
  #Step-4
  #MLE of parameter 'a'
  aMLE <- n/(1-exp(-bMLE*(tn)))
  #	 #print(aMLE)
  f[1] <- aMLE
  f[2] <- bMLE
  x <- f[1:2]
  f <- data.frame(x)
  ##print(x)
  return(x)
}	


# nnnnnnnnnnnneeddss modificati8on
GO_MVF <- function(param,d){
  n <- length(d$FT)
  r <- data.frame()
  t_index <- seq(d$FT[1],d$FT[n],(d$FT[n]-d$FT[1])/100)
  for(i in 1:length(t_index)){
    r[i,1] <- t_index[i]
    r[i,2] <- param$N0*(1-exp(-1*t_index[i]*param$Phi))
  }
  r <- data.frame(r[1],r[2])
  names(r) <- c("Time", "Failure")
  r
}
#NHPP log-likelihood function

GO_lnl  <- function(){
  t <- -aMLE*(1-exp(-bMLE*tn))+n*log(aMLE)+n*log(bMLE)-bMLE*sum(x)
  return t
}

#Mean Value function

#MVF <- aMLE*(1-exp(-bMLE*x))

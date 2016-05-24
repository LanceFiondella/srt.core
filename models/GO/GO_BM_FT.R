library(rootSolve)
#Vector of failure times data
# x <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)

#Define n, tn and sumT

GO_BM_MLE <- function(x){
  x <- as.numeric(x)
n <- length(x)
tn <- x[n]
sumT <- sum(as.numeric(x))
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

maxiter <- 20
  soln <- function(maxiter){
    sol <- tryCatch(
      stats::uniroot(GO_MLEeq, c(leftEndPoint,rightEndPoint), maxiter=maxiter, tol=1e-10, extendInt="yes")$root,
      warning = function(w){
      #print(f.lower)
        if(length(grep("_NOT_ converged",w[1]))>0){
          maxiter <<- maxiter+1 
          #print(paste("recursive", maxiter,sep='_'))
          soln(maxiter)
        }
      },
      error = function(e){
        print(e)
        #return(e)
      })
    sol
  }
  bMLE <- soln(maxiter)

	#bMLE <- stats::uniroot(GO_MLEeq,lower=leftEndPoint,upper=rightEndPoint, tol = 1e-10, maxiter=2000)$root
	#bMLE <- stats::uniroot(GO_MLEeq,c(leftEndPoint,rightEndPoint))$root
}

#print(bMLE)
#Step-4
#MLE of parameter 'a'
	 aMLE <- n/(1-exp(-bMLE*(tn)))
	 #print(aMLE)
   sol <- data.frame("GO_aMLE"=aMLE,"GO_bMLE"=bMLE)
	 # sol <- c(aMLE,bMLE)

	 sol
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

GO_MVF <- function(param,d) {
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  MVF <- param$GO_aMLE*(1-exp(-param$GO_bMLE*d$FT))
  r <- data.frame(MVF,d$FT,rep("GO", n))
  names(r) <- c("Failure","Time","Model")
  r
}


GO_MVF_inv <- function(param,d) {
  n <- length(d$FN)
  r <- data.frame()
  cumFailTimes <- -(log((param$GO_aMLE-d$FN)/param$GO_aMLE))/param$GO_bMLE
  r <- data.frame(d$FN,cumFailTimes, rep("GO", n))
  names(r) <- c("Failure","Time","Model")
  r
}


# September 8, 2015
# Alternate method for computing interfailure times for GO model.
# Based on SMERFS Library Access manual, NSWCDD TR 84-371, Rev 3,
# September 1993.  Uses IF equations for NHPP times to failure
# model, Chapter 7, p 7-3.
  
GO_MTTF <- function(param,d) {
  n <- length(d$FT)
  r <- data.frame()
  currentTimes <- utils::tail(d, length(d$FT)-1)
  prevTimes <- utils::head(d, length(d$FT)-1)
  currentFailNums <- c(2:n)
  prevFailNums <- c(1:(n-1))
  IFTimes <- ((currentFailNums*currentTimes$FT)/(param$GO_aMLE*(1-exp(-param$GO_bMLE*currentTimes$FT)))) - ((prevFailNums*prevTimes$FT)/(param$GO_aMLE*(1-exp(-param$GO_bMLE*prevTimes$FT))))
  #  r[1,1] <- 1
  #  r[1,2] <- ((d$FT[1])/(param$GO_aMLE*(1-exp(-param$GO_bMLE*d$FT[1]))))
  #  for(i in 2:n){
  #    r[i,1] <- i
  #    r[i,2] <- ((i*d$FT[i])/(param$GO_aMLE*(1-exp(-param$GO_bMLE*d$FT[i])))) - (((i-1)*d$FT[i-1])/(param$GO_aMLE*(1-exp(-param$GO_bMLE*d$FT[i-1]))))
  #  }
  r <- data.frame(c(1, currentFailNums), c(((d$FT[1])/(param$GO_aMLE*(1-exp(-param$GO_bMLE*d$FT[1])))), IFTimes), rep("GO", n))
  names(r) <- c("Failure_Number","MTTF","Model")
  r
}

    
#GO_MTTF <- function(params,d){
#  n <- length(d$FT)
#  r <-data.frame()
#  cumulr <-data.frame()
#  for(i in 1:n){
#    r[i,1] <- i
#    r[i,2] <-(1/(params$GO_aMLE*params$GO_bMLE*(exp(-params$GO_bMLE*d$FT[i]))))
#    r[i,3] <- "GO"
#    }
#  r <- data.frame(r[1],r[2],r[3])
#  names(r) <- c("Failure_Number","MTTF","Model")
#  r
#}

# Estimate and forecast failure intensities

GO_FI <- function(param,d) {
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  failIntensity <- param$GO_aMLE*param$GO_bMLE*exp(-param$GO_bMLE*d$FT)
  #  for(i in 1:length(fail_number)){
  #    r[i,1] <- fail_number[i]
  #    r[i,2] <- param$GO_aMLE*param$GO_bMLE*exp(-param$GO_bMLE*d$FT[i])
  #  }
  r <- data.frame(fail_number,failIntensity, rep("GO", n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r
}


#GO_FI <- function(params,d){
#  n <- length(d$FT)
#  r <-data.frame()
#  cumulr <-data.frame()
#  for(i in 1:n){
#    r[i,1] <- d$FT[i]
#    r[i,2] <- params$GO_aMLE*params$GO_bMLE*(exp(-params$GO_bMLE*d$FT[i]))
#    r[i,3] <- "GO"
#    }
#  r <- data.frame(r[1],r[2],r[3])
#  names(r) <- c("Failure_Count","Failure_Rate","Model")
#  r
#
#}



GO_R <- function(params,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- d$FT[i]
    r[i,2] <- exp(-params$GO_bMLE*d$FT[i])
    r[i,3] <- "GO"
  }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Time","Reliability","Model")
  r
}

GO_lnL <- function(x,params){
  n <- length(x)
  tn <- x[n]
  firstSumTerm <- 0
  for(i in 1:n){
    firstSumTerm = firstSumTerm + (-params$GO_bMLE*x[i])
  }
  lnL <- -(params$GO_aMLE)*(1-exp(-params$GO_bMLE*tn)) + n*(log(params$GO_aMLE)) +n*log(params$GO_bMLE) + firstSumTerm
  lnL
}

GO_MVF_cont <- function(params,t){
  return(params$GO_aMLE*(1-exp(-params$GO_bMLE*t)))
}

GO_R_delta <- function(params,cur_time,delta){
  return(exp(-(GO_MVF_cont(params,(cur_time+delta)) -GO_MVF_cont(params,cur_time))))
}

GO_R_MLE_root <- function(params,cur_time,delta, reliability){
  root_equation <- reliability - exp(params$GO_aMLE*(1-exp(-params$GO_bMLE*cur_time)) -params$GO_aMLE*(1-exp(-params$GO_bMLE*(cur_time+delta))))
  return(root_equation)
}

maxiter <- 1000
GO_Target_T <- function(params,cur_time,delta, reliability){

  f <- function(t){
    return(GO_R_MLE_root(params,t,delta, reliability))
  }

  current_rel <- GO_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- GO_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- GO_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (GO_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
        interval_left <- interval_left + (interval_right-interval_left)/2
      }
    } else {
      sol <- Inf
    }
    
    if (is.finite(interval_right) && is.finite(sol)) {
      sol <- tryCatch(
        stats::uniroot(f, c(interval_left, interval_right),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
        warning = function(w){
          #print(f.lower)
          if(length(grep("_NOT_ converged",w[1]))>0){
            maxiter <<- floor(maxiter*1.5)
            print(paste("recursive", maxiter,sep='_'))
            GO_Target_T(a,b,cur_time,delta, reliability)
          }
        },
        error = function(e){
          print(e)
          #return(e)
        })
    } else {
      sol <- Inf
    }
  } else {
    sol <- "Target reliability already achieved"
  }
    return(sol)
  }

GO_R_growth <- function(params,d,delta){  
  
  r <-data.frame()
  for(i in 1:length(d$FT)){   
    r[i,1] <- d$FT[i]
    temp <- GO_R_delta(params,d$FT[i],delta)
    #print(typeof(temp))
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
      r[i,3] <- "GO"
    }
    else{
      r[i,2] <- "NA"
      r[i,3] <- "GO"
    }     
  }
  g <- data.frame(r[1],r[2],r[3])
  names(g) <- c("Time","Reliability_Growth","Model")
  #print(g)
  g
}


#GO_R_growth <- function(params,cur_time,delta, reliability){  
#  
#  r <-data.frame()
#  tt_index <- seq(0,cur_time,cur_time/1000)
#  for(i in 1:length(tt_index)){   
#    r[i,1] <- tt_index[i]
#    temp <- GO_R_delta(params,tt_index[i],delta)
#    #print(typeof(temp))
#    if(typeof(temp) != typeof("character")){
#      r[i,2] <- temp
#      r[i,3] <- "GO"
#    }
#    else{
#      r[i,2] <- "NA"
#      r[i,3] <- "GO"
#    }     
#  }
#  g <- data.frame(r[1],r[2],r[3])
#  names(g) <- c("Time","Reliability_Growth","Model")
#  #print(g)
#  g
#  
#}


#NHPP log-likelihood function

#lnl <- -aMLE*(1-exp(-bMLE*tn))+n*log(aMLE)+n*log(bMLE)-bMLE*sum(x)

#Mean Value function

#MVF <- aMLE*(1-exp(-bMLE*x))
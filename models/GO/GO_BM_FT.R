library(rootSolve)

#Define n, tn and sumT

GO_BM_FT_MLE <- function(x){
  x <- as.numeric(x)
  n <- length(x)
  tn <- x[n]
  sumT <- sum(as.numeric(x))
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
    ##print('In Step 2 while loop of GO_BM_FT.R')
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
        ##print(f.lower)
          if(length(grep("_NOT_ converged",w[1]))>0){
            maxiter <<- maxiter+1 
            ##print(paste("recursive", maxiter,sep='_'))
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

  }

  ##print(bMLE)
  #Step-4
  #MLE of parameter 'a'
    aMLE <- n/(1-exp(-bMLE*(tn)))
    #print(aMLE)
    sol <- data.frame("GO_aMLE"=aMLE,"GO_bMLE"=bMLE)
    # sol <- c(aMLE,bMLE)
    sol
}

GO_FT_lnL <- function(params, x){
  n <- length(x)
  tn <- x[n]
  firstSumTerm <- 0
  for(i in 1:n){
    firstSumTerm = firstSumTerm + (-params$GO_bMLE*x[i])
  }
  lnL <- -(params$GO_aMLE)*(1-exp(-params$GO_bMLE*tn)) + n*(log(params$GO_aMLE)) +n*log(params$GO_bMLE) + firstSumTerm
  lnL
}

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
  #print(d$FN)
  cumFailTimes <- -(log((param$GO_aMLE-d$FN)/param$GO_aMLE))/param$GO_bMLE
  numPredPoints = floor(param$GO_aMLE) - (d$FN[1]-1) #Number of points to be predicted : floor(aMLE) - (number of samples)
  if(numPredPoints < n){
    cumFailTimes[is.na(cumFailTimes)] <- 0 #If there are NaNs in the frame, replace it with zeros
    cumFailTimes[numPredPoints:length(cumFailTimes)] <- max(cumFailTimes[1:numPredPoints])
    d$FN[numPredPoints:n] <- max(d$FN[1:numPredPoints])
    
    }
  r <- data.frame(d$FN,cumFailTimes, rep("GO", n))
  names(r) <- c("Failure","Time","Model")
  r
}

GO_MTTF <- function(params, d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
   r[i,2] <-(1/(params$GO_aMLE*params$GO_bMLE*(exp(-params$GO_bMLE*d$FT[i]))))
    r[i,3] <- "GO"
    }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Failure_Number","MTTF","Model")
  r
}

# Estimate and forecast failure intensities

GO_FI <- function(param, d) {
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  failIntensity <- param$GO_aMLE*param$GO_bMLE*exp(-param$GO_bMLE*d$FT)
  r <- data.frame(fail_number,failIntensity, rep("GO", n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r
}


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
          ##print(f.lower)
          if(length(grep("_NOT_ converged",w[1]))>0){
            maxiter <<- floor(maxiter*1.5)
            #print(paste("recursive", maxiter,sep='_'))
            GO_Target_T(a,b,cur_time,delta, reliability)
          }
        },
        error = function(e){
          #print(e)
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
    ##print(typeof(temp))
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
  ##print(g)
  g
}

GO_OR_CC<-function(params,c1,c2,c3){
  if (params$GO_aMLE*params$GO_bMLE<=(c3/(c2-c1))){
    t_opt=0
  }
  else{
    t_opt=(1/params$GO_bMLE)*log((params$GO_aMLE*params$GO_bMLE*(c2-c1))/c3)
  }
  return(t_opt)
}

GO_OR_RC<-function(params,x,R){
  s=(1/params$GO_bMLE)*((log(params$GO_aMLE*(1-exp(-params$GO_bMLE*x))))-log(log(1/R)))
  return (s)
}

GO_cost <- function(params, C1,C2,C3,t, t_lifecycle){
  return(C1*GO_MVF_cont(params,t) + C2*(GO_MVF_cont(params,t_lifecycle) - GO_MVF_cont(params,t)) + C3*t)
}


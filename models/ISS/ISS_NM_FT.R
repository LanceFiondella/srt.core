library(rootSolve)

ISS_NM_MLE <- function(tVec){
  tVec <- as.numeric(tVec)
  n <- length(tVec)
  tn <- tVec[n]
  sumT <- sum(tVec)
  
  #estimate starting point for 'b'
  b0 <- (n/sumT)
  c0 <- 1
  a0 <- n
  
  #(*MLE equation of a, b, and c*)
  
  #model1 routine will have the MLE equations defined as F1, F2, and F3. a,b,c are denoted as x[1],x[2], and x[3] here
  
  model1 <- function(x) {
    sumi <- c(0,0)
    for(i in 1:n)
    {
      sumi[1] <- sumi[1] + (1/x[2])-((2*tVec[i]*exp(x[2]*tVec[i]))/(x[3]+exp(x[2]*tVec[i])))+tVec[i]  
      sumi[2] <- sumi[2] + (-2/(x[3]+exp(x[2]*tVec[i])))+(1/(1+x[3]))
    }
    #print(x)
    
    c(F1 = ((1-exp(x[2]*tn)+(((x[3]+exp(x[2]*tn))*n)/x[1]))/(x[3]+exp(x[2]*tn))),
      F2 = ((-x[1]*(1+x[3])*tn*exp(x[2]*tn))/((x[3]+exp(x[2]*tn))^2)) + sumi[1],
      F3 = (x[1]*(-1+exp(x[2]*tn))/((x[3]+exp(x[2]*tn))^2)) + sumi[2])
  }
  
  abc <- tryCatch(
    rootSolve::multiroot(f=model1,start=c(a0,b0,c0), ctol = 1e-10)$root,
    warning = function(w){
      ##print(f.lower)
      #print(w)
      return('nonconvergence')
    },
    error = function(e){
      #print(e)
      return("nonconvergence")
    })
  #param <- data.frame(abc[1],abc[2],abc[3])
  #names(param) <- c(paste("ISS",ISS_param[1],sep="_"),paste("ISS",ISS_param[2],sep="_"),paste("ISS",ISS_param[3],sep="_"))
  param <- data.frame("ISS_aMLE"=abc[1],"ISS_bMLE"=abc[2],"ISS_cMLE"=abc[3])
  param
}

ISS_MVF <- function(param, d){ 
  n <- length(d$FT)
  r <- data.frame()
  MVF <- param$ISS_aMLE*(1-exp(-param$ISS_bMLE*d$FT))/(1+param$ISS_cMLE*exp(-param$ISS_bMLE*d$FT))
  r <- data.frame(MVF,d$FT,rep("ISS", n))
  names(r) <- c("Failure","Time","Model")
  return(r)
}

ISS_MVF_inv <- function(param, d ){ 
  n <- length(d$FN)
  r <- data.frame()
  cumFailTimes <- -(log((param$ISS_aMLE-d$FN)/(param$ISS_aMLE+param$ISS_cMLE*d$FN)))/param$ISS_bMLE
  numPredPoints = floor(param$ISS_aMLE) - (d$FN[1]-1) #Number of points to be predicted : floor(aMLE) - (number of samples)
  print(param$ISS_aMLE)
  print(d$FN[1])
  
  if(numPredPoints < n){
    cumFailTimes[is.na(cumFailTimes)] <- 0 #If there are NaNs in the frame, replace it with zeros
    cumFailTimes[numPredPoints:length(cumFailTimes)] <- max(cumFailTimes[1:numPredPoints]) #Replace the end of the frame with the max of cumulative times
  }
  
  r <- data.frame(d$FN,cumFailTimes,rep("ISS", n))
  names(r) <- c("Failure","Time","Model")
  r
}

ISS_FI <- function(param,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- d$FT[i]
    r[i,2] <- (param$ISS_aMLE*param$ISS_bMLE*(param$ISS_cMLE + 1)*exp(param$ISS_bMLE*d$FT[i]))/((param$ISS_cMLE + exp(param$ISS_bMLE*d$FT[i]))^2)
    r[i,3] <- "ISS"
  }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r
}

ISS_MTTF <- function(param,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
    r[i,2] <- 1/((param$ISS_aMLE*param$ISS_bMLE*(1+param$ISS_cMLE)*exp(param$ISS_bMLE*d$FT[i]))/((param$ISS_cMLE+exp(param$ISS_bMLE*d$FT[i]))^2))
    r[i,3] <- "ISS"
  }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Failure_Number","MTTF","Model")
  r
}

ISS_lnL <- function(x,param){
  n <- length(x)
  tn <- x[n]
  sum1 <- 0
  for(i in 1:n){
    sum1=sum1+ (log(param$ISS_aMLE*param$ISS_bMLE*(param$ISS_cMLE + 1)*exp(param$ISS_bMLE*x[i]))/((param$ISS_cMLE + exp(param$ISS_bMLE*x[i]))^2))
  }
  return((-param$ISS_aMLE*(1-exp(-param$ISS_bMLE*tn))/(1+param$ISS_cMLE*exp(-param$ISS_bMLE*tn)))+ sum1)
}



ISS_MVF_cont <- function(param,t){
  return((param$ISS_aMLE)*((1-exp(-param$ISS_bMLE*t))/(1+param$ISS_cMLE*exp(-param$ISS_bMLE*t))))
}

ISS_R_delta <- function(params,cur_time,delta){
  return(exp(-(ISS_MVF_cont(params,(cur_time+delta)) - ISS_MVF_cont(params,cur_time))))
}

ISS_R_MLE_root <- function(params,cur_time,delta, reliability){
  root_equation <- reliability - ISS_R_delta(params,cur_time,delta)
  return(root_equation)
}

ISS_Target_T <- function(param,cur_time,delta,reliability){
  
  f <- function(t){
    return(ISS_R_MLE_root(param,t,delta,reliability))
  }
  
  current_rel <- ISS_R_delta(param,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- ISS_R_delta(param,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- ISS_R_delta(param,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (ISS_R_delta(param,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
        interval_left <- interval_left + (interval_right-interval_left)/2
      }
    } else {
      sol <- Inf
    }
    
    if (is.finite(interval_right) && is.finite(sol)) {
      sol <- tryCatch(
        stats::uniroot(f, c(cur_time,cur_time + 50),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
        warning = function(w){
          #print(f.lower)
          if(length(grep("_NOT_ converged",w[1]))>0){
            maxiter <<- floor(maxiter*1.5)
            #print(paste("recursive", maxiter,sep='_'))
            ISS_Target_T(a,b,cur_time,delta, reliability)
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

ISS_R_growth <- function(params,d,delta){  
  
  r <-data.frame()
  for(i in 1:length(d$FT)){   
    r[i,1] <- d$FT[i]
    temp <- ISS_R_delta(params,d$FT[i],delta)
    ##print(typeof(temp))
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
    }
    else{
      r[i,2] <- "NA"
    }     
  }
  g <- data.frame(r[1],r[2],rep("ISS", length(d$FT)))
  names(g) <- c("Time","Reliability_Growth","Model")
  g
  
}

ISS_R <- function(){
  # TODO
}

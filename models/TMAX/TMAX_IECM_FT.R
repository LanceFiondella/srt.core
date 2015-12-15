require("utils")
# x is interfailure times
TMAX_IECM_MLE<-function(interFail){
  # ---------------------------------------------------------------------------------
  # Maximumum likelihood estimation method is used to estimate the parameters 
  # 'N0' and 'Phi'. The initial estimate leftinterval and right interval are expanded
  # untill the zero of the equation is bracketed. The bisection method is used for 
  # bracketing the zero of equation hence the name BM in the name of function with 
  # '_' seperator. The zero of the expression is the maximum likelihood estimation of
  # N0 and the corresponding Phi is derived.
  #----------------------------------------------------------------------------------
  #
  # @params   : interFail  (list)       Input interfailure vector
  #
  # @returns  : TMAX_params  (data.frame) Dataframe of 'N0' and 'Phi'  
  #             TMAX_params$N0 refers to N0 and TMAX_params$Phi refers to Phi 
  #==================================================================================
  
  interFail <- as.numeric(interFail)
  n <- length(interFail)

  tVec <- as.numeric(interFail)
  n <- length(interFail)
  tn <- tVec[n]
  sumT <- sum(tVec)  
  
  #********************************************
  #
  #   a= Omega , b=Mu , c=theta 
  #
  #*******************************************
  
  #Initial Values 
  a0 <- 1
  b0 <- -18.93490427655104
  c0 <- 20324.05191128935
  
  #Log-likelihood
  LNLa <- function(a0,b0,c0)
  {
    a <- a0
    b <- b0
    c <- c0
    sumln <- 0
    for(i in 1:n)
    {
      sumln = sumln + (-exp((b-tVec[i])/c))+(b-tVec[i])/c+log(a) - log(c-exp(-exp(b/c))*c)
    }
    
    lnla <- (-a)*exp(-exp((b-tn)/c))/(1-exp(-exp(b/c)))  + sumln
    return(lnla)
  }
  #Reduced Log Likelihood
  
  #MLE of Parameter w
  MLE_a <- function(a0)
  {
    a <- a0
    b <- b0
    c <- c0
    
    mle_a <- -(exp(-exp(((-tn + b)/c)))/(1 - exp(-exp((b/c))))) + (n/a)
    return(mle_a)  
  }
  b <- b0
  c <- c0
 
  LNL <- function(b0,c0)
  {
    #a <- aa
    b <- b0
    c <- c0
    sumL <- 0
    for(i in 1:n)
    {
      sumL = sumL + exp((b-tn)/c) - exp((b-tVec[i])/c) + ((b-tVec[i])/c) + log(1-exp(-exp(b/c))) + log(n) - log(c-exp(-exp(b/c))*c)
    }
    
    lnl <- -n + sumL
    return(lnl)
    
  }
  
  #aa <- stats::uniroot(MLE_a,c(1,200),maxiter=1e10,tol=1e-10,extendInt ="yes")$root
  #print(aa)
  
  #MLE of Parameter Mu
  MLE_b <- function(b0)
  {
    
    b <- b0
    c <- c0
    sumb <- 0
    for(i in 1:n)
    {
      sumb = sumb + (1/c) + (exp((b-tn)/c)/c) - (exp((b-tVec[i])/c)/c) + exp(-exp(b/c)+(b/c))/((1-exp(-exp(b/c)))*c) - (exp(-exp(b/c)+(b/c)))/(c-exp(-exp(b/c))*c)
    }
    
    mle_b <- sumb
    return(mle_b)
  }
  
  #MLE of Parameter Theta
  MLE_c <- function(c0)
  {
    b <- b0
    c <- c0
    sumc <- 0
    for(i in 1:n)
    {
      sumc = sumc + -((exp(-exp((b/c)) + b/c)*b)/((1 - exp(-exp((b/c))))*(c^2))) - (exp((-tn + b)/c)*(-tn + b))/(c^2) - (1 - exp(-exp((b/c))) - (
        exp(-exp((b/c)) + (b/c))* b)/c)/(c - exp(-exp((b/c)))*c) - (b - tVec[i])/(c^2) + (exp((b - tVec[i])/c)*(b - tVec[i]))/(c^2)
    }
    
    mle_c <-  sumc
    return(mle_c)
  }
  
  prev_lllist <-0
  curr_lllist <-1
  #arule <- a0
  brule <- b0
  crule <- c0
  llerror <- 0
  llerror <- 1.0
  j <- 1
  
  while(llerror >1e-15)
  {
    
    prev_lllist <- LNL(brule,crule)
    #arule <- stats::uniroot(MLE_a,c(1,130),maxiter=1e10,tol=1e-10,extendInt ="yes")$root
    brule <- stats::uniroot(MLE_b,c(1,40),maxiter=1e8,tol=1e-10,extendInt = "yes")$root
    crule <- stats::uniroot(MLE_c,c(1,40),maxiter=1e8,tol=1e-10,extendInt = "yes")$root
    curr_lllist <- LNL(brule,crule)
    llerror <- abs(curr_lllist - prev_lllist)
    
  }
  #Updated a value
  aHat <- n/(exp(-exp(((-tn + brule)/crule)))/(1 - exp(-exp((brule/crule)))))
  
  TMAX_params <-  data.frame("TMAX_aMLE"=aHat,"TMAX_bMLE"=brule,"TMAX_cMLE"=crule)
  return(TMAX_params)
}

TMAX_MVF <- function(param,d) {
  #----------------------------------------------------------------------
  # This function computes the MVF data frame
  # MVF - Mean Value Function
  #----------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Failure, Time, Model columns
  #-----------------------------------------------------------------------
  #TODO:
  #======================================================================
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  cumFailures <- (exp(-exp(((-d$FT + param$TMAX_bMLE)/param$TMAX_cMLE)))*param$TMAX_aMLE)/(1 - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE))))
  r <- data.frame(cumFailures, d$FT, rep("TMAX", n))
  names(r) <- c("Failure","Time", "Model")
  r
}

TMAX_MVF_inv <- function(param,d) {
  #------------------------------------------------------------------------
  # This does an "inverse" MVF function, solving for time given
  # a specific value of MVF.
  #------------------------------------------------------------------------
  
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Failure, Time, Model columns
  #------------------------------------------------------------------------
  # TODO :  
  #========================================================================
  n <- length(d$FN)
  r <- data.frame()
  cumFailTimes <- param$TMAX_bMLE - param$TMAX_aMLE*log(-log((d$FN/param$TMAX_aMLE)*(1-exp(-exp(param$TMAX_bMLE/param$TMAX_cMLE)))))
  r <- data.frame(d$FN,cumFailTimes, rep("TMAX", n))
  names(r) <- c("Failure","Time", "Model")
  r
}

TMAX_MTTF <- function(param,d){
  #------------------------------------------------------------------------
  # This function MTTF of given d with parameters
  # MTTF is Mean Time To Failure
  #------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Failure, MTTF, Model columns
  #------------------------------------------------------------------------
  # TODO :
  #========================================================================
  n <- length(d$FT)
  r <-data.frame()
  fail_number <- c(0:(n-1))
  IFTimes <- 1/(exp(-exp(((-d$FT + param$TMAX_bMLE)/param$TMAX_cMLE)) + (-d$FT + param$TMAX_bMLE)/param$TMAX_cMLE)*param$TMAX_aMLE)/(param$TMAX_cMLE - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE)))*param$TMAX_cMLE)
  r <- data.frame(c(1:n),IFTimes, rep("TMAX", n))
  names(r) <- c("Failure_Number","MTTF","Model")
  r  
}

TMAX_FI <- function(param,d){
  #------------------------------------------------------------------------
  # This function computes the Failure Intensity for a given data
  # with parameters 'param' of a given data
  #------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Failure Count, Failure_Rate, Model columns
  #------------------------------------------------------------------------
  # TODO :
  #========================================================================
  n <- length(d$FT)
  r <-data.frame()
  fail_number <- c(1:n)
  failIntensity <- (exp(-exp(((-d$FT + param$TMAX_bMLE)/param$TMAX_cMLE)) + (-d$FT + param$TMAX_bMLE)/param$TMAX_cMLE)*param$TMAX_aMLE)/(param$TMAX_cMLE - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE)))*param$TMAX_cMLE)
  r <- data.frame(fail_number,failIntensity, rep("TMAX",n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r  
}


TMAX_R <- function(param,d){
  #---------------------------------------------------------------------------
  # This function computes Reliability from given parameters and data
  #---------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Time, Reliability, Model columns
  #---------------------------------------------------------------------------
  # TODO:
  #===========================================================================
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- d$FT[i]
    r[i,2] <- 1- (exp(-exp(((-d$FT[i] + param$TMAX_bMLE)/param$TMAX_cMLE))))/(1 - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE))))
  }
  r <- data.frame(r[1],r[2], rep("TMAX", n))
  names(r) <- c("Time","Reliability","Model")
  r
}

TMAX_lnL <- function(x,params){
  #----------------------------------------------------------------------------
  # This computes Log-Likelihood for a given data x and parameters
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (numeric)       Numeric value of lnL
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  n <- length(x)          
  a <- param$TMAX_aMLE
  b <- param$TMAX_bMLE
  c <- param$TMAX_cMLE
  sumln <- 0
  for(i in 1:n)
  {
    sumln = sumln + (-exp((b-x[i])/c))+(b-x[i])/c+log(a) - log(c-exp(-exp(b/c))*c)
  }
  
  lnla <- (-a)*exp(-exp((b-x[n])/c))/(1-exp(-exp(b/c)))  + sumln
  return(lnla)
}

#Faults Remaining

TMAX_FaultsRemaining <- function(params,n){
  #----------------------------------------------------------------------------
  # This function evaluates the Faults remaining in the system
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @n          (numeric)       Length of vector
  
  # @returns    (numeric)       Faults remaining
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  return(floor(param$TMAX_aMLE-n))
}


TMAX_MVF_cont <- function(param,t){
  #----------------------------------------------------------------------------
  # This function computes MVF at a particular time
  # This is a continuos function of time hence the name 'cont'
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @t          (numeric)       time 
  
  # @returns    (numeric)       MVF value at time t
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  return((exp(-exp(((-t + param$TMAX_bMLE)/param$TMAX_cMLE)))*param$TMAX_aMLE)/(1 - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE)))))
}

TMAX_R_delta <- function(params,cur_time,delta){
  #----------------------------------------------------------------------------
  # This function computes the Change in Reliability with delta change in time
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @cur_time   (numeric)       current time -> time at which reliability is calculated
  # @delta      (numeric)       delta time -> (t` - t), t` is delta away from current time   
  
  # @returns    (numeric)       Change in reliability with delta change in time    
  #---------------------------------------------------------------------------
  #TODO:
  #===========================================================================
  return(exp(-(TMAX_MVF_cont(params,(cur_time+delta)) -TMAX_MVF_cont(params,cur_time))))
}

TMAX_R_BM_root <- function(params,cur_time,delta, reliability){
  #---------------------------------------------------------------------------
  # This defines the function required for root finding target reliability
  #---------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @cur_time   (numeric)       current time -> time at which reliability is calculated
  # @delta      (numeric)       delta time -> (t` - t), t` is delta away from current time
  # reliability (numeric)       reliability
  
  # @returns    (function)      Return a function for uniroot evaluation used by TMAX_Target_T
  #---------------------------------------------------------------------------
  # TODO:
  #===========================================================================
  root_equation <- reliability - exp((exp(-exp(((-cur_time + param$TMAX_bMLE)/param$TMAX_cMLE)))*param$TMAX_aMLE)/(1 - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE)))) - (exp(-exp(((-(cur_time+delta) + param$TMAX_bMLE)/param$TMAX_cMLE)))*param$TMAX_aMLE)/(1 - exp(-exp((param$TMAX_bMLE/param$TMAX_cMLE)))))
  return(root_equation)
}



maxiter <- 1000
TMAX_Target_T <- function(params,cur_time,delta, reliability){
  #----------------------------------------------------------------------------
  # This computes the time it takes to achieve the target reliability
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @cur_time   (numeric)       current time -> time at which reliability is calculated
  # @delta      (numeric)       delta time -> (t` - t), t` is delta away from current time
  # @reliability (numeric)       reliability
  
  # @returns     (numeric)/      time it takes to achieve the target reliability
  #             (character)     string message if target reliability is already achieved 
  #----------------------------------------------------------------------------
  # TODO:
  #===========================================================================
  f <- function(t){
    return(TMAX_R_BM_root(params,t,delta, reliability))
  }
  
  current_rel <- TMAX_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- TMAX_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- TMAX_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (TMAX_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
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
            TMAX_Target_T(params,cur_time,delta, reliability)
          }
        },
        error = function(e){
          print(e)
          #return(e)
        })
    } else {
      sol <- Inf
    }
  } 
  else {
    sol <- "Target reliability already achieved"
  }
  return(sol)
}


TMAX_R_growth <- function(params,d,delta){
  #---------------------------------------------------------------------------------------
  #  This function computes the reliability growth
  #---------------------------------------------------------------------------------------
  
  # @params      (data.frame)    Data.frame of parameters
  # @d           (data.frame)    Data.frame of data FT,FC,CFC,IF
  # @delta       (numeric)       delta time -> (t` - t), t` is delta away from current time   
  
  # @returns     (data.frame)    Data frame of Time,Reliablity Growth, Model
  #---------------------------------------------------------------------------------------
  #TODO:
  #=======================================================================================   
  
  r <-data.frame()
  for(i in 1:length(d$FT)){   
    r[i,1] <- d$FT[i]
    temp <- TMAX_R_delta(params,d$FT[i],delta)
    #print(typeof(temp))
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
      r[i,3] <- "TMAX"
    }
    else{
      r[i,2] <- "NA"
      r[i,3] <- "TMAX"
    }     
  }
  g <- data.frame(r[1],r[2],r[3])
  names(g) <- c("Time","Reliability_Growth","Model")
  g  
}

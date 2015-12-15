# require("Rmpfr") # This was our option if precision is a problem. It stays here as long we are not sure.
require("utils") # depends on utils library
#library(rootSolve)

PR_ECM_MLE<-function(interFail){
  x <- as.numeric(interFail);
  n <- length(interFail);
  tn <- interFail[n];
  sumT <- sum(as.numeric(interFail))
  previous_lnl<-0
  current_lnl<-1
  
  #Brule initial estimate
  Denominator = 0
  for(i in 1:n){
    #Solve the denominator
    Denominator = Denominator + log(x[i]+1)
  }
  B_MLE <- n/Denominator
  brule<-B_MLE
  #the initial value of crule was calculated using the find root wrt to b.
  crule<-0.9999804716245578
  #Calculation of error for ECM loop
  LLerr <- 1
  
  #f(t) function of pareto
  PR_ft<-function(){
    eq <- (b*c*(c/(c + x[i]))^(-1 + b))/(c + x[i])^2
    return(eq)
  }
  
  ##Log-likelihood##
  # The value of a parameter for pareto is negative and hence the answers were incorrect;
  # the updated brule and crule were in negative which should not be the case.
  # Solution: The parameter a is considered as constant and normalized and the loglikehood
  # is taken by eliminating the -(MVF) and only retaining log function using the f(t)
  ##################
  lnl <-function(brule,crule){
    b<-brule
    c<-crule
    lastSumTerm<-0
    for(i in 1:n){
      lastSumTerm = lastSumTerm + log( (b*c*(c/(c + x[i]))^(-1 + b))/(c + x[i])^2)
    }
    lnL <- lastSumTerm
    return(lnL)
  }
  
  #A_MLE function-- even though not used passed in data frame for SRT tool
  Da <- function(){
    a<-1
    b<-brule
    c<-crule
    da<-(-1+(n/a)+(c/tn*c)^b)
    return(da)
  } 
 
  #initial estimate a for faults remaining
  #a0 <- stats::uniroot(Da, c(1,200), maxiter = 1e4, tol = 1e-10, extendInt = "yes")$root
  
  #B_MLE function
  Db<-function(brule){
    c<-crule
    b<-brule
    B_MLE<-0
    
    for(i in 1:n){
      B_MLE = B_MLE + ((c/(x[i] + c))^(
        1 - b)*(x[i] + c)^2*((c*(c/(x[i] + c))^(-1 + b))/(x[i] + c)^2 + (
          b*c*(c/(x[i] + c))^(-1 + b)*log(c/(x[i] + c)))/(x[i] + c)^2))/(b*c)
    }
    return(B_MLE)
  }
  
  #C_MLE function
  Dc<-function(crule){
    c<-crule
    b<-brule
    C_MLE<-0
    
    for(i in 1:n){
      C_MLE = C_MLE + ((c/(x[i] + c))^(
        1 - b)*(x[i] + c)^2*(-((2*b*c*(c/(x[i] + c))^(-1 + b))/(x[i] + c)^3) + (
          b*(c/(x[i] + c))^(-1 + b))/(x[i] + c)^2 + ((-1 + b)*b*c*(c/(x[i] + c))^(-2 +
                                                                                    b)*(-(c/(x[i] + c)^2) + 1/(x[i] + c)))/(x[i] + c)^2))/(b*c)
    }
    return(C_MLE)
  }
  ptm <- proc.time()
  #Main ECM LOOP
  while(LLerr>1e-2){
    previous_lnl <- lnl(brule,crule)
    #arule <- stats::uniroot(Da, c(0.0001,20), maxiter = 1e8, tol = 1e-10, extendInt = "yes")$root
    crule <- stats::uniroot(Dc, c(1,40), maxiter = 1e8, tol = 1e-10, extendInt = "yes")$root
    brule <- stats::uniroot(Db, c(0.001,40), maxiter = 1e6, tol = 1e-10, extendInt = "yes")$root
    current_lnl <- lnl(brule,crule)
    LLerr <- abs(current_lnl-previous_lnl)
  }
  end <- proc.time() - ptm
  
  ahat <- -(n/(-1 + (crule/(x[n] +crule))^brule))
  
  PR_params <-  data.frame("PR_aMLE"=ahat,"PR_bMLE"=brule, "PR_cMLE"=crule) # return results in format {MODEL}_{MODEL_params[n]}=value1 for all parameters 
  return(PR_params)
}

PR_MVF <- function(param,d) {
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
  cumFailures <- param$PR_aMLE*(1-(param$PR_cMLE/(d$FT+param$PR_cMLE))^param$PR_bMLE)
  r <- data.frame(cumFailures, d$FT, rep("PR", n))
  names(r) <- c("Failure","Time", "Model")
  r
}

PR_MVF_inv <- function(param,d) {
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
  cumFailTimes <- -(1 - 1/(1 - (d$FN/param$PR_aMLE)^(1/param$PR_bMLE)))
  r <- data.frame(d$FN,cumFailTimes, rep("PR", n))
  names(r) <- c("Failure","Time", "Model")
  r
}

PR_MTTF <- function(param,d){
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
  IFTimes <- 1/(param$PR_aMLE*param$PR_bMLE*param$PR_cMLE*((param$PR_cMLE/(param$PR_cMLE+d$FT))^(-1+param$PR_bMLE))/(param$PR_cMLE+d$FT)^2)
  r <- data.frame(c(1:n),IFTimes, rep("PR", n))
  names(r) <- c("Failure_Number","MTTF","Model")
  r  
}

PR_FI <- function(param,d){
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
  failIntensity <- param$PR_aMLE*param$PR_bMLE*param$PR_cMLE*((param$PR_cMLE/(param$PR_cMLE+d$FT))^(-1+param$PR_bMLE))/(param$PR_cMLE+d$FT)^2
  r <- data.frame(fail_number,failIntensity, rep("PR",n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r  
}

PR_R <- function(param,d){
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
    r[i,2] <- (1-(param$PR_bMLE*param$PR_cMLE*(param$PR_cMLE/(param$PR_cMLE + d$FT[i]))^(-1 + param$PR_bMLE))/(param$PR_cMLE + d$FT[i])^2)
  }
  r <- data.frame(r[1],r[2], rep("PR", n))
  names(r) <- c("Time","Reliability","Model")
  r
}

PR_lnL <- function(x,param){
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
  tn<-x[n]
  firstSumTerm <- 0
  lastSumTerm <- 0
  for(i in 1:n){
    lastSumTerm = lastSumTerm + log((param$PR_aMLE*param$PR_bMLE*param$PR_cMLE*(param$PR_cMLE/(param$PR_cMLE + x[i]))^(-1 + param$PR_bMLE))/(param$PR_cMLE + x[i])^2)
  }
  lnL <- (-param$PR_aMLE)*(1-(param$PR_cMLE/(param$PR_cMLE+tn))^param$PR_bMLE) + lastSumTerm
  return(lnL)
}

PR_FaultsRemaining <- function(params,n){
  #----------------------------------------------------------------------------
  # This function evaluates the Faults remaining in the system
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @n          (numeric)       Length of vector
  
  # @returns    (numeric)       Faults remaining
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  return(floor(params$PR_aMLE-n))
}

PR_MVF_cont <- function(params,t){
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
  return(param$PR_aMLE*(1-(param$PR_cMLE/(t+param$PR_cMLE))^param$PR_bMLE))
}

PR_R_delta <- function(params,cur_time,delta){
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
  return(exp(-(PR_MVF_cont(params,(cur_time+delta)) -PR_MVF_cont(params,cur_time))))
}

PR_R_ECM_root <- function(params,cur_time,delta, reliability){
  #---------------------------------------------------------------------------
  # This defines the function required for root finding target reliability
  #---------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @cur_time   (numeric)       current time -> time at which reliability is calculated
  # @delta      (numeric)       delta time -> (t` - t), t` is delta away from current time
  # reliability (numeric)       reliability
  
  # @returns    (function)      Return a function for uniroot evaluation used by PR_Target_T
  #---------------------------------------------------------------------------
  # TODO:
  #===========================================================================
  root_equation <- reliability - exp(param$PR_aMLE*(1-(param$PR_cMLE/(cur_time+param$PR_cMLE))^param$PR_bMLE) - param$PR_aMLE*(1-(param$PR_cMLE/((cur_time+delta)+param$PR_cMLE))^param$PR_bMLE))
  return(root_equation)
}

maxiter <- 1000
PR_Target_T <- function(params,cur_time,delta, reliability){
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
    return(PR_R_ECM_root(params,t,delta, reliability))
  }
  
  current_rel <- PR_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- PR_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- PR_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (PR_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
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
            PR_Target_T(a,b,cur_time,delta, reliability)
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


PR_R_growth <- function(params,d,delta){
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
    temp <- PR_R_delta(params,d$FT[i],delta)
    #print(typeof(temp))
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
      r[i,3] <- "PR"
    }
    else{
      r[i,2] <- "NA"
      r[i,3] <- "PR"
    }     
  }
  g <- data.frame(r[1],r[2],r[3])
  names(g) <- c("Time","Reliability_Growth","Model")
  g  
}

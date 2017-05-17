# require("Rmpfr") # This was our option if precision is a problem. It stays here as long we are not sure.
require("utils") # depends on utils library

JM_BM_IF_MLE<-function(interFail){
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
  # @returns  : JM_params  (data.frame) Dataframe of 'N0' and 'Phi'  
  #             JM_params$N0 refers to N0 and JM_params$Phi refers to Phi 
  #==================================================================================


  interFail <- as.numeric(interFail)  # to avoid precision problems
  n <- length(interFail)              # length of vector interfail
  #----------------------------------------------------------------------------------
  MLEeq<-function(N0){
    #--------------------------------------------------------------------------------
    # Used to calculate Likelihood funtion value at point 'N0'.
    # This function aids in the calcultion of zero of the likelihood function.
    # -------------------------------------------------------------------------------
    # @params   : N0      (numeric)   Numeric value of leftendpoint or rightendpoint
    #
    # @returns  : N0_MLE  (numeric)   Numeric value of JM likehood funtion. 

    # TODO : @params N0 is confusing should be changed.
    #================================================================================

    leftTerm = 0  # term in the likelihood equation.
    interFailSum = 0 # variable to save the intermediate results of interfailure vector sum
    rightTermDenominator = 0 # right term in the likelihood funtion
    for(i in 1:n){    # for each value in interfail vector calculate the consecutive sum of leftterm interfailsum and rightterm
      leftTerm =leftTerm+(1/(N0-(i-1)))
      interFailSum = interFailSum + interFail[i]
      rightTermDenominator = rightTermDenominator+((N0-(i-1))*interFail[i])
    }

    N0_MLE <- leftTerm-((n* interFailSum)/rightTermDenominator) # function value
    return(N0_MLE) # return function value
  }

  #----------------------------------------------------------------------------------
  # Step-1: Determine initial parameter estimate for parameter 'b0'
  # The initial estimate should be able to bracket the zero of the likelihood function
  # That's only requirement and the following assumption is determined by trial and 
  # error. The other best tried option is included here for reference
  # b0 <- n/sum(interFail)
  #==================================================================================
  b0 <- n # initial estimate of leftendpoint equating to length of input vector
  #==================================================================================

  # ---------------------------------------------------------------------------------
  # Step-2: Bracket root
  # ---------------------------------------------------------------------------------
  i <- 1 # count variable should not exceed maxiterations
  maxIterations     <- 100000 # maxiterations defined
  leftEndPoint      <- b0     # leftendpoint defined
  leftEndPointMLE   <- MLEeq(leftEndPoint) # likelihood function value at leftendpoint
  rightEndPoint     <- 2*b0                # defining right end point
  rightEndPointMLE  <- MLEeq(rightEndPoint) # likelihood function value at rightendpoint

  # ----------------------DEBUG STATEMENTS-------------------
  #------> ! #print(paste("left:",leftEndPointMLE))
  #------> ! #print(paste("right:",rightEndPointMLE))
  # =========================================================

  while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){

    #------------DEBUG STATEMENTS--------------------
    ##print('In Step 2 while loop of JM_BM.R')
    #================================================

    # leftendPoint is reduced to half and rightendpoint is doubled 
    # untill zeros of equation is bracket but not exceeding 10000 iterations

    leftEndPoint <- leftEndPoint/2
    leftEndPointMLE <- MLEeq(leftEndPoint)
    rightEndPoint <- 2*rightEndPoint
    rightEndPointMLE <- MLEeq(rightEndPoint)
    i <- i+1	
  }

  # ---------------------DEBUG STATEMENTS--------------------
  # #printing endPoints to check.
  # -----> ! #print(c(leftEndPointMLE,rightEndPointMLE))
  #==========================================================


  # -----------------------------------------------------------------------
  # Step-3: Invoke uniroot or report non convergence to calling environment

  if(leftEndPointMLE*rightEndPointMLE > 0 ){
    # Enters this block only if the maxIterations is reached before bracketing 
    # the root
    return('nonconvergence')
  }
  else {
    # Enters this block when zero of equation is contained within the leftendpoint
    # and rightendpoint
    maxiter <- 20  # maxiter variable for uniroot function.
    soln <- function(maxiter){
      #-----------------------------------------------------------------------------------
      # This function is used to find the root of the equation ( N0 parameter).
      #-----------------------------------------------------------------------------------
      # @params   : maxIter   (integer)   Maximum number of iterations by uniroot to find root.
      # @returns  : sol       (numeric)   NO
      #----------------------------------------------------------------------------------

      # Note : [Recursive nature] maxiter will increase untill the root is found. The exception handler captures the 
      #         warning that the tolerance is not reached with in the maxiter and increases the maxiter
      #         untill the root is found recursively.
      #----------------------------------------------------------------------------------
      # TODO  : Maximumretries should be used to break out after Maximum number of retries
      #----------------------------------------------------------------------------------
      sol <- tryCatch(
                stats::uniroot(MLEeq, c(leftEndPoint,rightEndPoint), maxiter=maxiter, tol=1e-10)$root,
                warning = function(w){
                  if(length(grep("_NOT_ converged",w[1]))>0){
                    maxiter <- maxiter+10
                    ##print(paste("recursive", maxiter,sep='_'))
                    soln(maxiter)
                  }
                },
                error = function(e){
                  #print(e)
              })
      sol
    }

    N0_MLE <- soln(maxiter) #

    if(N0_MLE < n){
      #--------------------------------------------------------------
      # N0_MLE should usually be greater than length of input vector.
      # So break out if thats not the case
      #--------------------------------------------------------------
      return("nonconvergence")
    }
    # ---------------------------DEBUG STATEMENTS-----------------------------------------------------------------------------------
    # Tried different variations of uniroot
    # ----> ! N0_MLE <- stats::unirootR(MLEeq,interval=mpfr(c(leftEndPoint,rightEndPoint),120),tol=1e-20)$root
    # ----> ! N0_MLE <- stats::uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, extendInt="yes",maxiter=10000, tol = 1e-24)$root
    # ----> ! N0_MLE <- stats::unirootR(MLEeq,lower=mpfr(leftEndPoint,300),upper=mpfr(rightEndPoint,300), tol = 1e-40)$root
    #===============================================================================================================================
  }
  tmp_phi <- numeric(0) # numeric to avoid precision problems {TODO: mention of type of error if not followed(type casting to numeric)}
  for(i in 1:n){ #deriving phi
  	tmp_phi[i] <- (N0_MLE-(i-1))*interFail[i]
  }
  Phi <- n/sum(tmp_phi)

  JM_params <-  data.frame("JM_N0"=N0_MLE,"JM_Phi"=Phi) # return results in format {MODEL}_{MODEL_params[n]}=value1 for all parameters 
  return(JM_params)
}

JM_MVF_efficient <- function(param,d){
  #-----------------------------------------------------------------------
  # This function computes the MVF function using failure rate
  #-----------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF

  # @returns    (data.frame)    data.frame of Failure, Time, Model columns
  #-----------------------------------------------------------------------
  # TODO : Title is seriously vague
  #=======================================================================
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
    r[i,2] <- 1/(param$JM_Phi*(param$JM_N0-(i-1)))
    cumulr[i,1] <- i
    cumulr[i,2] <- 0    
    for(j in 1:length(r[[1]])){
      cumulr[i,2] <- cumulr[i,2]+r[j,2]
    }
  }

  g <- data.frame(cumulr[2],cumulr[1], rep("JM", n))
  names(g) <- c("Time","Failure", "Model")
  g  
}


JM_MVF <- function(param,d) {
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
  cumFailures <- param$JM_N0*(1-exp(-param$JM_Phi*d$FT))
  r <- data.frame(cumFailures, d$FT, rep("JM", n))
  names(r) <- c("Failure","Time", "Model")
  r
}



JM_MVF_inv <- function(param,d) {
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
  #print(param$JM_N0)
  #print(d$FN)
  numPredPoints = floor(param$JM_N0) - (d$FN[1] - 1) #Number of points to be predicted : floor(aMLE) - (number of samples)
  cumFailTimes <- -(log((param$JM_N0-d$FN)/param$JM_N0))/param$JM_Phi
  
  if(numPredPoints < n){
    cumFailTimes[is.na(cumFailTimes)] <- 0 #If there are NaNs in the frame, replace it with zeros
    cumFailTimes[numPredPoints:length(cumFailTimes)] <- max(cumFailTimes[1:numPredPoints])
  }
  r <- data.frame(d$FN,cumFailTimes, rep("JM", n))
  names(r) <- c("Failure","Time", "Model")
  r
}


JM_MTTF <- function(param,d){
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
  IFTimes <- 1/(param$JM_Phi*(param$JM_N0 - fail_number))
  r <- data.frame(c(1:n),IFTimes, rep("JM", n))
  names(r) <- c("Failure_Number","MTTF","Model")
  r  
}

JM_FI <- function(param,d){
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
  failIntensity <- param$JM_N0*param$JM_Phi*exp(-param$JM_Phi*d$FT)
  r <- data.frame(fail_number,failIntensity, rep("JM",n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r  
}

JM_R <- function(param,d){
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
    r[i,2] <- exp(-param$JM_Phi*(param$JM_N0-(n-1))*d$FT[i])
  }
  r <- data.frame(r[1],r[2], rep("JM", n))
  names(r) <- c("Time","Reliability","Model")
  r
}

JM_MVF_r <- function(param,d){
  #---------------------------------------------------------------------------
  # This is another redundant MVF function 
  #---------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF

  # @returns    (data.frame)    data.frame of Failure, Time, Model columns
  #===========================================================================
  n <- length(d$FT)
  r <- data.frame()
  t_index <- seq(d$FT[1],d$FT[n],(d$FT[n]-d$FT[1])/100)
  for(i in 1:length(t_index)){
    r[i,1] <- t_index[i]
    r[i,2] <- param$JM_N0*(1-exp(-1*t_index[i]*param$JM_Phi))
  }
  r <- data.frame(r[1],r[2], rep("JM", n))
  names(r) <- c("Time","Failure", "Model")
  r
}


JM_lnL <- function(x,params){
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
  secondTerm=0
  thirdTerm = 0

  for(i in 1:n){
    secondTerm = secondTerm +log((params$JM_N0-(i-1)))
    thirdTerm = thirdTerm +((params$JM_N0-(i-1))*x[i])#x=interFail
  }
  lnL <- n*log(params$JM_Phi)+ secondTerm-(params$JM_Phi*thirdTerm)
  return(lnL)
}
 
 #Faults Remaining
 
JM_FaultsRemaining <- function(params,n){
  #----------------------------------------------------------------------------
  # This function evaluates the Faults remaining in the system
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @n          (numeric)       Length of vector

  # @returns    (numeric)       Faults remaining
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  return(floor(params$JM_N0-n))
}
 
 #Reliability

JM_Reliability <- function(n,x,params){
  #----------------------------------------------------------------------------
  # This function computes reliability but we are not using it
  # Its here for reference. I dont remember so not documenting accurately
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @x          (list)          Failure time vector
  # n           (numeric)       It used to depend on n not so now.

  # @returns    (numeric)       Reliability
  #----------------------------------------------------------------------------
  # TODO : Should consider if it should be here / document it properly
  #============================================================================

  Reliability <- numeric(0)
  Reliability <- exp(-params$Phi*(params$JM_N0-(i-1))*x[i])
  return(Reliability)
}
 


JM_MVF_cont <- function(params,t){
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
  #print("---")
  #print(params$JM_N0[[1]]*(1-exp(-params$JM_Phi[[1]]*t)))
  return(params$JM_N0*(1-exp(-params$JM_Phi*t)))
}

JM_R_delta <- function(params,cur_time,delta){
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
  return(exp(-(JM_MVF_cont(params,(cur_time+delta)) -JM_MVF_cont(params,cur_time))))
}

JM_R_BM_root <- function(params,cur_time,delta, reliability){
  #---------------------------------------------------------------------------
  # This defines the function required for root finding target reliability
  #---------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @cur_time   (numeric)       current time -> time at which reliability is calculated
  # @delta      (numeric)       delta time -> (t` - t), t` is delta away from current time
  # reliability (numeric)       reliability

  # @returns    (function)      Return a function for uniroot evaluation used by JM_Target_T
  #---------------------------------------------------------------------------
  # TODO:
  #===========================================================================
  root_equation <- reliability - exp(params$JM_N0*(1-exp(-params$JM_Phi*cur_time)) - params$JM_N0*(1-exp(-params$JM_Phi*(cur_time+delta))))
  return(root_equation)
}

maxiter <- 1000
JM_Target_T <- function(params,cur_time,delta, reliability){
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
    return(JM_R_BM_root(params,t,delta, reliability))
  }

  current_rel <- JM_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- JM_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- JM_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (JM_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
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
            JM_Target_T(a,b,cur_time,delta, reliability)
          }
        },
        error = function(e){
          #print(e)
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


JM_R_growth <- function(params,d,delta){
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
    temp <- JM_R_delta(params,d$FT[i],delta)
    ##print(typeof(temp))
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
      r[i,3] <- "JM"
    }
    else{
      r[i,2] <- "NA"
      r[i,3] <- "JM"
    }     
  }
  g <- data.frame(r[1],r[2],r[3])
  names(g) <- c("Time","Reliability_Growth","Model")
  g  
}


#JM_R_growth <- function(params,cur_time,delta, reliability){  
#  
#  r <-data.frame()
#  tt_index <- seq(0,cur_time,cur_time/1000)
#    for(i in 1:length(tt_index)){   
#      r[i,1] <- tt_index[i]
#      temp <- JM_R_delta(params,tt_index[i],delta)
#      ##print(typeof(temp))
#      if(typeof(temp) != typeof("character")){
#        r[i,2] <- temp
#        r[i,3] <- "JM"
#      }
#      else{
#        r[i,2] <- "NA"
#        r[i,3] <- "JM"
#      }     
#    }
#    g <- data.frame(r[1],r[2],r[3])
#    names(g) <- c("Time","Reliability_Growth","Model")
#    ##print(g)
#    g
#      
#}

 #MTTF
 
 # JM_MTTF <- function(n,params){ # params should be passed instead
 #  MTTF=0
 #  for(i in 1:n){
 #    MTTF = MTTF +(1/(params$JM_Phi*(params$JM_N0-(n+(i-1)))))
 #  }
 #  return(MTTF)
 # }
 
 
 #Jelinski-Moranda - Equation is same as GO
 

JM_OR_CC <- function(param,c1,c2,c3){
  return((1/param$JM_Phi)*log((param$JM_N0*param$JM_Phi*(c2-c1))/c3))
}

#Cost equation for JM optimal release plots
JM_cost <- function(params,c1,c2,c3,t,t_lifecycle){
  return(c1*JM_MVF_cont(params,t) + c2*(JM_MVF_cont(params,t_lifecycle) - JM_MVF_cont(params,t)) + c3*t)
}

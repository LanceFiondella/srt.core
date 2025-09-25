# Maximum likelihood estimation using Expectation Condition Maximization on failure time data
MO_ECM_FT_MLE <- function(inputData){
  # inputData is a vector of failure times; if another data type is desired, the name of the MLE function should be changed to match the desired input type, such as  MO_ECM_IF_MLE for interfailure times, or MO_ECM_FC_MLE for failure counts
  
  # Vector of failure times
  t <- as.numeric(inputData)
  n <- length(t) # size of input vector
  tn <- t[n] # final value of t
  
  # Partial derivative of lnL with respect to a
  ahat = function(a, b, t, tn){
    leftTerm = -(b * tn)/(a * (a * b * tn + 1)) + log(a * b * tn + 1)/(a^2)
    
    rightTerm <- 0
    for(i in 1:n){
      rightTerm = rightTerm + (b * t[i])/(a * b * t[i] + 1)
    }
    amle = leftTerm - rightTerm
    return(amle)
  }
  
  # Partial derivative of lnL with respect to b
  bhat = function(b, a, t, tn){
    leftTerm = -tn/(a * b * tn + 1)
    rightTerm <- 0
    for(i in 1:n){
      rightTerm = rightTerm + 1/(b * (a * b * t[i] + 1))
    }
    bmle = leftTerm + rightTerm
    return(bmle)
  }
  
  # Initial guesses for a and b parameters. These values may need to be adjusted depending on the input data
  a <- 0.01;
  b <- 0.01;
  
  # Log-likelihood of initial guesses
  params <- data.frame("MO_aMLE" = a, "MO_bMLE" = b)
  LL <- MO_lnL(t, params)
  
  # Placeholder error value
  LLerror = 1;
  
  # First calculation based on initial values. Interval, max iterations, and tolerance all may need adjustment depending on the input data. Relies on uniroot root finding utility, which optimizes a function with respect to the first input parameter. Here we are minimizing a based on initial guesses, and b based on the output of the minimization of a
  a <- stats::uniroot(ahat, interval = c(.02, .1), b = b, t = t, tn = tn, maxiter=100, tol=1e-10, extendInt="yes")$root
  b <- stats::uniroot(bhat, interval = c(.01, .1), a = a, t = t, tn = tn, maxiter=100, tol=1e-10, extendInt="yes")$root
  
  
  params <- data.frame("MO_aMLE" = a, "MO_bMLE" = b)
  # Log-likelihood of first iteration
  LLnew <- MO_lnL(t, params)
  # Error is current log-likelihood value minus previous value
  LLerror <- LLnew - LL
  # Current log-likelihood becomes previous value
  LL <- LLnew
  
  # Iterate until an arbitrarily small error is reached
  while (LLerror > 10^-10)
  {
    # Find roots of MLE equations
    a <- stats::uniroot(ahat, interval = c(.001, 1), b = b, t = t, tn = tn, maxiter=100, tol=1e-10, extendInt="yes")$root
    b <- stats::uniroot(bhat, interval = c(.001, 1), a = a, t = t, tn = tn, maxiter=100, tol=1e-10, extendInt="yes")$root
    
    # Calculate new Log-likelihood and error
    params <- data.frame("MO_aMLE" = a, "MO_bMLE" = b)
    LLnew <- MO_lnL(t, params)
    LLerror <- LLnew - LL
    
    LL <- LLnew
    
  }
  # a and b are the calculated parameter values. The solution is returned as a data frame with field names based on the params variable set in model_specifications.R
  
  solution <- data.frame("MO_aMLE"= a, "MO_bMLE"=b)
  return(solution)
}

# Log-Likelihood function
MO_lnL <- function(inputData, params) {
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # Row vector of failure times taken from input data
  t <- inputData
  n <- length(t) # Number of rows in the input
  tn <- t[n] # Final value of input vector
  
  # Leftmost term of lnL. Negative MVF at tn
  leftTerm = -log(a * b * tn + 1)/a
  
  # Right terms of lnL. Summation of log(MO_FI)
  rightTerm <- 0
  for(i in 1:n){
    rightTerm = rightTerm + log(b/(a * b * t[i] + 1))
  }
  
  # Return the log-likelihood
  lnL <- leftTerm + rightTerm
  return(lnL)
}

# Mean Value Function
MO_MVF <- function(params, inputData) {
  # Params is a data frame named according to model_specifications.R

  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # Row vector of failure times taken from input data
  t <- inputData$FT 
  n <- length(t) # Number of rows in the input
  
  # Calculate the mean value function based on the parameters and input failure time data
  MVF <- log(b * a * t + 1)/a
  
  # MVF is vector of mean number of failures for each input time
  solution <- data.frame("Failure" = MVF, "Time" = t, "Model" = rep("MO", n))
  return(solution)
}

# Analytically determined inverse MVF
MO_MVF_inv <- function(params, inputData) {
  # Params is a data frame named according to model_specifications.R

  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # Row vector of failure numbers taken from input data
  fn <- inputData$FN 
  n <- length(fn) # Number of rows in the input
  
  # Calculate the inverse mean value function based on the parameters and input failure number data
  IMVF <- (-1 + exp(a * fn))/(b * fn)
  
  # IMVF is vector of failure times for each input failure count
  solution <- data.frame("Failure" = fn, "Time" = IMVF, "Model" = rep("MO", n))
  return(solution)
}

# Mean time to failure (1 divided by the failure intensity)
MO_MTTF <- function(params, inputData) {
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # Row vector of failure times taken from input data
  t <- inputData$FT 
  n <- length(t) # Number of rows in the input
  
  # Calculate the mean time to failure based on the parameters and input failure time data
  MTTF <- (a * b * t + 1)/b
  
  # MTTF is vector of mean time to failure for each input time
  solution <- data.frame("Failure_Number" = c(1:n), "MTTF" = MTTF, "Model" = rep("MO", n))
  return(solution)
}

# Failure Intensity (Derivative of MVF)
MO_FI <- function(params, inputData) {
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # Row vector of failure times taken from input data
  t <- inputData$FT 
  n <- length(t) # Number of rows in the input
  
  # Index of failure times
  failnum <- c(1:n)
  
  # Calculate the failure intensity based on the parameters and input failure time data
  failIntensity <- b / (a * b * t + 1)
  
  # failIntensity is the calculated vector of mean time to failure for each input time
  solution <- data.frame("Failure_Count" = failnum, "Failure_Rate" = failIntensity, "Model" = rep("MO", n))
  return(solution)
}

# MVF given scalar time value
MO_MVF_cont <- function(params, failureTime) {
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # failureTime is a float representing the time to calculate number of failures at
  
  # Calculate the mean value function based on the parameters and input failure time data
  MVF <- log(b * a * failureTime + 1)/a
  # MVF is mean number of failures at the input time
  return(MVF)
}

MO_R_delta <- function(params, cur_time, delta) {
  # cur_time is a float representing the "current" time, or starting point
  # delta represents the time interval to measure change in reliability over
  solution <- exp(-(MO_MVF_cont(params, (cur_time[1] + delta[1])) - MO_MVF_cont(params, cur_time[1])))
  
  # Check if solution exists
  if(anyNA(solution))
  {
    solution <- 0;
    warning("MO_MVF_cont contains NAs")
  }
  return(solution)
}

MO_R_growth <- function(params, inputData, delta){
  
  # Params is a data frame named according to model_specifications.R
  
  # Row vector of failure times taken from input data
  t <- inputData$FT 
  n <- length(t) # Number of rows in the input
  
  # Declare solution in advance
  solution <- data.frame()
  
  # Compute change in reliability at every point. If it produces an error at any point, replace the value with NA
  for(i in 1:n){
    solution[i, 1] <- t[i]
    temp <- MO_R_delta(params, t[i], delta)
    
    if(typeof(temp) != typeof("character")){
      solution[i,2] <- temp
      solution[i,3] <- "MO"
    }
    else{
      solution[i,2] <- "NA"
      solution[i,3] <- "MO"
    }
  }
  
  names(solution) <- c("Time", "Reliability_Growth", "Model")
  return(solution)
}

MO_R_MLE_root <- function(params, cur_time, delta, reliability){
  root_equation <- reliability - MO_R_delta(params, cur_time, delta)
  return(root_equation)
}

MO_Target_T <- function(params, cur_time, delta, reliability){
  maxiter <- 1000
  f <- function(t){
    return(MO_R_MLE_root(params,t,delta, reliability))
  }
  
  current_rel <- MO_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- MO_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- MO_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (MO_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
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
            MO_Target_T(params,cur_time,delta, reliability)
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

# Optimal release considering cost
MO_OR_CC<-function(params, c1, c2, c3){
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # c1 is the cost of removing a fault during testing
  
  # c2 is the cost of removing a fault after release
  
  # c3 is the cost of testing per time unit
  
  # Calculate optimal release time as a factor of parameters and input costs
  t_opt <- (b * (c2 - c1) - c3)/(a * b * c3)
  
  return(t_opt)
}

# Optimal release time considering reliability
MO_OR_RC<-function(params, t, R){
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # R is reliability
  
  # t is ???????????
  
  # Calculate optimal release time as a factor of parameters and reliability
  opt_t <- -1/b * (log(log(1 + a * b * t)/a) - log(log(R)))
  return (opt_t)
}

# Cost equation
MO_cost <- function(params, c1, c2, c3, t, t_lifecycle){
  # Params is a data frame named according to model_specifications.R
  
  # Define a parameter as calculated by the MLE function
  a <- params$MO_aMLE 
  b <- params$MO_bMLE # Define b parameter
  
  # c1 is the cost of removing a fault during testing
  
  # c2 is the cost of removing a fault after release
  
  # c3 is the cost of testing per time unit
  
  # t is ??????
  
  # t_lifecyle is software lifecyle time
  
  # Calculate optimal release time as a factor of parameters, input costs, and software lifecycle time
  cost <- 1/a((c1 - c2) * log(1 + a * b * t) + c2 * log(1 + a * b * t_lifecycle)) + a * c3 * t
  return(cost)
}

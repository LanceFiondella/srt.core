### ------- Maximum Likelihood Estimation (MLE) -------

LN_LS_FT_MLE <- function(t, maxiter = 10000) {
  #----------------------------------------------------------------------------------
  # Maximumum likelihood estimation method is used to estimate the parameters 
  # 'alpha', 'mean', and 'std'. Mean and standard deviation are calculated directly
  # from the loglikelihood, while alpha is the predicted number of observations given
  # infinite time and is calculated via least squares with the inverse MVF
  #
  #----------------------------------------------------------------------------------
  # @t          (list)          list of failure times FT
  # @maxit      (numeric)       maximum number of iterations for optim()
  #
  # @returns    (data.frame)    data.frame of MLE alpha and beta values
  #----------------------------------------------------------------------------------
  # Source :
  #----------------------------------------------------------------------------------
  # TODO : Is there a better way to determine alpha (predicted failures)?
  #==================================================================================
  n <- length(t)
  log_t <- log(t)
  
  LN_mean <- sum(log_t) / n  # The mean of the log of the data
  LN_std <- sqrt(sum((log_t - LN_mean)^2) / n)  # The standard deviation of the log of the data
  
  # Use Least Squares to determine the best scaling value, 'alpha'
  alphaEQ <- function(x) {
    pparams<- data.frame("LN_alpha" = x, "LN_mean" = LN_mean, "LN_std" = LN_std)
    sum((LN_MVF_inv(pparams, data.frame("FN" = seq_along(t)))$Time - t)^2)
  }
  
  # Interval ranges from just above the current number of failures, n+1, to n^2.5
  # Note that alpha > n for the inverse, and for prediction
  # The 2.5 exponent is a purely arbitrary value that *should* encapsulate the possible number of failures
  LN_alpha <- optimize(alphaEQ, c(n + 1, n^2.5))$minimum
  
  sol <- data.frame(LN_alpha, LN_mean, LN_std)
  names(sol) <- c(paste("LN", LN_params[1], sep="_"),  # alpha
                  paste("LN", LN_params[2], sep="_"),  # mean
                  paste("LN", LN_params[3], sep="_"))  # standard deviation
  sol
} # LN_LS_FT_MLE


### ------- Mean Value Function (MVF) -------

LN_MVF <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the MVF for the Log Normal distribution
  #
  # MVF : cum failures = alpha * Norm((log(t) - mean) / std)
  #       where Norm is the cumulative standard normal function N(0,1), in pnorm
  # Inputs : vector of failure times (FT)
  # Outputs : vector of the expected cumulative number of failures
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure, Time, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(d$FT)
  
  MVF <- params$LN_alpha * stats::pnorm((log(d$FT) - params$LN_mean) / params$LN_std, 0, 1)
  
  r <- data.frame("Failure" = MVF, "Time" = d$FT, "Model" = rep("LN", n))
  r
} # LN_MVF


LN_MVF_inv <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the inverse MVF for the Log Normal distribution
  #
  # MVF_inv : time = exp(std * Norm.Inv(failures) + mean)
  #           where Norm.Inv is the inverse cumulative standard normal function,
  #             found in qnorm
  # Inputs : vector of cumulative number of failures (FN)
  # Outputs : vector of expected failure times
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure, Time, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(d$FN)
  
  cumFailTimes <- exp(params$LN_std * stats::qnorm(d$FN / params$LN_alpha, 0, 1) + params$LN_mean)

  numPredPoints = floor(params$LN_alpha) - (d$FN[1] - 1) # Number of points to be predicted : floor(alpha) - (number of samples)
  if(numPredPoints < n) {
    cumFailTimes[is.na(cumFailTimes)] <- 0              # If there are NaNs in the frame, replace it with zeros
    cumFailTimes[numPredPoints:length(cumFailTimes)] <- max(cumFailTimes[1:numPredPoints])
    d$FN[numPredPoints:n] <- max(d$FN[1:numPredPoints])
  }   

  r <- data.frame("Failure" = d$FN, "Time" = cumFailTimes, "Model" = rep("LN", n))
  r
} # LN_MVF_inv


LN_MVF_cont <- function(params, x) {
  #----------------------------------------------------------------------------------
  # Computes the continuous MVF for the Log Normal distribution
  #
  # MVF : cum failures = alpha * Norm((log(t) - mean) / std)
  #       where Norm is the cumulative standard normal function N(0,1), in pnorm
  # Inputs : numeric time value
  # Outputs : expected number of failures at input time
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @x          (numeric)       numeric time value
  #
  # @returns    (numeric)       MVF value at time x
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  params$LN_alpha * stats::pnorm((log(x) - params$LN_mean) / params$LN_std, 0, 1)
} # LN_MVF_cont


### ------- Failure Intensity (FI) -------

LN_FI <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the Failure Intensity for the Log Normal distribution
  # FI = d/dt MVF
  #
  # FI : failures = 1 / (t * std * sqrt(2*pi)) * exp(-(ln(t) - mean)^2 / (2*std^2))
  # Inputs : vector of failure times (FT)
  # Outputs : vector of expected number of failures
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure Count, Failure Rate, and Model
  #                             columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(d$FT)
  fail_number <- c(1:n)
  
  failIntensity <- params$LN_alpha / (d$FT * params$LN_std * sqrt(2 * pi)) * exp(-(log(d$FT) - params$LN_mean)^2 / (2 * params$LN_std^2))
  
  r <- data.frame("Failure_Count" = fail_number, "Failure_Rate" = failIntensity, "Model" = rep("LN", n))
  r
} # LN_FI


### ------- Log Likelihood (lnL) -------

LN_lnL <- function(t, params) {
  #----------------------------------------------------------------------------------
  # Computes the Log Likelihood formula for the Log Normal distribution
  #
  # lnL : Log Likelihood = -MVF(t_n) + sum(log(FI(t_i))) 
  # Inputs : vector of failure times (FT)
  # Outputs : Log Likelihood
  #----------------------------------------------------------------------------------
  # @t          (list)          list of failure times
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  #
  # @returns    (numeric)       value of the Log Likelihood estimate
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(t)
  tn <- t[n]
  
  lnLFirstTerm <- -LN_MVF_cont(params, tn)
  lnLSumTerms <- sum(log(LN_FI(params, data.frame("FT" = t))$Failure_Rate))
  
  lnLFirstTerm + lnLSumTerms
} # LN_lnL


### ------- Mean Time to Failure (MTTF) -------

LN_MTTF <- function(params, d){
  #----------------------------------------------------------------------------------
  # Computes the MTTF for the Log Normal distribution
  #
  # MTTF : time to next failure = mean
  # Inputs : vector of failure times (FT)
  # Outputs : vector of average times to next failure
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure Number, MTTF, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : Figure out the correct way to do this. Do we calculate the mean and var at
  #        each failure? Do we just use the current mean and var? Is the formula
  #        correct? (it produces large values)
  #==================================================================================
  n <- length(d$FT)
  fail_number <- c(1:n)
  
  MTTF <- rep(0, n)
  
  for(i in 1:n) {
    ilog_t <- log(d$FT[1:i])
    imean <- sum(ilog_t) / i 
    ivar <- sum((ilog_t - imean)^2) / i
    MTTF[i] <- exp(imean + 1/2 * ivar)
  }
  
  #MTTF <- c(MTTF, exp(params$LN_mean + 1/2 * params$LN_std^2))
  
  r <- data.frame("Failure_Number" = fail_number, "MTTF" = MTTF, "Model" = rep("LN", n))
  r
} # LN_MTTF


### ------- Reliability Functions -------

LN_R <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the Reliability function, R(t), for the Log Normal distribution
  # R(t) = 1 - F(t) = 1 - MVF / alpha
  # 
  # R(t) : Reliability = 1 - Norm((log(t) - mean) / std)
  # Inputs : vector of inter-failure times (IF)
  # Outputs : vector of reliability
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Time, Reliability, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(d$FT)
  fail_number <- c(1:n)
  
  reli <- 1 - stats::pnorm((log(d$FT) - params$LN_mean) / params$LN_std, 0, 1)
  
  r <- data.frame("Time" = d$FT, "Reliability" = reli, "Model" = rep("LN", n))
  r
} # LN_R


LN_R_delta <- function(params, cur_time, delta) {
  #----------------------------------------------------------------------------------
  # Calculates a change in reliability from time t to time t + delta t.
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @cur_time   (numeric)       numeric value for the current time
  # @delta      (numeric)       numeric value for the time difference, delta
  #
  # @returns    (numeric)       change in Reliability
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  exp(LN_MVF_cont(params, cur_time) - LN_MVF_cont(params, cur_time + delta))
} # LN_R_delta


LN_R_MLE_root <- function(params, cur_time, delta, reliability) {
  #----------------------------------------------------------------------------------
  # Helper function to find the time to a target reliability.
  #
  # Root : Reliability - R_delta(t, delta) = 0
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @cur_time   (numeric)       numeric value for the current time
  # @delta      (numeric)       numeric value for the mission time, delta
  # @reliability(numeric)       numeric value for the desired reliability
  #
  # @returns    (numeric)       root finding expression for Target_T function
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  reliability - LN_R_delta(params, cur_time, delta)
} # LN_R_MLE_root


LN_Target_T <- function(params, cur_time, delta, reliability) {
  #----------------------------------------------------------------------------------
  # Finds the time to a target reliability.
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @cur_time   (numeric)       numeric value for the current time
  # @delta      (numeric)       numeric value for the mission time, delta
  # @reliability(numeric)       numeric value for the desired reliability
  #
  # @returns    (numeric)       time to target Reliability, if target is already
  #                             achieved return a string
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  # Pulled from other methods
  maxiter <- 1000

  f <- function(t){
   LN_R_MLE_root(params, t, delta, reliability)
  }

  current_rel <- LN_R_delta(params, cur_time, delta)
  if(current_rel < reliability) {
   # Bound the estimation interval
   sol <- 0
   interval_left <- cur_time
   interval_right <- 2 * interval_left
   local_rel <- LN_R_delta(params, interval_right, delta)

   while (local_rel <= reliability) {
     interval_right <- 2 * interval_right
     if(local_rel == reliability) {
       interval_right <- 2.25 * interval_right
     }
     if (is.infinite(interval_right)) {
       break
     }
     local_rel <- LN_R_delta(params, interval_right, delta)
   }

   if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
     while (LN_R_delta(params,(interval_left + (interval_right - interval_left) / 2), delta) < reliability) {
       interval_left <- interval_left + (interval_right - interval_left) / 2
     }
   } else {
     sol <- Inf
   }

   if (is.finite(interval_right) && is.finite(sol)) {
     sol <- tryCatch(
       stats::uniroot(f, c(interval_left, interval_right), extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
       warning = function(w){
         ##print(f.lower)
         if(length(grep("_NOT_ converged", w[1])) > 0){
           maxiter <<- floor(maxiter * 1.5)
           #print(paste("recursive", maxiter, sep='_'))
           LN_Target_T(a, b, cur_time, delta, reliability)
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

  sol
} # LN_Target_T


LN_R_growth <- function(params, d, delta) {
  #----------------------------------------------------------------------------------
  # Generates the data for the plot of the reliability curve.
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  # @delta      (numeric)       numeric value for the mission time, delta
  #
  # @returns    (numeric)       Reliability growth over time data for plotting
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  growth <- LN_R_delta(params, d$FT, delta)
  
  growth <- as.numeric(as.character(growth))  # Replace strings with NA
  
  r <- data.frame("Time" = d$FT, "Reliability_Growth" = growth, "Model" = rep("LN", length(d$FT)))
  r
} # LN_R_growth


### ------- Cost Functions -------

LN_OR_CC <- function(params, c1, c2, c3) {
  #----------------------------------------------------------------------------------
  # Calculates the optimal release time for the Linear Littlewood-Verrall model
  #
  # Release : t = 
  # Inputs : Costs
  # Outputs : Time for optimal release of software
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @c1         (numeric)       numeric for the cost of removing fault in testing
  # @c2         (numeric)       numeric for the cost of removing fault in operation
  # @c3         (numeric)       numeric for the cost per unit time of testing
  #
  # @returns    (numeric)       Optimal release time
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : Figure out the mathematics behind this, currently just a placeholder using
  #        the structure from GO renamed for LN.
  #==================================================================================
  if (params$LN_alpha * params$LN_mean * params$LN_std <= (c3 / (c2 - c1))) {
    NA
  } else {
    NA
  }
} # LN_OR_CC


LN_OR_RC <- function(params, x, R) {
  #----------------------------------------------------------------------------------
  # Calculates the optimal time to achieve target reliability
  #
  # Reli : t = 
  # Inputs : Mission time (x) and target reliability (R)
  # Outputs: Time to target reliability
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha, mean, and std
  # @x          (numeric)       numeric for mission time
  # @R          (numeric)       numeric for the target reliability
  #
  # @returns    (numeric)       Optimal time to target reliability
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : Figure out the mathematics behind this.
  #==================================================================================
  NA
} # LN_OR_RC


LN_cost <- function(params, c1, c2, c3, t, t_lifecycle) {
  #----------------------------------------------------------------------------------
  # Calculates the total cost at the optimal release time determined in LN_OR_CC
  #
  # Cost : Cost = (c1-1)*MVF(t) + c2*MVF(t_lifecycle) + c3*t
  # Inputs : Costs, optimal release time, and lifecycle duration
  # Outputs : Cost at optimal release time
  #----------------------------------------------------------------------------------
  # @params      (data.frame)   data.frame of parameters alpha, mean, and std
  # @c1          (numeric)      numeric for the cost of removing fault in testing
  # @c2          (numeric)      numeric for the cost of removing fault in operation
  # @c3          (numeric)      numeric for the cost per unit time of testing
  # @t           (numeric)      numeric for the optimal release time
  # @t_lifecycle (numeric)      numeric for the software lifecycle duration
  #
  # @returns     (numeric)      Cost at optimal release time
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : Figure out the mathematics behind this.
  #==================================================================================
  (c1 - 1) * LN_MVF_cont(params, t) + c2 * LN_MVF_cont(params, t_lifecycle) + c3 * t
} # LN_cost







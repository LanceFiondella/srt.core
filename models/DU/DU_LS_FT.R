### ------- Maximum Likelihood Estimation (MLE) -------

DU_LS_FT_MLE <- function(t) {
  #----------------------------------------------------------------------------------
  # Uses Least Squares to estimate the parameters 'alpha' and 'beta', and so is not
  # technically an MLE technique. It fits a linear equation to the logarithm of the
  # MVF, based on the fundamental property of the Duane model.
  #
  #----------------------------------------------------------------------------------
  # @t          (list)          list of failure times FT
  #
  # @returns    (data.frame)    data.frame of MLE alpha and beta values
  #----------------------------------------------------------------------------------
  # Source : http://reliawiki.org/index.php/Duane_Model
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(t)
  
  lnt <- log(t)
  sum_lnt <- sum(lnt)
  
  log_fail_number <- log(c(1:n))
  sum_lnFN <- sum(log_fail_number)
  
  betaHat <- (sum(lnt * log_fail_number) - (sum_lnt * sum_lnFN) / n) /
             (sum(lnt^2) - sum_lnt^2 / n)
  
  alphaHat <- exp(1/n * (sum_lnFN - betaHat * sum_lnt))
  
  # This naming scheme should be used with caution without handling the parameter names in the below methods
  # For instance, if alpha is ever renamed, the MVF method will be unable to determine what the hardcoded reference "DU_alpha" is
  sol <- data.frame(alphaHat, betaHat)
  names(sol) <- c(paste("DU", DU_params[1], sep="_"),  # alpha
                  paste("DU", DU_params[2], sep="_"))  # beta
  sol
} # DU_LS_MLE

DU_SS_FT_MLE <- function(t, init = c(1, 1), maxiter = 10000) {
  #----------------------------------------------------------------------------------
  # Maximumum likelihood estimation method is used to estimate the parameters 
  # 'alpha' and 'beta'. Both MLE equations are minimized simulataneously by summing
  # their squares (thus the SS in the method name).
  #
  #----------------------------------------------------------------------------------
  # @t          (list)          list of failure times FT
  # @init       (list)          list of initial guesses for parameters
  # @maxit      (numeric)       maximum number of iterations for optim()
  #
  # @returns    (data.frame)    data.frame of MLE alpha and beta values
  #----------------------------------------------------------------------------------
  # Source : Reliability.R package
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  t <- as.numeric(t)
  n <- length(t)
  tn <- t[n]
  
  # Define MLE equation for parameter 'alpha'
  DU_aMLEeq <- function(alpha, beta) {
    alpha - n / tn^beta 
  }
  
  # Define MLE equation for parameter 'beta'
  DU_bMLEeq <- function(beta) {
    beta - (n / sum(log(tn/t)))
  }
  
  # Define global MLE equation for optimization
  DU_global_MLEeq <- function(params) {
    alpha <- params[1]
    beta <- params[2]
    eq1 <- DU_aMLEeq(alpha, beta)^2
    eq2 <- DU_bMLEeq(beta)^2
    eq1 + eq2
  }
  
  # Optimize global MLE equation
  res <- optim(init, DU_global_MLEeq, control = list(maxit = maxiter))
  DU_params_MLE <- optim(c(res$par[1], res$par[2], res$par[3]), DU_global_MLEeq, control = list(maxit = maxiter))
  
  sol <- data.frame(DU_params_MLE$par[1], DU_params_MLE$par[2])
  names(sol) <- c(paste("DU", DU_params[1], sep="_"),  # alpha
                  paste("DU", DU_params[2], sep="_"))  # beta
  sol
} # DU_SS_MLE


### ------- Mean Value Function (MVF) -------

DU_MVF <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the MVF for the Duane Model
  #
  # MVF : cumulative failures = alpha * t^beta
  # Inputs : vector of failure times (FT)
  # Outputs : vector of the expected cumulative number of failures
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure, Time, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(d$FT)
  # Instead of hardcoding data.frame names here, which will cause a problem if they are ever renamed in DU_params,
  # this will pull them based on their names, assuming 'alpha' is first and 'beta' is second
  #alpha <- params[[paste("DU", DU_params[1], sep="_")]]
  #beta  <- params[[paste("DU", DU_params[2], sep="_")]]
  
  MVF <- params$DU_alpha * d$FT^params$DU_beta
  
  r <- data.frame("Failure" = MVF, "Time" = d$FT, "Model" = rep("DU", n))
  r
} # DU_MVF


DU_MVF_inv <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the inverse MVF for the Duane Model
  #
  # MVF_inv : time = (failures / alpha)^(1 / beta) 
  # Inputs : vector of cumulative number of failures (FN)
  # Outputs : vector of expected failure times
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure, Time, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(d$FN)
  
  cumFailTimes <- (d$FN / params$DU_alpha)^(1 / params$DU_beta)
  
  r <- data.frame("Failure" = d$FN, "Time" = cumFailTimes, "Model" = rep("DU", n))
  r
} # DU_MVF_inv


DU_MVF_cont <- function(params, x) {
  #----------------------------------------------------------------------------------
  # Computes the continuous MVF for the Duane Model
  #
  # MVF : cumulative failures = alpha * x^beta
  # Inputs : numeric time value
  # Outputs : expected number of failures at input time
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @x          (numeric)       numeric time value
  #
  # @returns    (numeric)       MVF value at time x
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  params$DU_alpha * x^params$DU_beta
} # DU_MVF_cont


### ------- Failure Intensity (FI) -------

DU_FI <- function(params, d) {
  #----------------------------------------------------------------------------------
  # Computes the Failure Intensity for the Duane Model
  # FI = d/dt MVF
  #
  # FI : failures = alpha * beta * (t^(beta - 1))
  # Inputs : vector of failure times (FT)
  # Outputs : vector of expected number of failures
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
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
  
  failIntensity <- params$DU_alpha * params$DU_beta * d$FT^(params$DU_beta - 1)
  
  r <- data.frame("Failure_Count" = fail_number, "Failure_Rate" = failIntensity, "Model" = rep("DU", n))
  r
} # DU_FI


### ------- Log Likelihood (lnL) -------

DU_lnL <- function(t, params) {
  #----------------------------------------------------------------------------------
  # Computes the Log Likelihood formula for the Duane Model
  #
  # lnL : Log Likelihood = -MVF(t_n) + sum(log(FI(t_i))) 
  # Inputs : vector of failure times (FT)
  # Outputs : Log Likelihood
  #----------------------------------------------------------------------------------
  # @t          (list)          list of failure times
  # @params     (data.frame)    data.frame of parameters alpha and beta
  #
  # @returns    (numeric)       value of the Log Likelihood estimate
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  n <- length(t)
  tn <- t[n]
  
  lnLFirstTerm <- -DU_MVF_cont(params, tn)
  lnLSumTerms <- sum(log(DU_FI(params, data.frame("FT" = t))$Failure_Rate))
  
  lnLFirstTerm + lnLSumTerms
} # DU_lnL


### ------- Mean Time to Failure (MTTF) -------

DU_MTTF <- function(params, d){
  #----------------------------------------------------------------------------------
  # Computes the MTTF for the Duane Model
  #
  # MTTF : time to next failure = 1 / FI
  # Inputs : vector of failure times (FT)
  # Outputs : vector of average times to next failure
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Failure Number, MTTF, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : Verify the formula for Duane MTTF
  #==================================================================================
  n <- length(d$FT)
  fail_number <- c(1:n)
  
  MTTF <- 1 / (params$DU_alpha * params$DU_beta * d$FT^(params$DU_beta - 1))
  
  r <- data.frame("Failure_Number" = fail_number, "MTTF" = MTTF, "Model" = rep("DU", n))
  r
} # DU_MTTF


### ------- Reliability Functions (not applicable) -------

DU_R <- function(params, d) {
  #----------------------------------------------------------------------------------
  # As the Duane model is a power model, it does not have a Reliability function R(t)
  # While the initial failure is Weibull distributed, the rest follow the FI
  # 
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  #
  # @returns    (data.frame)    data.frame of Time, Reliability, and Model columns
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  # Currently returns all zero values
  reli <- rep(0, length(d$FT))
  
  r <- data.frame("Time" = d$FT, "Reliability" = reli, "Model" = rep("DU", n))
  r
} # DU_R


DU_R_delta <- function(params, cur_time, delta) {
  #----------------------------------------------------------------------------------
  # Calculates a change in reliability from time t to time t + delta t.
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @cur_time   (numeric)       numeric for the current time
  # @delta      (numeric)       numeric for the time difference, delta
  #
  # @returns    (numeric)       change in Reliability
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  exp(DU_MVF_cont(params, cur_time) - DU_MVF_cont(params, cur_time + delta))
} # DU_R_delta


DU_R_MLE_root <- function(params, cur_time, delta, reliability) {
  #----------------------------------------------------------------------------------
  # As the Duane model is a power model, it does not have a Reliability function R(t)
  # nor a Hazard function z(t)
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @cur_time   (numeric)       numeric for the current time
  # @delta      (numeric)       numeric for the mission time, delta
  # @reliability(numeric)       numeric for the desired reliability
  #
  # @returns    (numeric)       root finding expression for Target_T function
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  # Return NaN
  NA
} # DU_R_MLE_root


DU_Target_T <- function(params, cur_time, delta, reliability) {
  #----------------------------------------------------------------------------------
  # As the Duane model is a power model, it does not have a Reliability function R(t)
  # nor a Hazard function z(t)
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @cur_time   (numeric)       numeric for the current time
  # @delta      (numeric)       numeric for the mission time, delta
  # @reliability(numeric)       numeric for the desired reliability
  #
  # @returns    (numeric)       time to target Reliability, if target is already
  #                             achieved return a string
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  # Return NaN
  NA
} # DU_Target_T


DU_R_growth <- function(params, d, delta) {
  #----------------------------------------------------------------------------------
  # As the Duane model lacks a Reliability function, its change over time cannot be
  # plotted
  #
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @d          (data.frame)    data.frame of data FT,FC,FN,CFC,IF
  # @delta      (numeric)       numeric for the mission time, delta
  #
  # @returns    (numeric)       Reliability growth over time data for plotting
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : 
  #==================================================================================
  # Return NaN
  r <- data.frame("Time" = d$FT, "Reliability_Growth" = rep(NaN, length(d$FT)), "Model" = rep("DU", length(d$FT)))

  r
} # DU_R_growth


### ------- Cost Functions -------

DU_OR_CC <- function(params, c1, c2, c3) {
  #----------------------------------------------------------------------------------
  # Calculates the optimal release time for the Duane model
  #
  # Release : t = 
  # Inputs : Costs
  # Outputs : Time for optimal release of software
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
  # @c1         (numeric)       numeric for the cost of removing fault in testing
  # @c2         (numeric)       numeric for the cost of removing fault in operation
  # @c3         (numeric)       numeric for the cost per unit time of testing
  #
  # @returns    (numeric)       Optimal release time
  #----------------------------------------------------------------------------------
  # Source : 
  #----------------------------------------------------------------------------------
  # TODO : Figure out the mathematics behind this, currently just a placeholder using
  #        the strcture from GO renamed for DU.
  #==================================================================================
  if (params$DU_alpha * params$DU_beta <= (c3 / (c2 - c1))) {
    NA
  } else {
    NA
  }
} # DU_OR_CC


DU_OR_RC <- function(params, x, R) {
  #----------------------------------------------------------------------------------
  # Calculates the optimal time to achieve target reliability
  #
  # Reli : t = 
  # Inputs : Mission time (x) and target reliability (R)
  # Outputs: Time to target reliability
  #----------------------------------------------------------------------------------
  # @params     (data.frame)    data.frame of parameters alpha and beta
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
} # DU_OR_RC


DU_cost <- function(params, c1, c2, c3, t, t_lifecycle) {
  #----------------------------------------------------------------------------------
  # Calculates the total cost at the optimal release time determined in DU_OR_CC
  #
  # Cost : Cost = (c1-1)*MVF(t) + c2*MVF(t_lifecycle) + c3*t
  # Inputs : Costs, optimal release time, and lifecycle duration
  # Outputs : Cost at optimal release time
  #----------------------------------------------------------------------------------
  # @params      (data.frame)   data.frame of parameters alpha and beta
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
  (c1 - 1) * DU_MVF_cont(params, t) + c2 * DU_MVF_cont(params, t_lifecycle) + c3 * t
} # DU_cost





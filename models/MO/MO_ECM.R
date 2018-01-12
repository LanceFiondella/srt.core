# Uncomment the next line if using the first set of exprs that use the Deriv() method
#library('Deriv') # Expression derivation package used in ECM implementation


# Taken from the Reliability package source code
# Default init and tol values should be fine on most machines, they cover the precision used
# by the optimization function
MO_BM_FT_MLE <- function (t, init = c(0, 1), tol = .Machine$double.eps^.25)
{
  theta0hat <- function(theta1, t) {
    n <- length(t)
    t <- cumsum(t)
    tn <- t[length(t)]
    return(n/log(1 + theta1 * tn))
  }
  theta1hat <- function(theta1, t) {
    i <- seq(along = t)
    n <- length(t)
    t <- cumsum(t)
    tn <- t[length(t)]
    return((1/theta1 * sum(1/(1 + theta1 * t[i])) - n * tn/((1 +
                                                               theta1 * tn) * log(1 + theta1 * tn)))^2)
  }
  min <- optimize(theta1hat, init, tol = tol, t = t)
  theta1 <- min$minimum
  theta0 <- theta0hat(theta1, t)
  return(data.frame("MO_theta0"=theta0,"MO_theta1"=theta1))
}

# Maximum likelihood estimate for parameters using Expectation Condition Maximization
MO_ECM_FT_MLE = function(t, init_est = c(10, 10), tol = .Machine$double.eps^.5)
{
  # These initial guesses were arbitrarily made through process of elimination 
  aroot = init_est[1] # initial guess for parameter a
  broot = init_est[2] # initial guess for parameter b
  
  # Function for log-likelihood formula
  lnL <- function(a, b, t)
  {
    -log(b * a * t[length(t)] + 1)/a + sum(log(b/(1 + a * b * t)))
  }
  
  # Define expression for partial derivatives of lnL with respect to a and b
  # These require the 'Deriv.R' package to provide a derivative
  #expra = Deriv(expression(-log(b * a * tn + 1)/a + sum(log(b/(1 + a * b * t)))), 'a')
  #exprb = Deriv(expression(-log(b * a * tn + 1)/a + sum(log(b/(1 + a * b * t)))), 'b')
  
  # These are directly derived and so do not depend on 'Deriv.R'
  expra = expression(1/a^2 * log(a * b * tn + 1) - 1/a * (b * tn)/(a * b * tn + 1) - sum((b * t)/(1 + a * b * t)))
  exprb = expression(-tn/(a * b * tn + 1) + sum(1/(b * (1 + a * b * t))))
  
  # Functions to evaluate expressions of partial derivatives
  dlnLa = function(a, b, t, exprs)
  {
    tn = t[length(t)]
    return(eval(exprs))
  }
  dlnLb = function(b, a, t, exprs) # b is first param because uniroot optimizes first param
  {
    tn = t[length(t)]
    return(eval(exprs))
  }
  
  # Initial log-likelihood value
  LL = lnL(aroot, broot, t)
  if(!is.finite(LL))
  {
    warning("Initial Log-likelihood estimate is not finite, setting to 0")
    LL = 0;
  }
  
  # Initialize while loop control variables
  LLerror = 1;
  j = 2;
  iter = 1;
  maxiter = 1000;
  
  # While loop is an implementation of Expectation Condition Maximization
  # First we make an arbitrary guess for the a and b parameter and evaluate the LL at that guess
  # Then we take the partial derivative with respect to a, and plug our previous b guess into it,
  #   minimizing the function by solving for a where f(a) = 0
  # The a minimization is plugged into the partial derivative with respect to b, which is then
  #   minimized in the same way.
  # The LL is re-evaluated using the a and b minimized estimates
  # The process is repeated until the change in LL (LLerror) is less than or equal to an arbitrary
  #   tolerance
  while(LLerror > tol) # arbitrarily small number
  {
    # Estimate the root of the a derivative using previous b derivative output
    tryCatch(
      {
        aroot[j] = uniroot(dlnLa, c(10^-8, 10), b = broot[j - 1], t = t, exprs = expra, extendInt="yes", tol = 1e-10)$root
        # Estimate the root of the b derivative using the a derivative output from the line above
        broot[j] = uniroot(dlnLb, c(10^-8, 10), a = aroot[j], t = t, exprs = exprb, extendInt="yes", tol = 1e-10)$root
      },
      warning = function(war)
        {
        # Don't do anything about warnings
        },
      error = function(err) # If uniroot fails to converge, don't crash entire tool
        {
          warning("Uniroot in ECM algorithm failed")
          return(data.frame("MO_theta0" = NA, "MO_theta1" = NA))
        })
    # Log-likelihood value of current a and b roots
    LL[j] = lnL(aroot[j], broot[j], t)

    # Sometimes our a + b guess produces a NaN in the lnL
    if(is.finite(LL[j]))
    {
      # Once the difference between the current log-likelihood and the previous are sufficiently small,
      # assume the current parameter estimates do a reasonable job of minimizing the lnL function
      LLerror = LL[j] - LL[j-1]
      j = j + 1;
    }
    
    iter = iter + 1;
    if(iter >= maxiter) # Avoid infinite loop in while
    {
      warning("Reached maxiterations in ECM, unable to optimize function")
      # SRT looks for parameters of type character in the case of non-convergence
      #return(data.frame("MO_theta0" = "Not Converged", "MO_theta1" = "Not Converged"))
      # Tool was changed to look for NA to symbolize non-convergence
      return(data.frame("MO_theta0" = NA, "MO_theta1" = NA))
    }
  }
  # Final estimate assumed good enough
  a = aroot[length(aroot)]
  b = broot[length(broot)]
  # Redefine parameters to match Lyu chapter 3 where theta0 = 1/theta and theta1 = lambda * theta
  return(data.frame("MO_theta0" = 1/a, "MO_theta1" = a * b))
}

# Taken from the Reliability package source code
MO_MVF <- function (param, d) 
{
  n <- length(d$FT)
  
  theta0 = param$MO_theta0
  theta1 = param$MO_theta1
  
  if (length(theta0) != 1 || length(theta1) != 1) {
    stop("theta0 and theta1 should have length 1")
  }
  
  mvf = theta0 * log(theta1 * d$FT + 1)
  
  r = data.frame(mvf, d$FT, rep("MO", n))
  names(r) = c("Failure", "Time", "Model")
  return(r)
}

# Analytic solution for the inverse of the MVF function 
MO_MVF_inv <- function(param, d)
{
  n <- length(d$FN)
  r <- data.frame()

  theta1 = param$MO_theta1
  theta0 = param$MO_theta0
  
  cumFailTimes = (exp(d$FN/theta0))/(theta1) - 1/(theta1)

  r = data.frame("Failure"=d$FN, "Time"=cumFailTimes, "Model"=rep("MO", n))
  return(r)
}

# MVF evaluated at a single time instead of a vector
MO_MVF_cont <- function(params, t)
{
  theta0 = params$MO_theta0
  theta1 = params$MO_theta1
  return(mvf = theta0 * log(theta1 * t + 1))
}

# Log-likelihood function 
# Calculated as the negative MVF at the nth value of t plus 
# the summation of the natural log of the derivative of the 
# MVF with respect to t
MO_lnL <- function(x,params)
{ 
  n <- length(x)
  tn <- x[n]
  
  theta0 = params$MO_theta0
  theta1 = params$MO_theta1
  
  # First term is -(MVF(tn)) where tn is the final t value
  firstTerm <- - MO_MVF_cont(params, tn)
  # Second term is ln(FI(x))
  lnL <- firstTerm + sum(log((theta0 * theta1)/(theta1*x + 1)))
  lnL
}

# Reliability growth function
# MO_R <- function(params,d){ #CHANGE THIS
#   n <- length(d$FT)
#   r <-data.frame()
#   cumulr <-data.frame()
#   print("Calling MO_R")
#   for(i in 1:n){
#     r[i,1] <- d$FT[i]
#     r[i,2] <- exp(-params$GO_bMLE*d$FT[i])
#     r[i,3] <- "MO"
#   }
#   r <- data.frame(r[1],r[2],r[3])
#   names(r) <- c("Time","Reliability","Model")
#   r
# }

# Change in reliability with time
# As in Lyu Chapter 3, the reliability is given by exp(-mvf(t+delta)-mvf(t)) for a NHPP
MO_R_delta <- function(params,cur_time,delta)
{ 
  temp = exp(-(MO_MVF_cont(params, (cur_time + delta)) - MO_MVF_cont(params,cur_time)))
  if(anyNA(temp))
  {
    temp = 0;
    warning('MO_MVF_cont is contains NAs')
  }
  return(temp)
}

# Time to target reliability
MO_Target_T <- function(params, cur_time, delta, reliability)
{ 
  maxiter <- 1000
  f <- function(t){
    return(MO_R_MLE_root(params, t, delta, reliability))
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
      local_rel <- MO_R_delta(params, interval_right, delta)
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
            MO_Target_T(a,b,cur_time,delta, reliability)
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

# Inverse of the first derivative of the MVF
MO_MTTF <- function(params,d)
{ 
  n <- length(d$FT)
  r <-data.frame()
  
  theta0 = params$MO_theta0
  theta1 = params$MO_theta1
  lambda = theta1*theta0
  
  mttf = (theta1*d$FT + 1)/lambda
  return(data.frame("Failure_Number"=1:n, "MTTF"=mttf, "Model"=rep("MO", n)))
}

# This equation is taken from Chapter 3 of Lyu's Handbook for Software Reliability
MO_FI <- function(param, d)
{
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)
  
  theta0 = param$MO_theta0
  theta1 = param$MO_theta1
  failIntensity = (theta0 * theta1)/(theta1*d$FT + 1)
  
  r = data.frame(fail_number, failIntensity, rep("MO", n))
  names(r) = c("Failure_Count", "Failure_Rate", "Model")
  return(r)
}

MO_R_MLE_root <- function(params,cur_time,delta, reliability)
{ 
  theta0 = params$MO_theta0
  theta1 = params$MO_theta1
  root_equation <- reliability - exp(theta0*log(theta1*cur_time + 1) - theta0*log(theta1*(cur_time+delta)+1))
  return(root_equation)
}

MO_R_growth <- function(params,d,delta)
{
  r <-data.frame()
  for(i in 1:length(d$FT)){   
    r[i,1] <- d$FT[i]
    temp <- MO_R_delta(params,d$FT[i],delta)
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
      r[i,3] <- "MO"
    }
    else{
      r[i,2] <- "NA"
      r[i,3] <- "MO"
    }     
  }
  g <- data.frame(r[1], r[2], r[3])
  names(g) <- c("Time","Reliability_Growth","Model")
  ##print(g)
  g
}

# TODO: Figure out the OR_CC, OR_RC, and cost
# Everything below is a guess. Do not trust these values. I was not sure
# how to derive these functions and needed some place holder code. 
MO_OR_CC<-function(params,c1,c2,c3){
  if (params$MO_theta1*params$MO_theta0 <= (c3/(c2-c1))){
    t_opt=0
  }
  else{
    t_opt=(1/params$MO_theta1) * log((params$MO_theta0 * params$MO_theta1 * (c2-c1))/c3)
  }
  return(t_opt)
}

MO_OR_RC<-function(params,x,R){
  s=(1/params$MO_theta1)*((log(params$MO_theta0 * log(params$MO_theta1 * x + 1))-log(log(1/R))))
  return (s)
}

MO_cost <- function(params, C1,C2,C3,t, t_lifecycle){
  return(C1*MO_MVF_cont(params,t) + C2*(MO_MVF_cont(params,t_lifecycle) - MO_MVF_cont(params,t)) + C3*t)
}
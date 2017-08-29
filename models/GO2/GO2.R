
GO2_makedata_FT <- function(x) {
  faultdata.time(time=diff(c(0,x)), te=0)
}

GO2_EM_MLE <- function(x){
  data <- GO2_makedata_FT(x)
  result <- emfit(srm("exp"), data)
  data.frame(GO2_aMLE=result$srm$params[1], GO2_bMLE=result$srm$params[2])
}

GO2_MVF <- function(param, d) {
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  MVF <- model$mvf(d$FT)
  data.frame(Failure=MVF, Time=d$FT, Model="GO2")
}

GO2_MVF_inv <- function(param, d) {
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  ft <- model$inv_mvf(d$FN)
  ft[is.na(ft)] <- 0
  data.frame(Failure=d$FN, Time=ft, Model="GO2")
}

GO2_MTTF <- function(param, d){
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  fail_number <- 1:length(d$FT)
  mttf <- 1/model$intensity(d$FT)
  data.frame(Failure_Number=fail_number, MTTF=mttf, Model="GO2")
}

GO2_FI <- function(param, d) {
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  fail_number <- 1:length(d$FT)
  failIntensity <- model$intensity(d$FT)
  data.frame(Failure_Count=fail_number, Failure_Rate=failIntensity, Model="GO2")
}

GO2_R <- function(param, d){
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  reli <- model$reliab(d$FT, s=0)
  data.frame(Time=d$FT, Reliability=reli, Model="GO2")
}

GO2_lnL <- function(x, param){
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  data <- GO2_makedata_FT(x)
  retval <- model$llf(data)
  print(retval)
  retval
}

GO2_MVF_cont <- function(param, t){
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  model$mvf(t)
}

GO2_R_delta <- function(param, cur_time, delta){
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  model$reliab(delta, cur_time)
}

GO2_R_MLE_root <- function(params,cur_time,delta, reliability){
  root_equation <- reliability - exp(params$GO2_aMLE*(1-exp(-params$GO2_bMLE*cur_time)) -params$GO2_aMLE*(1-exp(-params$GO2_bMLE*(cur_time+delta))))
  return(root_equation)
}

maxiter <- 1000
GO2_Target_T <- function(params,cur_time,delta, reliability){

  f <- function(t){
    return(GO2_R_MLE_root(params,t,delta, reliability))
  }

  current_rel <- GO2_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval

    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- GO2_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- GO2_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (GO2_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
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
            GO2_Target_T(a,b,cur_time,delta, reliability)
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

GO2_R_growth <- function(param, d, delta) {
  model <- ExpSRM$new(param$GO2_aMLE, param$GO2_bMLE)
  reli <- model$reliab(t=delta, s=d$FT)
  data.frame(Time=d$FT, Reliability_Growth=reli, Model="GO2")
}

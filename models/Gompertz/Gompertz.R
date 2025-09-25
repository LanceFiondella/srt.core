
Gompertz_makedata_FT <- function(x) {
  faultdata(time=diff(c(0,x)), te=0)
}

Gompertz_makemodel_FT <- function(params) {
  TXVMaxSRM$new(params$Gompertz_aMLE, params$Gompertz_bMLE, params$Gompertz_cMLE)
}

Gompertz_EM_MLE <- function(x){
  data <- Gompertz_makedata_FT(x)
  result <- emfit(srm("txvmax"), data)
  data.frame(Gompertz_aMLE=result$srm$params[1], Gompertz_bMLE=result$srm$params[2], Gompertz_cMLE=result$srm$params[3])
}

Gompertz_MVF <- function(params, d) {
  model <- Gompertz_makemodel_FT(params)
  MVF <- model$mvf(d$FT)
  data.frame(Failure=MVF, Time=d$FT, Model="Gompertz")
}

Gompertz_MVF_inv <- function(params, d) {
  model <- Gompertz_makemodel_FT(params)
  ft <- model$inv_mvf(d$FN)
  ft[is.na(ft)] <- 0
  data.frame(Failure=d$FN, Time=ft, Model="Gompertz")
}

Gompertz_MTTF <- function(params, d){
  model <- Gompertz_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  mttf <- 1/model$intensity(d$FT)
  data.frame(Failure_Number=fail_number, MTTF=mttf, Model="Gompertz")
}

Gompertz_FI <- function(params, d) {
  model <- Gompertz_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  failIntensity <- model$intensity(d$FT)
  data.frame(Failure_Count=fail_number, Failure_Rate=failIntensity, Model="Gompertz")
}

# Gompertz_R <- function(params, d){
#   model <- Gompertz_makemodel_FT(params)
#   reli <- model$reliab(d$FT, s=0)
#   data.frame(Time=d$FT, Reliability=reli, Model="Gompertz")
# }

Gompertz_lnL <- function(x, params){
  model <- Gompertz_makemodel_FT(params)
  data <- Gompertz_makedata_FT(x)
  model$llf(data)
}

Gompertz_MVF_cont <- function(params, t){
  model <- Gompertz_makemodel_FT(params)
  model$mvf(t)
}

Gompertz_Target_T <- function(params, cur_time, delta, reliability) {
  model <- Gompertz_makemodel_FT(params)

  f <- function(t, p) {
    if (t < cur_time) {
      NULL
    } else {
      (model$reliab(delta, t) - p)^2
    }
  }

  rightmax <- 10*cur_time
  current_rel <- model$reliab(delta, cur_time)
  rightmax_rel <- model$reliab(delta, rightmax)
  if (current_rel >= reliability) {
    return("Target reliability already achieved")
  }
  if (rightmax_rel < reliability) {
    return(Inf)
  }
  optimize(f=f, interval=c(cur_time, 10*cur_time), p=reliability)$minimum
}

Gompertz_R_growth <- function(params, d, delta) {
  model <- Gompertz_makemodel_FT(params)
  reli <- model$reliab(t=delta, s=d$FT)
  data.frame(Time=d$FT, Reliability_Growth=reli, Model="Gompertz")
}

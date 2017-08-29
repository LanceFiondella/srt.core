
TNorm_makedata_FT <- function(x) {
  faultdata.time(time=diff(c(0,x)), te=0)
}

TNorm_makemodel_FT <- function(params) {
  TNormSRM$new(params$TNorm_aMLE, params$TNorm_bMLE, params$TNorm_cMLE)
}

TNorm_EM_MLE <- function(x){
  data <- TNorm_makedata_FT(x)
  result <- emfit(srm("tnorm"), data)
  data.frame(TNorm_aMLE=result$srm$params[1], TNorm_bMLE=result$srm$params[2], TNorm_cMLE=result$srm$params[3])
}

TNorm_MVF <- function(params, d) {
  model <- TNorm_makemodel_FT(params)
  MVF <- model$mvf(d$FT)
  data.frame(Failure=MVF, Time=d$FT, Model="TNorm")
}

TNorm_MVF_inv <- function(params, d) {
  model <- TNorm_makemodel_FT(params)
  ft <- model$inv_mvf(d$FN)
  ft[is.na(ft)] <- 0
  data.frame(Failure=d$FN, Time=ft, Model="TNorm")
}

TNorm_MTTF <- function(params, d){
  model <- TNorm_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  mttf <- 1/model$intensity(d$FT)
  data.frame(Failure_Number=fail_number, MTTF=mttf, Model="TNorm")
}

TNorm_FI <- function(params, d) {
  model <- TNorm_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  failIntensity <- model$intensity(d$FT)
  data.frame(Failure_Count=fail_number, Failure_Rate=failIntensity, Model="TNorm")
}

# TNorm_R <- function(params, d){
#   model <- TNorm_makemodel_FT(params)
#   reli <- model$reliab(d$FT, s=0)
#   data.frame(Time=d$FT, Reliability=reli, Model="TNorm")
# }

TNorm_lnL <- function(x, params){
  model <- TNorm_makemodel_FT(params)
  data <- TNorm_makedata_FT(x)
  model$llf(data)
}

TNorm_MVF_cont <- function(params, t){
  model <- TNorm_makemodel_FT(params)
  model$mvf(t)
}

TNorm_Target_T <- function(params, cur_time, delta, reliability) {
  model <- TNorm_makemodel_FT(params)

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

TNorm_R_growth <- function(params, d, delta) {
  model <- TNorm_makemodel_FT(params)
  reli <- model$reliab(t=delta, s=d$FT)
  data.frame(Time=d$FT, Reliability_Growth=reli, Model="TNorm")
}

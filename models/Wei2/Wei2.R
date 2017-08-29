
Wei2_makedata_FT <- function(x) {
  faultdata.time(time=diff(c(0,x)), te=0)
}

Wei2_makemodel_FT <- function(params) {
  LXVMinSRM$new(params$Wei2_aMLE, params$Wei2_bMLE, params$Wei2_cMLE)
}

Wei2_EM_MLE <- function(x){
  data <- Wei2_makedata_FT(x)
  result <- emfit(srm("lxvmin"), data)
  data.frame(Wei2_aMLE=result$srm$params[1], Wei2_bMLE=result$srm$params[2], Wei2_cMLE=result$srm$params[3])
}

Wei2_MVF <- function(params, d) {
  model <- Wei2_makemodel_FT(params)
  MVF <- model$mvf(d$FT)
  data.frame(Failure=MVF, Time=d$FT, Model="Wei2")
}

Wei2_MVF_inv <- function(params, d) {
  model <- Wei2_makemodel_FT(params)
  ft <- model$inv_mvf(d$FN)
  ft[is.na(ft)] <- 0
  data.frame(Failure=d$FN, Time=ft, Model="Wei2")
}

Wei2_MTTF <- function(params, d){
  model <- Wei2_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  mttf <- 1/model$intensity(d$FT)
  data.frame(Failure_Number=fail_number, MTTF=mttf, Model="Wei2")
}

Wei2_FI <- function(params, d) {
  model <- Wei2_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  failIntensity <- model$intensity(d$FT)
  data.frame(Failure_Count=fail_number, Failure_Rate=failIntensity, Model="Wei2")
}

# Wei2_R <- function(params, d){
#   model <- Wei2_makemodel_FT(params)
#   reli <- model$reliab(d$FT, s=0)
#   data.frame(Time=d$FT, Reliability=reli, Model="Wei2")
# }

Wei2_lnL <- function(x, params){
  model <- Wei2_makemodel_FT(params)
  data <- Wei2_makedata_FT(x)
  model$llf(data)
}

Wei2_MVF_cont <- function(params, t){
  model <- Wei2_makemodel_FT(params)
  model$mvf(t)
}

Wei2_Target_T <- function(params, cur_time, delta, reliability) {
  model <- Wei2_makemodel_FT(params)

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

Wei2_R_growth <- function(params, d, delta) {
  model <- Wei2_makemodel_FT(params)
  reli <- model$reliab(t=delta, s=d$FT)
  data.frame(Time=d$FT, Reliability_Growth=reli, Model="Wei2")
}


GO2_makedata_FT <- function(x) {
  faultdata.time(time=diff(c(0,x)), te=0)
}

GO2_makemodel_FT <- function(params) {
  ExpSRM$new(params$GO2_aMLE, params$GO2_bMLE)
}

GO2_EM_MLE <- function(x){
  data <- GO2_makedata_FT(x)
  result <- emfit(srm("exp"), data)
  data.frame(GO2_aMLE=result$srm$params[1], GO2_bMLE=result$srm$params[2])
}

GO2_MVF <- function(params, d) {
  model <- GO2_makemodel_FT(params)
  MVF <- model$mvf(d$FT)
  data.frame(Failure=MVF, Time=d$FT, Model="GO2")
}

GO2_MVF_inv <- function(params, d) {
  model <- GO2_makemodel_FT(params)
  ft <- model$inv_mvf(d$FN)
  ft[is.na(ft)] <- 0
  data.frame(Failure=d$FN, Time=ft, Model="GO2")
}

GO2_MTTF <- function(params, d){
  model <- GO2_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  mttf <- 1/model$intensity(d$FT)
  data.frame(Failure_Number=fail_number, MTTF=mttf, Model="GO2")
}

GO2_FI <- function(params, d) {
  model <- GO2_makemodel_FT(params)
  fail_number <- 1:length(d$FT)
  failIntensity <- model$intensity(d$FT)
  data.frame(Failure_Count=fail_number, Failure_Rate=failIntensity, Model="GO2")
}

# GO2_R <- function(params, d){
#   model <- GO2_makemodel_FT(params)
#   reli <- model$reliab(d$FT, s=0)
#   data.frame(Time=d$FT, Reliability=reli, Model="GO2")
# }

GO2_lnL <- function(x, params){
  model <- GO2_makemodel_FT(params)
  data <- GO2_makedata_FT(x)
  model$llf(data)
}

GO2_MVF_cont <- function(params, t){
  model <- GO2_makemodel_FT(params)
  model$mvf(t)
}

GO2_Target_T <- function(params, cur_time, delta, reliability) {
  model <- GO2_makemodel_FT(params)

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

GO2_R_growth <- function(params, d, delta) {
  model <- GO2_makemodel_FT(params)
  reli <- model$reliab(t=delta, s=d$FT)
  data.frame(Time=d$FT, Reliability_Growth=reli, Model="GO2")
}

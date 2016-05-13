#Vector of failure times data
library(rootSolve)
#rm(list=ls())
#tVec <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416,50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409, 76057, 81542, 82702, 84566, 88682)

#tVec <- c(479, 745, 1022, 1576, 2610, 2859,3552, 4149, 4266, 4436, 4553, 5827, 6296, 7470, 8163, 10071, 10206, 10483, 11079, 11836, 12273, 14503, 14940, 15280, 15685, 16220, 16497, 16860, 17382, 17995, 18272, 19572, 20393, 20606, 22226, 23827, 24125, 24999, 25617, 28257, 28262, 28411, 29445, 31886, 32346, 32911, 34030, 34467, 35394, 39856, 40570, 40751, 42236, 42993, 46147, 48262, 49146, 51183, 52664, 53223, 53713, 54306, 56075, 56160, 58996, 59209, 61075, 61565, 63052, 67374, 68792, 69815, 75305, 76825, 80106, 82822, 84997, 88502, 89227, 91190, 95169, 96259, 96504, 97698, 98692, 102594)

#tVec <- c(39, 49, 53, 89, 93, 98, 102, 193, 242, 243, 268, 269, 273, 303, 345, 354, 403, 447, 479, 482, 560, 561, 591, 796, 801, 930, 1033, 1257, 1443, 1496, 1510, 1519, 1521, 1531, 1532, 1566, 1736, 1865, 1869, 1873, 1908, 1913, 1918, 1940, 1976, 2011, 2132, 2155, 2188, 2236, 2268, 2289, 2293, 2316, 2325, 2338, 2503, 2517, 2539, 2580, 2592, 2730, 2825, 2874, 2936, 2938, 2973, 3062, 3152, 3221, 3243, 3258, 3277, 3319, 3333, 3344, 3385, 3595, 3611, 3641, 3678, 3744, 3753, 3769, 3783, 3807, 3819, 3978, 4067, 4185, 4214, 4235, 4253, 4255, 4369, 4406, 4452, 4469, 4470, 4620, 5002, 5162, 5228, 5434, 5443, 5469, 5531, 5770, 5783, 5787, 5872, 5957, 6197, 6375, 6409, 6511, 6520, 6666, 6725, 6773, 6798, 6823, 6934, 6939, 6970, 7021, 7027, 7220, 7247, 7272, 7368, 7394, 7424, 7454, 7471, 7791, 7869, 7908, 7921, 7934,  7953, 8081, 8115, 8199, 8239, 8416, 8765, 9039, 9121, 9179, 9210, 9324, 9363, 9451, 9535, 9767, 9875, 9913, 9999, 10006, 10028, 10108, 10347, 10350, 10389, 10452, 10604, 10667, 10747, 10992, 11188, 11234, 11386, 11488, 11497, 11725, 11945, 12153, 12231, 12234, 12317, 12323, 12535, 12626, 12629, 12639, 12811, 12832, 13005, 13376, 13416, 13464, 13590, 13680, 13829, 13859, 14176, 14676, 15349, 15781, 15847, 16015, 16081, 16147, 16275, 16324, 16656)

ISS_NM_MLE <- function(tVec){
  tVec <- as.numeric(tVec)
  n <- length(tVec)
  tn <- tVec[n]
  sumT <- sum(tVec)
  
  #estimate starting point for 'b'
  b0 <- (n/sumT)
  c0 <- 1
  a0 <- n
  
  #(*MLE equation of a, b, and c*)
  
  #model1 routine will have the MLE equations defined as F1, F2, and F3. a,b,c are denoted as x[1],x[2], and x[3] here
  
  model1 <- function(x) {
    sumi <- c(0,0)
    for(i in 1:n)
    {
      sumi[1] <- sumi[1] + (1/x[2])-((2*tVec[i]*exp(x[2]*tVec[i]))/(x[3]+exp(x[2]*tVec[i])))+tVec[i]  
      sumi[2] <- sumi[2] + (-2/(x[3]+exp(x[2]*tVec[i])))+(1/(1+x[3]))
    }
    #print(x)
    
    c(F1 = ((1-exp(x[2]*tn)+(((x[3]+exp(x[2]*tn))*n)/x[1]))/(x[3]+exp(x[2]*tn))),
      F2 = ((-x[1]*(1+x[3])*tn*exp(x[2]*tn))/((x[3]+exp(x[2]*tn))^2)) + sumi[1],
      F3 = (x[1]*(-1+exp(x[2]*tn))/((x[3]+exp(x[2]*tn))^2)) + sumi[2])
  }
  
  #Use the multiroot command to update three equtions simultaneously
  abc <- rootSolve::multiroot(f=model1,start=c(a0,b0,c0), ctol = 1e-10)$root
  #print(abc)
  
  params <- data.frame(abc[1],abc[2],abc[3])
  names(params) <- c(paste("ISS",ISS_params[1],sep="_"),paste("ISS",ISS_params[2],sep="_"),paste("ISS",ISS_params[3],sep="_"))
  
  return(params)
}
# 
# ISS_NM_MLE <- function(x){
#   ISS_params <- data.frame(
#     "ISS_aMLE"=aMLE,
#     "ISS_bMLE"=bMLE,
#     "ISS_cMLE"=cMLE)
#   return(ISS_params)
# }

ISS_MVF <- function(params, d )
{ 
  
  n <- length(d$FT)
  r <- data.frame()
  MVF <- param$ISS_aMLE*(1-exp(-param$ISS_bMLE*d$FT))/(1+param$ISS_cMLE*exp(-param$ISS_bMLE*d$FT))
  r <- data.frame(MVF,d$FT,rep("ISS", n))
  names(r) <- c("Failure","Time","Model")
  return(r)
}

ISS_MVF_inv <- function(params, d )
{ 
  
  n <- length(d$FN)
  r <- data.frame()
  cumFailTimes <- -(log((param$ISS_aMLE-d$FN)/(param$ISS_aMLE+params$ISS_cMLE*d$FN)))/param$ISS_bMLE
  r <- data.frame(d$FN,cumFailTimes,rep("ISS", n))
  names(r) <- c("Failure","Time","Model")
  return(r)
}

ISS_MVF_cont <- function(param, t){
  return(param$ISS_aMLE * (1-exp(-param$ISS_bMLE*t)/(1 + param$ISS_cMLE * exp(-param$ISS_bMLE*t))))
}

ISS_FI <- function(params,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- d$FT[i]
    r[i,2] <- (params$ISS_aMLE*params$ISS_bMLE*(params$ISS_cMLE + 1)*exp(params$ISS_bMLE*d$FT[i]))/((params$ISS_cMLE + exp(params$ISS_bMLE*d$FT[i]))^2)
    r[i,3] <- "ISS"
  }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r
}

ISS_MTTF <- function(params,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
    r[i,2] <- 1/(params$ISS_aMLE*params$ISS_bMLE*(params$ISS_cMLE + 1)*exp(params$ISS_bMLE*d$FT[i]))/((params$ISS_cMLE + exp(params$ISS_bMLE*d$FT[i]))^2)
    r[i,3] <- "ISS"
  }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Failure_Number","MTTF","Model")
  r
}

ISS_R_delta <- function(params,cur_time,delta){
  return(exp(-(ISS_MVF_cont(params,(cur_time+delta)) - ISS_MVF_cont(params,cur_time))))
}

ISS_lnL <- function(x,params){
  n <- length(x)
  tn <- x[n]
  sum1 <- 0
  for(i in 1:n){
    sum1=sum1+(log(ISS_FI(params,x[i])))
  }
  return(-1*ISS_MVF(params,tn)+sum1)
}



ISS_Target_T <- function(params,cur_time,delta,reliability){
  
  f <- function(t){
    return(ISS_R_MLE_root(params,t,delta,reliability))
  }
  
  current_rel <- ISS_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- ISS_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- ISS_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (ISS_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
        interval_left <- interval_left + (interval_right-interval_left)/2
      }
    } else {
      sol <- Inf
    }
    
    if (is.finite(interval_right) && is.finite(sol)) {
      sol <- tryCatch(
        stats::uniroot(f, c(cur_time,cur_time + 50),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
        warning = function(w){
          #print(f.lower)
          if(length(grep("_NOT_ converged",w[1]))>0){
            maxiter <<- floor(maxiter*1.5)
            #print(paste("recursive", maxiter,sep='_'))
            ISS_Target_T(a,b,cur_time,delta, reliability)
          }
        },
        error = function(e){
          print(e)
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

ISS_R_growth<-function(params,data,delta){
  r<-data.frame()
  for(i in 1:legnth(d$FT)){
    r[i,1]<-d$FT[i]
    temp<-ISS_R_delta(params,d$FT[i],delta)
    if(typeof(temp)!=typeof("character")){
      r[i,2]<-temp
      r[i,3]<-"ISS"
    }
    else{
      r[i,2]<-"NA"
      r[i,3]<-"ISS"
    }
  }
  g<-data.frame(r[1],r[2],r[3])
  names(g) <-c("Time","Reliability_Growth", "Model")
  return(g)
}

ISS_R_MLE_root<-function(params,cur_time,delta,reliability){
  root_equation <-reliability-
    exp(params$ISS_aMLE*(1-exp(-params$ISS_bMLE*cur_time))
        -params$ISS_aMLE*(1-exp(-params$ISS_bMLE*(cur_time+delta))))
  return(root_equation)
}
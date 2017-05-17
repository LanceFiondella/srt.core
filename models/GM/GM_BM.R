#x <- c(3, 33, 146, 227, 342, 351, 353, 444,556, 571, 709, 759, 836, 860, 968,1056, 1726, 1846, 1872, 1986, 2311,2366, 2608, 2676, 3098, 3278, 3288,4434, 5034, 5049, 5085, 5089, 5089,5097, 5324, 5389, 5565, 5623, 6080,6380, 6477, 6740, 7192, 7447, 7644,7837, 7843, 7922, 8738, 10089, 10237,10258, 10491, 10625, 10982, 11175,11411, 11442, 11811, 12559, 12559,12791, 13121, 13486, 14708, 15251,15261, 15277, 15806, 16185, 16229,16358, 17168, 17458, 17758, 18287,18568, 18728, 19556, 20567, 21012,21308, 23063, 24127, 25910, 26770,27753, 28460, 28493, 29361, 30085,32408, 35338, 36799, 37642, 37654,37915, 39715, 40580, 42015, 42045,42188, 42296, 42296, 45406, 46653,47596, 48296, 49171, 49416, 50145,52042, 52489, 52875, 53321, 53443,54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732,64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)




GM_BM_IF_MLE <- function(interFail){
  # TODO :
      # ----> as numeric is prefered if data contain large values Should consider its propagation effect through out
      # ----> should also check its effects on precision.

  interFail <- as.numeric(interFail)  
n <-length(interFail)


MLEeq<-function(phi){
  NrTerm  <- 0
  DrTerm  <- 0
  
  for(i in 1:n){
    NrTerm =NrTerm+(i*(phi^i)*interFail[i])
    DrTerm = DrTerm +((phi^i)*interFail[i])
  }
  phi_MLE <- (NrTerm/DrTerm)-((n+1)/2)
  return(phi_MLE)
}


#Step-1: Determine initial parameter estimate for parameter 'b0'

#b0 <- n/sum(interFail)
#b0 <- n
b0 <- 1.0

#Step-2: Bracket root

i <- 0 
maxIterations <- 200
leftEndPoint <- b0/2
leftEndPointMLE <- MLEeq(leftEndPoint)
rightEndPoint <- 1.2*b0
rightEndPointMLE <- MLEeq(rightEndPoint)

while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
  ##print('In Step 2 while loop of GM_BM.R')
  leftEndPoint <- leftEndPoint/2
  leftEndPointMLE <- MLEeq(leftEndPoint)
  rightEndPoint <- 1.1*rightEndPoint
  rightEndPointMLE <- MLEeq(rightEndPoint)
  i <- i+1  
}

#Step-3: Invoke uniroot or report non convergence to calling environment

if(leftEndPointMLE*rightEndPointMLE > 0 ){
  return('nonconvergence')
} else {


  maxiter <- 20
  soln <- function(maxiter){
    sol <- tryCatch(
      stats::uniroot(MLEeq, c(leftEndPoint,rightEndPoint), maxiter=maxiter, tol=1e-10, extendInt="yes")$root,
      warning = function(w){
      ##print(f.lower)
        if(length(grep("_NOT_ converged",w[1]))>0){
          maxiter <- maxiter+1 
          #print(paste("recursive", maxiter,sep='_'))
          soln(maxiter)
        }
      },
      error = function(e){
        #print(e)
        #return(e)
      })
    sol
  }
  phiMLE <- soln(maxiter)

  if(phiMLE < 0){
    return('nonconvergence')
  }



  #phiMLE <- stats::uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, extendInt="yes", tol = 1e-10)$root
}
##print(phiMLE)

#Step-4
#MLE of parameter 'D'
DrTerm = 0
for(i in 1:n){
  DrTerm = DrTerm +((phiMLE^i)*interFail[i])
}
D_MLE <- (phiMLE*n)/DrTerm

##print(D_MLE)
params <- data.frame("GM_D0"=D_MLE,"GM_Phi"=phiMLE)
params
}

GM_MVF <- function(param,d){
 n <- length(d$FT)
 r <- data.frame()
 beta <- -log(param$GM_Phi)
# t_index <- seq(d$FT[1],d$FT[n],(d$FT[n]-d$FT[1])/100)
# for(i in 1:length(t_index)){
#   r[i,1] <- t_index[i]
#   r[i,2] <- param$D0*(1-exp(-1*t_index[i]*param$Phi))
# }
 MVF <- (1/beta)*log((param$GM_D0*d$FT*beta/param$GM_Phi)+1)
 r <- data.frame(d$FT,MVF,rep("GM", n))
 names(r) <- c("Time","Failure","Model")
 r
} 



GM_MVF_inv <- function(param,d){
  n <- length(d$FN)
  r <-data.frame()
  beta <- -log(param$GM_Phi)
  cumFailTimes <- (exp(-beta)*(exp(beta*d$FN) - 1))/(param$GM_D0*beta)
  r <- data.frame(d$FN,cumFailTimes, rep("GM", n))
  names(r) <- c("Failure","Time","Model")
  r
}

GM_MTTF <- function(param,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
    r[i,2] <- 1/(param$GM_D0*(param$GM_Phi)^i)
    }
  r <- data.frame(r[1],r[2],rep("GM", n))
  names(r) <- c("Failure_Number","MTTF","Model")
  r  
}

GM_FI <- function(param,d){
  n <- length(d$FT)
  r <-data.frame()
  fail_count <- c(1:n)
  beta <- -log(param$GM_Phi)
  failIntensity <- (param$GM_D0/param$GM_Phi)/((beta*param$GM_D0/param$GM_Phi)*d$FT + 1)
  r <- data.frame(fail_count,failIntensity,rep("GM", n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r
}

GM_R <- function(param,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- d$FT[i]
    r[i,2] <- exp((-1*param$GM_D0*(param$GM_Phi^i)*d$FT[i]))
    r[i,3] <- "GM"
  }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Time","Reliability","Model")
  #print(r)
  r
  
}


GM_lnL <-  function(x,params){

  sum1=0
  sum2=0
  #print(params)
  n <- length(x)
  for(i in 1:n){
    sum1=sum1+ ((i-1)*log(params$GM_Phi)) 
    sum2=sum2+ (params$GM_Phi^(i-1) * x[i])
  }
  lnL <- n*log(params$GM_D0) + sum1 - params$GM_D0*sum2
  #print(lnL)
  return(lnL)
}


GM_MVF_cont <- function(params,t){
  return((-1/log(params$GM_Phi))*log(1+(params$GM_D0*(-log(params$GM_Phi)/params$GM_Phi)*t)))
  # return( (-1/log(params$GM_Phi))*log(1+params$GM_D0*(-log(params$GM_Phi)*exp(-log(params$GM_Phi)))*t))
  #return(log(params$GM_D0*(1-exp(-params$JM_Phi*t)))
}

GM_R_delta <- function(params,cur_time,delta){
  return(exp(-(GM_MVF_cont(params,(cur_time+delta)) -GM_MVF_cont(params,cur_time))))
}

GM_R_MLE_root <- function(params,cur_time,delta, reliability){
  root_equation <- reliability - GM_R_delta(params,cur_time,delta)
  return(root_equation)
}

maxiter <- 1000
GM_Target_T <- function(params,cur_time,delta, reliability){

  f <- function(t){
    return(GM_R_MLE_root(params,t,delta, reliability))
  }
  
  current_rel <- GM_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
      # Bound the estimation interval
      
      sol <- 0
      interval_left <- cur_time
      interval_right <- 2*interval_left
      local_rel <- GM_R_delta(params,interval_right,delta)
      while (local_rel <= reliability) {
        interval_right <- 2*interval_right
        if(local_rel == reliability) {
          interval_right <- 2.25*interval_right
        }
        if (is.infinite(interval_right)) {
          break
        }
        local_rel <- GM_R_delta(params,interval_right,delta)
      }
      if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
        while (GM_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
          interval_left <- interval_left + (interval_right-interval_left)/2
        }
      } else {
        sol <- Inf
      }

      if(is.finite(interval_right) && is.finite(sol)) {
        sol <- tryCatch(
          stats::uniroot(f, c(interval_left,interval_right),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
          warning = function(w){
            ##print(f.lower)
            if(length(grep("_NOT_ converged",w[1]))>0){
              maxiter <<- floor(maxiter*1.5)
              #print(paste("recursive", maxiter,sep='_'))
              GM_Target_T(a,b,cur_time,delta, reliability)
            }
          },
          error = function(e){
            #print(e)
            #return(e)
          })
      } else {
        # Infinite amount of time required to achieve reliability
        sol <- Inf
      }
  } else {
    sol <- "Target reliability already achieved"
  }
    return(sol)
}

GM_R_growth <- function(params,d,delta){  
  
  r <-data.frame()
    for(i in 1:length(d$FT)){   
      r[i,1] <- d$FT[i]
      temp <- GM_R_delta(params,d$FT[i],delta)
      ##print(typeof(temp))
      if(typeof(temp) != typeof("character")){
        r[i,2] <- temp
        r[i,3] <- "GM"
      }
      else{
        r[i,2] <- "NA"
        r[i,3] <- "GM"
      }     
    }
    g <- data.frame(r[1],r[2],r[3])
    names(g) <- c("Time","Reliability_Growth","Model")
    ##print(g)
    g
      
}


#GM_R_growth <- function(params,cur_time,delta, reliability){  
#  
#  r <-data.frame()
#  tt_index <- seq(0,cur_time,cur_time/1000)
#  for(i in 1:length(tt_index)){   
#    r[i,1] <- tt_index[i]
#    temp <- GM_R_delta(params,tt_index[i],delta)
#    ##print(typeof(temp))
#    if(typeof(temp) != typeof("character")){
#      r[i,2] <- temp
#      r[i,3] <- "GM"
#    }
#    else{
#      r[i,2] <- "NA"
#      r[i,3] <- "GM"
#    }     
#  }
#  g <- data.frame(r[1],r[2],r[3])
#  names(g) <- c("Time","Reliability_Growth","Model")
#  ##print(g)
#  g
#  
#}
#Geometric SRGM   

GM_OR_CC <- function(param,c1,c2,c3){
  return((c1*param$GM_D0-c2*param$GM_D0+c3*param$GM_Phi)/(c3*param$GM_D0*log(param$GM_Phi)))
}

#Cost equation for GM optimal release plots
GM_cost <- function(params,c1,c2,c3,t,t_lifecycle){
  return(c1*GM_MVF_cont(params,t) + c2*(GM_MVF_cont(params,t_lifecycle) - GM_MVF_cont(params,t)) + c3*t)
}

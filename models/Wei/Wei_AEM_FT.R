#Vector of failure times data
library(rootSolve)
#rm(list=ls())

#tVec <- c(39,49,53,89,93,98,102,193,242,243,268,269,273,303,345,354,403,447,479,482,560,561,591,796,801,930,1033,1257,1443,1496,1510,1519,1521,1531,1532,1566,1736,1865,1869,1873,1908,1913,1918,1940,1976,2011,2132,2155,2188,2236,2268,2289,2293,2316,2325,2338,2503,2517,2539,2580,2592,2730,2825,2874,2936,2938,2973,3062,3152,3221,3243,3258,3277,3319,3333,3344,3385,3595,3611,3641,3678,3744,3753,3769,3783,3807,3819,3978,4067,4185,4214,4235,4253,4255,4369,4406,4452,4469,4470,4620,5002,5162,5228,5434,5443,5469,5531,5770,5783,5787,5872,5957,6197,6375,6409,6511,6520,6666,6725,6773,6798,6823,6934,6939,6970,7021,7027,7220,7247,7272,7368,7394,7424,7454,7471,7791,7869,7908,7921,7934,7953,8081,8115,8199,8239,8416,8765,9039,9121,9179,9210,9324,9363,9451,9535,9767,9875,9913,9999,10006,10028,10108,10347,10350,10389,10452,10604,10667,10747,10992,11188,11234,11386,11488,11497,11725,11945,12153,12231,12234,12317,12323,12535,12626,12629,12639,12811,12832,13005,13376,13416,13464,13590,13680,13829,13859,14176,14676,15349,15781,15847,16015,16081,16147,16275,16324,16656)


Wei_AEM_FT_MLE <- function(tVec){


      #Define n, tn and sumT
      n <- length(tVec)
      tn <- tVec[n]
      sumT <- sum(tVec)

      #Define non adaptive EM function for use in adaptive procedure

      aEM <- numeric(0)
      bEM <- numeric(0)
      llEM <- numeric(0)

      nonAdaptive <- function(c0){
        b0 <- n/sum(tVec^c0)
        a0 <- n
        aEM[1] <- a0
        bEM[1] <- b0
        llEM[1] <- -(1-exp(-(tn^c0)*bEM[1]))*aEM[1]+sum(log(exp(-bEM[1]*(tVec^c0))*bEM[1]*aEM[1]*c0*(tVec^(c0-1))))
        LLError <- 0
        i <- 2
        while(LLError <= 1e-15){
          aEM[i] <- n+aEM[i-1]*exp(-bEM[i-1]*(tn^c0))
          bEM[i] <- (n+aEM[i-1]*exp(-bEM[i-1]*(tn^c0)))/((sum(tVec^c0))+aEM[i-1]*((tn^c0)+(1/bEM[i-1]))*exp(-bEM[i-1]*(tn^c0)))
          llEM[i] <- -(1-exp(-(tn^c0)*bEM[i]))*aEM[i]+sum(log(exp(-bEM[i]*(tVec^c0))*bEM[i]*aEM[i]*c0*(tVec^(c0-1))))
          LLError <- llEM[i]-llEM[i-1]
          i <- i+1
        }
        llEM[length(llEM)]
      }

      #Works fine till here


      #Perfrom multiple iterations of adaptive algorithm

      iterationsEM <- 1000;
      c0 <- 1
      cStep <- 0.1
      leftlnL <- nonAdaptive(c0-cStep)
      presentlnL <- nonAdaptive(c0)
      rightlnL <- nonAdaptive(c0+cStep)

      c0iteration <- numeric(0)
      lnpresentiteration <- numeric(0)

      for(adaptiveIteration in 1:20){
        lnpresentiteration[adaptiveIteration] <- presentlnL
        c0iteration[adaptiveIteration] <- c0
        #Go left
        if(leftlnL>presentlnL){#print(1)
          c0 <- c0-cStep
          rightlnL <- presentlnL
          presentlnL <- leftlnL
          leftlnL <- nonAdaptive(c0-cStep)
        }
        #Go right
        if(rightlnL>presentlnL){#print(2)
          c0 <- c0+cStep
          leftlnL <- presentlnL
          presentlnL <- rightlnL
          rightlnL <- nonAdaptive(c0+cStep)
        }
        #Decrease step size
        if(leftlnL<presentlnL && rightlnL<presentlnL){#print(3)
          cStep <- cStep/2
          leftlnL <- nonAdaptive(c0-cStep)
          rightlnL <- nonAdaptive(c0+cStep)
        }
      }

      #print(c0)


        b0 <- n/sum(tVec^c0)
        a0 <- n
        aEM[1] <- a0
        bEM[1] <- b0
        #Log likelihood function
        llEM[1] <- -(1-exp(-(tn^c0)*bEM[1]))*aEM[1]+sum(log(exp(-bEM[1]*(tVec^c0))*bEM[1]*aEM[1]*c0*(tVec^(c0-1))))
        LLError <- 0
        i <- 2
        while(LLError <= 1e-15){
          aEM[i] <- n+aEM[i-1]*exp(-bEM[i-1]*(tn^c0))
          bEM[i] <- (n+aEM[i-1]*exp(-bEM[i-1]*(tn^c0)))/((sum(tVec^c0))+aEM[i-1]*((tn^c0)+(1/bEM[i-1]))*exp(-bEM[i-1]*(tn^c0)))
          llEM[i] <- -(1-exp(-(tn^c0)*bEM[i]))*aEM[i]+sum(log(exp(-bEM[i]*(tVec^c0))*bEM[i]*aEM[i]*c0*(tVec^(c0-1))))
          LLError <- llEM[i]-llEM[i-1]
          i <- i+1
        }
      #print(bEM[length(bEM)])
      #print(aEM[length(aEM)])
      #print(llEM[length(llEM)])

      b0 <- bEM[length(bEM)]

      model1 <- function(x) {
        sumi <- c(0,0)
        for(i in 1:n)
        {
          sumi[1] <- sumi[1] + (1/x[1]) - ((tVec[i])^x[2])   
          sumi[2] <- sumi[2] + (1/x[2]) - (((tVec[i])^x[2])*log(tVec[i])*x[1]) + (log(tVec[i]))      #calculating the values for the summation 
        }
        #print(x)
        
        c(F1 = ((-1*n*(tn^x[2]))/(exp(x[1]*(tn^x[2]))-1) + sumi[1]),
          F2 = ((-1*x[1]*n*(tn^x[2])*log(tn))/(exp(x[1]*(tn^x[2]))-1) + sumi[2]))
      }
      ab <- multiroot(f=model1,start=c(b0,c0),maxiter = 4000, ctol = 1e-24)$root
      bMLE <- ab[1]
      cMLE <- ab[2]


      aMLE <- (n*exp(bMLE*tn^cMLE))/(exp(bMLE*tn^cMLE)-1)
      #print(aMLE)
      #print(ab)

      if(is.nan(ab[1])){
  bMLE <- bEM[length(bEM)]
  cMLE <- c0
  aMLE <- aEM[length(aEM)]
#  print(-(1-exp(-(tn^cMLE)*bMLE))*aMLE+sum(log(exp(-bMLE*(tVec^cMLE))*bMLE*aMLE*cMLE*(tVec^(cMLE-1)))))
}

      params <- data.frame(aMLE,bMLE,cMLE)
      names(params) <- c(paste("Wei",Wei_params[1],sep="_"),paste("Wei",Wei_params[2],sep="_"),paste("Wei",Wei_params[3],sep="_"))
      print("Wei AEM Params")
      print(params)
      params
}


#print(-(1-exp(-(tn^cMLE)*bMLE))*aMLE+sum(log(exp(-bMLE*(tVec^cMLE))*bMLE*aMLE*cMLE*(tVec^(cMLE-1)))))

#if(is.nan(ab[1])){
#  bMLE <- bEM[length(bEM)]
#  cMLE <- c0
#  aMLE <- aEM[length(aEM)]
#  print(-(1-exp(-(tn^cMLE)*bMLE))*aMLE+sum(log(exp(-bMLE*(tVec^cMLE))*bMLE*aMLE*cMLE*(tVec^(cMLE-1)))))
#}
#par(mfrow=c(2,2))
#plot(aEM, type="l",col="red",main="parameter a")
#plot(bEM, type="l",col="green",main="parameter b")
#plot(llEM, type="l",main="Improvements in log-likelihood")
#plot(aEM,bEM, type="b",main="parameter search")

#Newton's method routine -- doesn't work !! :-( x[1] -> bmle, x[2]->cmle
#Model <- function(x){
# c(
# f1=((-(n*(1-tn^(x[2]*x[1])+(tn^x[2])*x[1]+exp((tn^x[1])*x[2])*(-1+(tVec^x[1])*x[1])))/((-1+exp((tn^x[2])*x[1])*x[1]))), f2=((n*(1-exp((tn^x[2])*x[1]))+((1-exp((tn^x[2])*x[1])*x[2]*(-1+(tVec^x[2])*x[1])*log(tVec)))+(tn^x[2])*x[2]*x[1]*log(tn))/((1-exp((tn^x[2])*x[1])*x[2])))	)
#}
#multiroot(Model, c(bMLE,c0))


Wei_lnL <- function(x,params){
  n <- length(x)
  tn <- x[n]
  sum1 <- 0
  for(i in 1:n){
    sum1=sum1+ (log(params$Wei_bMLE*params$Wei_cMLE*exp(-params$Wei_bMLE*(x[i]^params$Wei_cMLE))*params$Wei_aMLE*(x[i]^(params$Wei_cMLE-1))))
  }
  return(((-1+exp(-params$Wei_bMLE*(tn^params$Wei_cMLE)))*params$Wei_aMLE) + sum1)
}


Wei_MVF <- function(param,d){
  n <- length(d$FT)
  r <- data.frame()
  MVF <- (param$Wei_aMLE)*(1-exp(-1*(d$FT^param$Wei_cMLE)*param$Wei_bMLE))
  r <- data.frame(d$FT,MVF,rep("Wei", n))
  names(r) <- c("Time","Failure","Model")
  r
  #a(1-e^(-bt^c))
}


Wei_MVF_inv <- function(param,d){
  n <- length(d$FN)
  r <- data.frame()
  cumFailTimes <- (-log((param$Wei_aMLE-d$FN)/param$Wei_aMLE)/param$Wei_bMLE)^(1/param$Wei_cMLE)
  numPredPoints = floor(param$Wei_aMLE) - (d$FN[1]-1) #Number of points to be predicted : floor(aMLE) - (number of samples)
  print(param$Wei_aMLE)
  print(d$FN[1])
  
  if(numPredPoints < n){
    cumFailTimes[is.na(cumFailTimes)] <- 0 #If there are NaNs in the frame, replace it with zeros
    cumFailTimes[numPredPoints:length(cumFailTimes)] <- max(cumFailTimes[1:numPredPoints]) #Replace the end of the frame with the max of cumulative times
  }
  
  r <- data.frame(d$FN,cumFailTimes,rep("Wei", n))
  names(r) <- c("Failure","Time","Model")
  r
}


Wei_FI <- function(params,d){
  #------------------------------------------------------------------------
  # This function computes the Failure Intensity for a given data
  # with parameters 'param' of a given data
  #------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF

  # @returns    (data.frame)    data.frame of Failure Count, Failure_Rate, Model columns
  #------------------------------------------------------------------------
  # TODO :
  #========================================================================
  n <- length(d$FT)
  r <-data.frame()
  fail_number <- c(1:n)
  failIntensity <- params$Wei_aMLE*params$Wei_bMLE*params$Wei_cMLE*(exp(-params$Wei_bMLE*(d$FT)^params$Wei_cMLE))*d$FT^(params$Wei_cMLE-1)
  r <- data.frame(fail_number,failIntensity, rep("JM",n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r
}

Wei_R <- function(){
  # TODO
}

Wei_MTTF <- function(params,d){
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- i
    r[i,2] <- 1/(params$Wei_aMLE*params$Wei_bMLE*params$Wei_cMLE*(exp(-params$Wei_bMLE*(d$FT[i])^params$Wei_cMLE))*d$FT[i]^(params$Wei_cMLE-1))
    r[i,3] <- "Wei"
    }
  r <- data.frame(r[1],r[2],r[3])
  names(r) <- c("Failure_Number","MTTF","Model")
  r
}


Wei_MVF_cont <- function(param,t){
  return((param$Wei_aMLE)*(1-exp(-1*(t^param$Wei_cMLE)*param$Wei_bMLE)))
}

Wei_R_delta <- function(params,cur_time,delta){
  return(exp(-(Wei_MVF_cont(params,(cur_time+delta)) - Wei_MVF_cont(params,cur_time))))
}

Wei_R_MLE_root <- function(params,cur_time,delta, reliability){
  root_equation <- reliability - Wei_R_delta(params,cur_time,delta)
  return(root_equation)
}

maxiter <- 1000
Wei_Target_T <- function(params,cur_time,delta, reliability){

  f <- function(t){
    return(Wei_R_MLE_root(params,t,delta, reliability))
  }

  current_rel <- Wei_R_delta(params,cur_time,delta)
  if(current_rel < reliability){
    # Bound the estimation interval
    
    sol <- 0
    interval_left <- cur_time
    interval_right <- 2*interval_left
    local_rel <- Wei_R_delta(params,interval_right,delta)
    while (local_rel <= reliability) {
      interval_right <- 2*interval_right
      if(local_rel == reliability) {
        interval_right <- 2.25*interval_right
      }
      if (is.infinite(interval_right)) {
        break
      }
      local_rel <- Wei_R_delta(params,interval_right,delta)
    }
    if(is.finite(interval_right) && is.finite(local_rel) && (local_rel < 1)) {
      while (Wei_R_delta(params,(interval_left + (interval_right-interval_left)/2),delta) < reliability) {
        interval_left <- interval_left + (interval_right-interval_left)/2
      }
    } else {
      sol <- Inf
    }
    
    if (is.finite(interval_right) && is.finite(sol)) {
      sol <- tryCatch(
        stats::uniroot(f, c(cur_time,cur_time + 50),extendInt="yes", maxiter=maxiter, tol=1e-10)$root,
        warning = function(w){
          ##print(f.lower)
          if(length(grep("_NOT_ converged",w[1]))>0){
            maxiter <<- floor(maxiter*1.5)
            ##print(paste("recursive", maxiter,sep='_'))
            Wei_Target_T(a,b,cur_time,delta, reliability)
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

Wei_R_growth <- function(params,d,delta){  
  
  r <-data.frame()
    for(i in 1:length(d$FT)){   
      r[i,1] <- d$FT[i]
      temp <- Wei_R_delta(params,d$FT[i],delta)
      ##print(typeof(temp))
      if(typeof(temp) != typeof("character")){
        r[i,2] <- temp
      }
      else{
        r[i,2] <- "NA"
      }     
    }
    g <- data.frame(r[1],r[2],rep("Wei", length(d$FT)))
    names(g) <- c("Time","Reliability_Growth","Model")
    g
      
}

#Weibull SRGM
#Method -1: tStar equation 
Wei_OR_CC <- function(params,c1,c2,c3){
library(emdbook)
  return(((-(params$Wei_cMLE-1)*lambertW(((c3/(params$Wei_aMLE*params$Wei_bMLE*params$Wei_cMLE*(c2-c1)))^(params$Wei_cMLE/(params$Wei_cMLE-1))*params$Wei_bMLE*params$Wei_cMLE)/(1-params$Wei_cMLE)))/(params$Wei_bMLE*params$Wei_cMLE))^(1/params$Wei_cMLE))
}

#Method-2: Using uniroot function
#eqn <- function(d){
#  aMLE<- 172.526
#    bMLE<- 0.000696057
#    cMLE<- 0.676739
#    c1<- 50
#    c2<-1000000
#    c3<-200
#  rt <- numeric(0)
#  rt <-(bMLE*cMLE*exp(-bMLE*(d^cMLE))*d^(cMLE-1)*aMLE)-(c3/(c2-c1))
#  return(rt)}

#end point of the data
#tn<-88682
#uniroot(eqn, c(0.0000001,tn), tol=1e-10, extendInt="yes")$root

#Cost equation for Weibull optimal release plots
Wei_cost <- function(params,c1,c2,c3,t,t_lifecycle){
  return(c1*Wei_MVF_cont(params,t) + c2*(Wei_MVF_cont(params,t_lifecycle) - Wei_MVF_cont(params,t)) + c3*t)
}




library(MASS)
library(rootSolve)
# require("Rmpfr") # This was our option if precision is a problem. It stays here as long we are not sure.
require("utils") # depends on utils library

LNORM_ECM_MLE<-function(FT){
  # ---------------------------------------------------------------------------------
  # Maximumum likelihood estimation method is used to estimate the parameters 
  # 'N0' and 'Phi'. The initial estimate leftinterval and right interval are expanded
  # untill the zero of the equation is bracketed. The bisection method is used for 
  # bracketing the zero of equation hence the name BM in the name of function with 
  # '_' seperator. The zero of the expression is the maximum likelihood estimation of
  # N0 and the corresponding Phi is derived.
  #----------------------------------------------------------------------------------
  #
  # @params   : interFail  (list)       Input interfailure vector
  #
  # @returns  : JM_params  (data.frame) Dataframe of 'N0' and 'Phi'  
  #             JM_params$N0 refers to N0 and JM_params$Phi refers to Phi 
  #==================================================================================
  
  
  tVec<- as.numeric(FT)  # to avoid precision problems
  n <- length(FT)              # length of vector interfail
  #----------------------------------------------------------------------------------  
  
  
  
#tVec1 <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)
#Define n, tn and sumT
tVec <- as.numeric(tVec)
n <- length(tVec)
tn <- tVec[n]
sumT <- sum(tVec)
crule<- 4
brule<- sum(log(tVec)-crule)/sqrt(n)
#brule<- 300

Da<- function(a){
  b <- brule
  c<- crule
  da=(n/a)-(1/2)*pracma::erfc((b-log(tVec[n]))/sqrt(2)*c)
  return(da)
}
ao<- stats::uniroot(Da, c(0,200), maxiter=1e15, tol=1e-10, extendInt="yes")$root


Db <- function(brule){
  b<- brule
  firstterm<- 0  
  c <- crule
  
  for(i in 1:n)
  {
    firstterm= firstterm -((b-log(tVec[i]))/c^2)+ (n*exp(-(b-log(tVec[n]))^2/c^2)*sqrt(2/pi))/c*pracma::erfc((b-log(tVec[n]))/sqrt(2)*c)
  }
  return(firstterm)
  
}

Dc<- function(crule){
  secondterm <- 0
  b<- brule
  c<-crule
  for(i in 1:n)
  {
   secondterm= secondterm  + ((b-log(tVec[i]))^2)/(c^3) 
  }
  dc=-n/c+ secondterm-(n*exp(-(b-log(tVec[n]))^2/(2*c^2))*(sqrt(2/pi)*(b-log(tVec[n]))))/((c^2)*(pracma::erfc((b-log(tVec[n]))/(sqrt(2)*c))))
  return(dc)
 
}

lnLa <- function(ao,brule,crule){
  ao<- stats::uniroot(Da, c(0,200), maxiter=1e4, tol=1e-10, extendInt="yes")$root
  a<-  ao
  b<- brule
  c<- crule
  firstterm <- 0
  secondterm <- 0
  
  for(i in 1:n){
    
    firstterm = firstterm + ((b-log(tVec[i]))^2)/(2*(c^2))
  }
  for(i in 1:n)
  {
  secondterm = secondterm + log(c*sqrt(2*pi)*tVec[i])
  }
  
  lnLa= -(1/2)*a*pracma::erfc((b-log(tVec[n]))/sqrt(2)*c)+ n*log(a)-firstterm-secondterm
  return(lnLa)
}

lnL<- function(brule,crule){
  firstterm <- 0
  secondterm<- 0
  b<- brule
  c<- crule
  for(i in 1:n){
    firstterm=firstterm + ((b-log(tVec[i]))^2)/(2*c^2)
  }
  for(i in 1:n){
    secondterm=secondterm+log(tVec[i]*c*sqrt(2*pi))
  }
  lnL= -n-firstterm-secondterm+n*log((272)/((pracma::erfc((b-log(tVec[n]))/(sqrt(2)*c)))))
  
  return(lnL)
}


previous_lnl<- 0
current_lnl<- 1
LLerror<- 1

while(LLerror > 1e-15){
  previous_lnl<- lnL(brule,crule)
  arule<- stats::uniroot(Da, c(0,140), maxiter=1e5, tol=1e-10, extendInt="yes")$root
  brule<- stats::uniroot(Db, c(0,140), maxiter=1e5, tol=1e-10, extendInt="yes")$root
  crule <- stats::uniroot(Dc, c(1,40), maxiter=1e5, tol=1e-10, extendInt="yes")$root
 current_lnl<- lnL(brule,crule)
  LLerror<- abs(current_lnl-previous_lnl)
  
}

aHat<- 272/((pracma::erfc((brule-log(tVec[n]))/(sqrt(2)*crule))))


faulremain<- aHat-n


LNORM_params <-  data.frame("a"=aHat,"brule"=brule,"crule"=crule)
return(LNORM_params)
}


LNORM_MVF <- function(param,d) {
  #----------------------------------------------------------------------
  # This function computes the MVF data frame
  # MVF - Mean Value Function
  #----------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Failure, Time, Model columns
  #-----------------------------------------------------------------------
  #TODO:
  #======================================================================
  n <- length(d$FT)
  r <- data.frame()
  fail_number <- c(1:n)

   
  cumFailures <- (1/2)*param$a*((pracma::erfc((param$mu-log(d$FT))/(sqrt(2)*param$sigma))))
    
  

  r <- data.frame(cumFailures, d$FT, rep("LNORM", n))
  names(r) <- c("Failure","Time", "Model")
  r

}
LNORM_MTTF <- function(param,d){
  #------------------------------------------------------------------------
  # This function MTTF of given d with parameters
  # MTTF is Mean Time To Failure
  #------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Failure, MTTF, Model columns
  #------------------------------------------------------------------------
  # TODO :
  #========================================================================
  n <- length(d$FT)
  r <-data.frame()
  fail_number <- c(0:(n-1))
  IFTimes <- 1/(exp(-(param$mu-Log(d$FT))^2/2*param$sigma^2))/sqrt(2*pi)*param$sigma*d$FT
  r <- data.frame(c(1:n),IFTimes, rep("LNORM", n))
  names(r) <- c("Failure_Number","MTTF","Model")
  r  
}

LNORM_FI <- function(param,d){
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
  failIntensity <- (exp(-(param$sigma-Log(d$FT))^2/2*param$sigma^2))/sqrt(2*pi)*param$sigma*d$FT
  r <- data.frame(fail_number,failIntensity, rep("JM",n))
  names(r) <- c("Failure_Count","Failure_Rate","Model")
  r  
}


LNORM_R <- function(param,d){
  #---------------------------------------------------------------------------
  # This function computes Reliability from given parameters and data
  #---------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (data.frame)    data.frame of Time, Reliability, Model columns
  #---------------------------------------------------------------------------
  # TODO:
  #===========================================================================
  n <- length(d$FT)
  r <-data.frame()
  cumulr <-data.frame()
  for(i in 1:n){
    r[i,1] <- d$FT[i]
    r[i,2] <- 1- (1/2)*((pracma::erfc((param$mu-log(d$FT[i]))/(sqrt(2)*param$sigma))))
  }
  r <- data.frame(r[1],r[2], rep("LNORM", n))
  names(r) <- c("Time","Reliability","Model")
  r
}
LNORM_lnL <- function(x,params){
  #----------------------------------------------------------------------------
  # This computes Log-Likelihood for a given data x and parameters
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @d          (data.frame)    Data.frame of data FT,FC,FN,CFC,IF
  
  # @returns    (numeric)       Numeric value of lnL
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  n <- length(x)          
  firstterm <- 0
  secondterm<- 0
  b<- mu
  c<- sigma
  for(i in 1:n){
    firstterm=firstterm + ((b-log(tVec[i]))^2)/(2*c^2)
  }
  for(i in 1:n){
    secondterm=secondterm+log(d$FT[i]*c*sqrt(2*pi))
  }
  lnL= -n-firstterm-secondterm+n*log((272)/((pracma::erfc((b-log(D$FT[n]))/(sqrt(2)*c)))))
  
  return(lnL)
}
#Faults Remaining

LNORM_FaultsRemaining <- function(params,n){
  #----------------------------------------------------------------------------
  # This function evaluates the Faults remaining in the system
  #----------------------------------------------------------------------------
  # @params     (data.frame)    Data.frame of parameters
  # @n          (numeric)       Length of vector
  
  # @returns    (numeric)       Faults remaining
  #----------------------------------------------------------------------------
  # TODO:
  #============================================================================
  return(floor(params$a-n))
}



LNORM_R_growth <- function(params,d,delta){
  #---------------------------------------------------------------------------------------
  #  This function computes the reliability growth
  #---------------------------------------------------------------------------------------
  
  # @params      (data.frame)    Data.frame of parameters
  # @d           (data.frame)    Data.frame of data FT,FC,CFC,IF
  # @delta       (numeric)       delta time -> (t` - t), t` is delta away from current time   
  
  # @returns     (data.frame)    Data frame of Time,Reliablity Growth, Model
  #---------------------------------------------------------------------------------------
  #TODO:
  #=======================================================================================   
  
  r <-data.frame()
  for(i in 1:length(d$FT)){   
    r[i,1] <- d$FT[i]
    temp <- JM_R_delta(params,d$FT[i],delta)
    #print(typeof(temp))
    if(typeof(temp) != typeof("character")){
      r[i,2] <- temp
      r[i,3] <- "LNORM"
    }
    else{
      r[i,2] <- "NA"
      r[i,3] <- "LNORM"
    }     
  }
  g <- data.frame(r[1],r[2],r[3])
  names(g) <- c("Time","Reliability_Growth","Model")
  g  
}





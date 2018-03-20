#Vector of failure times data
library(rootSolve)
#rm(list=ls())

#kVec <- c(10,64,18,43,44,13,34,28,15,10)
#tVec <- c(1,1.5,2,3,4.5,6,8,11,12,13)

Wei_NM_FC_MLE <- function(tVec, kVec){
            #tVec <- c(0,tVec)
            n <- length(kVec)

            tn <- tVec[length(tVec)]
            sumT <- sum(tVec)
            sumK <- sum(kVec)

            #estimate starting point for 'b'
            b0 <- n/sumT

            MLEeq<-function(b){
              sumil = 0
              sumil1=0
              for(i in 2:n)
              {
                sumil= sumil + ((((exp(-b*tVec[i])*tVec[i])-(exp(-b*tVec[i-1])*tVec[i-1]))*kVec[i])/(exp(-b*tVec[i-1])-exp(-b*tVec[i])))    
              }
              for(j in 1:n)
              {
                sumil1 = sumil1 + kVec[j]
              }

              b_MLE <- ((kVec[1]*tVec[1])/(-1+exp(b*tVec[1]))) - (exp(-b*tn)*tn*sumil1) + sumil
              
              return(b_MLE)
            }

            i <- 0 
            maxIterations <- 200
            leftEndPoint <- b0/2
            leftEndPointMLE <- MLEeq(leftEndPoint)
            rightEndPoint <- 2.0*b0
            rightEndPointMLE <- MLEeq(rightEndPoint)

            while(leftEndPointMLE*rightEndPointMLE > 0 & i <= maxIterations){
              print('In Step 2 while loop of Wei_BM.R')
              leftEndPoint <- leftEndPoint/2
              leftEndPointMLE <- MLEeq(leftEndPoint)
              rightEndPoint <- 2.0*rightEndPoint
              rightEndPointMLE <- MLEeq(rightEndPoint)
              i <- i+1	
            }

            if(leftEndPointMLE*rightEndPointMLE > 0 ){
              return('nonconvergence')
            } else {
              b_initial <- uniroot(MLEeq,lower=leftEndPoint,upper=rightEndPoint, extendInt="yes", tol = 1e-24)$root
            }
            #print(b_initial)

            b0 <- b_initial/2 #signif(b_initial,1)

            a0 <- sumK
            c0 <-  1.0


            #(*MLE equation of x[1]*)
            model <- function(x) {
              sumi <- c(0,0,0)
              for(i in 1:n)
              {
                sumi[1] <- sumi[1] + (kVec[i]/x[1])   
              }
              for(i in 2:n)
              {
                sumi[2] <- sumi[2] + ((((exp(-x[2]*tVec[i]^x[3])*tVec[i]^x[3])-(exp(-x[2]*tVec[i-1]^x[3])*tVec[i-1]^x[3]))*kVec[i])/(exp(-x[2]*tVec[i-1]^x[3])-exp(-x[2]*tVec[i]^x[3])))
                sumi[3] <- sumi[3] + ((x[2]*(exp(-x[2]*tVec[i]^x[3])*tVec[i]^x[3] *log(tVec[i]) -  exp(-x[2]*tVec[i-1]^x[3])*tVec[i-1]^x[3] *log(tVec[i-1]))*kVec[i])/(exp(-x[2]*tVec[i-1]^x[3])-exp(-x[2]*tVec[i]^x[3])))
              }  #print(x)
              
              c(F1 =(-1+exp(-x[2]*tn^x[3]))+sumi[1],
                F2 = (-x[1]*exp(-x[2]*tn^x[3])*tn^x[3]) + ((kVec[1]*tVec[1]^x[3])/(exp(x[2]*tVec[1]^x[3])-1)) + sumi[2],
                F3= (-x[1]*x[2]*exp(-x[2]*tn^x[3])*tn^x[3]*log(tn)) + (x[2]*log(tVec[1])*kVec[1]*tVec[1]^x[3])/(-1+exp(x[2]*tVec[1]^x[3]))+sumi[3])
            }

            abc <- multiroot(f=model,start=c(a0,b0,c0),maxiter = 100000,ctol = 1e-24)$root
            print(abc)

            if(is.nan(abc[1])){
              b0 <- sumK/(sumT*tn)
              abc <- multiroot(f=model,start=c(a0,b0,c0),maxiter = 100000,ctol = 1e-24)$root
            }

            #print(b0)
            #print(abcd)
	aMLE <- abc[1]
  bMLE <- abc[2]
  cMLE <- abc[3] 
	print(aMLE, bMLE, cMLE)
	#sol <- data.frame(aMLE, bMLE, cMLE)
  sol <- data.frame(abc[1], abc[2], abc[3])
  names(sol) <- c("Wei_aMLE", "Wei_bMLE", "Wei_cMLE")
    # sol <- c(aMLE,bMLE)
    sol
}

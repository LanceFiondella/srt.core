#log-likelihood function 

JM_BM_LLF <- function(n,x,N0_MLE,phi){						
secondTerm=0
thirdTerm = 0

for(i in 1:n){
    secondTerm = secondTerm +log((N0_MLE-(i-1)))
    thirdTerm = thirdTerm +((N0_MLE-(i-1))*x[i])#x=interFail
  }
  llf <- n*log(phi)+ secondTerm-(phi*thirdTerm)
  return(llf)
 }
 
 #Faults Remaining
 
 JM_MB_FaultsRemaining <- function(N0_MLE,n){
 	return(floor(N0_MLE-n))
 }
 
 #Reliability
 
 JM_BM_Reliability <- function(n,x,N0_MLE,phi){
 	Reliability <- numeric(0)
 	Reliability <- exp(-phi*(N0_MLE-(i-1))*x[i])
 	return(Reliability)
 }
 
 #MTTF
 
 JM_MTTF <- function(n,N0_MLE,phi){
 	MTTF=0
 	for(i in 1:n){
 		MTTF = MTTF +(1/(phi*(N0_MLE-(n+(i-1)))))
 	}
 	return(MTTF)
 }

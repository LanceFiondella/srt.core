#The purpose of these fuctions are to convert between failure times and interfailure times
failureT_to_interF <- function(interfailure)
{

failure_T <- c()
failure_T[1] = interfailure[1] #initial value for failure time

n = 2

while(n<=length(interfailure))
{
   failure_T[n] = interfailure[n] - interfailure[n-1]
   
   n = n+1
}

return(failure_T)#return failure times(failure_T)
}




#################################



 interF_to_failureT <- function(failure_T)
{


interfailure <- c()

interfailure[1] = failure_T[1] #initial value for failure time

n = 2

while(n<=length(failure_T))
{
  interfailure[n] = failure_T[n] + interfailure[n-1]
  
  n = n + 1
}


return(interfailure)#return interfailure times(interfailure)
}



CumulativeFailureC_to_failureC <- function(x){
  fc <- c()
  x <- c(0,x)
  for(i in 1:(length(x)-1)){
     fc[i] <- x[i+1] - x[i]   
  }
  return(fc)
}

#################################




#failureC_to_failureT <- function(failure_Count)
#{

#failure_T <- c()

#n = 1
#i = 1

#while(n<=length(failure_Count))
#{
#  if(failure_Count[n] != 0)
#  {
#    failure_T[i] = n
#    i = i + 1
 # }
  
 # n = n + 1;
#}

#return(failure_T)#return failure times(failure_t)
#}

failureC_to_failureT <- function(time_vec,num_count) #failure count to failure time
{
  failure_T <- c()
  time_vec<- c(0,time_vec)
  m <- 1
  for(j in 1:(length(time_vec)-1))
  {
    for(i in 1:num_count[j])
    {
      if(num_count[j]!=0)
      {
        failure_T[m] <- time_vec[j]+ ((i-0.5)*((time_vec[j+1]-time_vec[j])/num_count[j]))
        m <- m+1
      }
    }
  }
  return(failure_T)
}


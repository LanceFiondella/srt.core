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





#################################




failureC_to_failureT <- function(failure_Count)
{

failure_T <- c()

n = 1
i = 1

while(n<=length(failure_Count))
{
  if(failure_Count[n] != 0)
  {
    failure_T[i] = n
    i = i + 1
  }
  
  n = n + 1;
}

return(failure_T)#return failure times(failure_t)
}



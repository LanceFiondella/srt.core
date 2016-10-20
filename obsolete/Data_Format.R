#The purpose of these fuctions are to convert between failure times and interfailure times
failureT_to_interF <- function(failure_T)
{

interfailure <- c()
interfailure[1] = failure_T[1] #initial value for failure time

n = 2

while(n<=length(failure_T))
{
  interfailure[n] = failure_T[n] - failure_T[n-1]
   
   n = n+1
}

return(interfailure)#return failure times(failure_T)
}





#################################



 interF_to_failureT <- function(interfailure) #interfailure to failure times
{


   failure_T <- c()

   failure_T[1] = interfailure[1] #initial value for failure time

n = 2

while(n<=length(interfailure))
{
  failure_T[n] = interfailure[n] + failure_T[n-1]
  
  n = n + 1
}


return(failure_T)#return interfailure times(interfailure)
}





#################################




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

CumulativeFailureC_to_failureC <- function(x){
  fc <- c()
  x <- c(0,x)
  for(i in 1:(length(x)-1)){
     fc[i] <- x[i+1] - x[i]   
  }
  return(fc)
}






createFields <- function(data){
  len <- nrow(data[1]) #length of data
  FC<-c(1:len)  #vector of failure counts from 1 to length
  names(FC)<-"FC" #naming the vector
  
  if (names(data[1]) =="FT"){ #if the first column is failure times, convert to interfail
    FT <- data[,1]
    names(FT)<-"FT"
    IF <- failureT_to_interF(data[,1]) #converts from failure times to interfailure times
    names(IF)<-"IF"
  }else if(names(data[1]) == "IF"){ #if the first column is interfailure times, convert to failure time
    IF <- data[,1]
    names(IF)<-"IF"
    FT <-interF_to_failureT(data[,1]) 
    names(FT)<-"FT"
  }else if(names(data[1]) == "FC") { #if the first column is failure count and next rows are IF or FT
    FC <- data[,1]
    names(FC)<-"FC"
    if(names(data[2])=="FT"){#if second row is failure time find IF
      FT <- data[,2]
      names(FT)<-"FT"
      IF <- failureT_to_interF(data[,2])
      names(IF)<-"IF"
    }else if(names(data[2])=="IF"){#if second row is interfailure times find FT
      IF <- data[,2]
      names(IF)<-"IF"
      FT <-interF_to_failureT(data[,2])
      names(FT)<-"FT"}
  }
  return(list(FC=FC,FT=FT,IF=IF))
}

failureN_to_failureCount <-function(x)
{
  ##print("HI i am X")
  #print(x)  
  bins <- seq(min(x), max(x), length.out = length(x))
  histo <- hist(x, breaks = bins, col = 'darkgray', border = 'white')
  r <- data.frame(histo$counts)
  #print(r)
  r$x

}
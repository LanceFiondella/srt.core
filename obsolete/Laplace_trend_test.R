
source("Data_Format.R")
laplace_trend_test <- function(inter_failure)
{
  #laplace_score <- c()    
  #inte <- 0 
  n <- length(failure_time)
  #Time <- failute_time[n]         #the final time
  #inte=(sum(failute_time))-failute_time[n]        #sum of failure time for n-1
  #laplace_score <- ((inte/n)-(0.5*Time))/(Time*((1/(12*n))^0.5))      # calculating the laplace score according to (http://kscsma.ksc.nasa.gov/Reliability/Documents/Laplace_Test.pdf)
  failure_time <- inter_failure_to_failute_time(inter_failure)       # convering failure time to inter-failure for laplace test fn
  laplace_trend <- c()
  for(i in 2:n)
  {
    sumint <- 0
    sumint1 <- 0
    for(j in 1:(i-1))
    {
      sumint <- sumint + inter_failure[j]             # the laplace test function is implemented acccrding to (http://www.ece.uvic.ca/~itraore/seng426-07/notes/qual07-8.pdf)
        for(k in 1:j)
        {
          sumint1 <- sumint1 +inter_failure[k]
        }
    }
    laplace_trend[i] <-(((1/(i-1))*sumint1) -(sumint/2))/(failure_time[i]*(1/(12*(i-1))^(0.5)))
  }
  ##print(laplace_score)   ##printing laplace score on the console
  ##print(laplace_trend) 
  #plot(laplace_trend,type="b")    # ploting laplace function
  trend_data <- data.frame(c(1:length(laplace_trend)),laplace_trend)
  names(trend_data) <- c("Index","Laplace_factor")
  return(trend_data)
}

laplace_trend_test <- function(inter_failure)
{
  #laplace_score <- c()    
  #inte <- 0 
  
  n <- length(inter_failure)
  #Time <- failute_time[n]         #the final time
  #inte=(sum(failute_time))-failute_time[n]        #sum of failure time for n-1
  #laplace_score <- ((inte/n)-(0.5*Time))/(Time*((1/(12*n))^0.5))      # calculating the laplace score according to (http://kscsma.ksc.nasa.gov/Reliability/Documents/Laplace_Test.pdf)
  failure_time <- IF_to_FT(inter_failure)       # convering failure time to inter-failure for laplace test fn
  
  laplace_trend <- c()
  laplace_trend[1] <- 0
  for(i in 2:n)
  {
    sumint <- 0
    for(j in 1:(i-1))
    {
      sumint <- sumint + failure_time[j]             # the laplace test function is implemented acccrding to (http://www.ece.uvic.ca/~itraore/seng426-07/notes/qual07-8.pdf)
    }
    laplace_trend[i] <-(((1/(i-1))*sumint) -(failure_time[i]/2))/(failure_time[i]*(1/(12*(i-1))^(0.5)))
  }
  ##print(laplace_score)   ##printing laplace score on the console
  ##print(laplace_trend) 
  #plot(laplace_trend,type="b")    # ploting laplace function
  #laplace_trend
  trend_data <- data.frame(c(1:length(laplace_trend)),laplace_trend)
  names(trend_data) <- c("Index","Laplace_factor")
  return(trend_data)
}

laplace_trend_test <- function(inter_failure)
{
  n <- length(inter_failure)
  failure_time <- IF_to_FT(inter_failure)       # convering failure time to inter-failure for laplace test fn
  
  laplace_trend <- c()
  laplace_trend[1] <- 0
  for(i in 2:n)
  {
    sumint <- 0
    sumint1 <- 0
    for(j in 1:(i-1))
    {
      sumint <- sumint + failure_time[j]             # the laplace test function is implemented acccrding to (http://www.ece.uvic.ca/~itraore/seng426-07/notes/qual07-8.pdf)
    }
    laplace_trend[i] <-(((1/(i-1))*sumint) -(failure_time[i]/2))/(failure_time[i]*(1/(12*(i-1))^(0.5)))
  }
  #print(laplace_score)   #printing laplace score on the console
  print(laplace_trend) 
  #plot(laplace_trend,type="b")    # ploting laplace function
  #laplace_trend
  trend_data <- data.frame(failure_time,laplace_trend)
  names(trend_data) <- c("Index","Laplace_factor")
  return(trend_data)
}

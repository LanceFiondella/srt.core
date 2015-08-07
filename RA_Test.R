

running_average_test <- function(inter_failure_in)
{
  inter_failure <- c(unlist(inter_failure_in), use.names=FALSE)
  n <- length(inter_failure)
  runningAverage <- c()
  for(i in 1:n)
  {
    sum1 <-0
    for(j in 1:i)
    {
      sum1 <- sum1 + inter_failure[j]
    }
    runningAverage[i] <- (1/i)*sum1;
  }
  runningAverage <- data.frame(c(1:length(runningAverage)),runningAverage)
  names(runningAverage) <- c("Index","Running_Average")
  return(runningAverage)
  #return(armean)
}
Arithmetic_Mean_Test <- function(inter_failure)
{
  n <- length(inter_failure)   # getting the length of the vector
  ar_mean <- c()
  for(i in 1:n)
  {
    sum1 <=0
    for(j in 1:i)           # integrating the inter-failure from 1 to i
    {
      sum1 <- sum1 + inter_failure[j]
    }
    armean[i] <- (1/i)*sum1;    #finding the running avg
  }
  return(armean)
}

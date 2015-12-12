GeoModel <- function(data)
{
  
  #n=length(data)
  #interfail=c()
  #interfail[1,1]=data[1,1]
  #interfail[1,2]=data[1,2]
  #for(i in 2:n){
  #interfail[i,1] = i
  #interfail[i,2]=data[i,2]-data[i-1,2]}
  
  #return(interfail)
  
  
  newdata <- data
  newdata[,2] <- 3000 
  return(newdata)
  
}
GoelOkModel <- function(data)
{
  newdata <- data
  newdata[,2] <- newdata[,2] * 2
  return(newdata)
  
}
YamadaModel <- function(data)
{
  newdata <- data
  newdata[,2] <- newdata[,1]
  return(newdata)
  
}


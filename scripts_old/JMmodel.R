fPhi <- function(N0,interfail,n){
  Phi = n/sum((N0-(0:(n-1)))*interfail) #Calculating phi
  return(Phi)}
#I commented
fN0 <- function(N0,interfail,n){ #calculating the initial number of failures
  Nnot = sum(interfail)*fPhi(N0,interfail,n)-sum(1/(N0-(0:(n-1))))
  return(Nnot)}

JMmodel <- function(data){
  n=length(data)
  interfail=c()
  interfail[1]=data[1,2]
  for(i in 2:n){
    interfail[i]=data[i,2]-data[i-1,2]} #calculate interfailure times from failure counts (data[])
  
  
  N0=-1 #calulating upper
  up=n*2 # and lower bounds of function
  while(N0<0){  
    N0=fN0(up,interfail,n) #which is what these three lines implement
    up=up*2}
  
  N0 = uniroot(function(N0) fN0(N0,interfail,n),lower=n,upper=up,tol=1e-9)$root #calculates orginal estimated number of
  Phi = fPhi(N0,interfail,n) #failures based off of the starting upper and lower bounds previously calculated
  
  rely = c()
  for(tnow in as.integer(seq(1,data[n,2],data[n,2]/100))){ #plotting reliabilty. Not necessary as of right now
    rely[tnow,1] <-   tnow
    rely[tnow,2] <-   exp(-Phi * (N0 - n) * tnow)}
  
  return(rely)
}

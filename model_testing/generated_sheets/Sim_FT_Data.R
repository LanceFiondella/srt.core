generate_FT <- function(){

N0 <- 100

interFail <- c()

JMLambda <- function(N0,Phi,i){
	return(Phi*(N0-(i-1)))
	}

GMLambda <- function(D0,Phi,i){
	return(Phi*(D)^i)
	}

for(i in 1:N0){
	rand <- runif(1)
	while(rand==0){
		rand <- runif(1)
	}
	interFail[i] <- -log(rand)/JMLambda(100,0.001,i)
}
failureTime = c()

for(i in 1:N0){
	if(i==1){
		failureTime[i] <- interFail[i]
		next
	}
	failureTime[i] <- interFail[i] + failureTime[i-1]
}
##print(failureTime)
##print(interFail)
	failureTime
}
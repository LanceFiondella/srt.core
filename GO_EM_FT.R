 #Vector of failure times data
tVec <- c(3, 33, 146, 227, 342, 351, 353, 444, 556, 571, 709, 759, 836, 860, 968, 1056, 1726, 1846, 1872, 1986, 2311, 2366, 2608, 2676, 3098, 3278, 3288, 4434, 5034, 5049, 5085, 5089, 5089, 5097, 5324, 5389, 5565, 5623, 6080, 6380, 6477, 6740, 7192, 7447, 7644, 7837, 7843, 7922, 8738, 10089, 10237, 10258, 10491, 10625, 10982, 11175, 11411, 11442, 11811, 12559, 12559, 12791, 13121, 13486, 14708, 15251, 15261, 15277, 15806, 16185, 16229, 16358, 17168, 17458, 17758, 18287, 18568, 18728, 19556, 20567, 21012, 21308, 23063, 24127, 25910, 26770, 27753, 28460, 28493, 29361, 30085, 32408, 35338, 36799, 37642, 37654, 37915, 39715, 40580, 42015, 42045, 42188, 42296, 42296, 45406, 46653, 47596, 48296, 49171, 49416, 50145, 52042, 52489, 52875, 53321, 53443, 54433, 55381, 56463, 56485, 56560, 57042, 62551, 62651, 62661, 63732, 64103, 64893, 71043, 74364, 75409,76057, 81542, 82702, 84566, 88682)
#Define n, tn and sumT
n <- length(tVec)
tn <- tVec[n]
sumT <- sum(tVec)

#Initial values of parameter 'a' and 'b'
b0 <- n/sumT
a0 <- n

#log-likelihood function


aEM <- numeric(0)
bEM <- numeric(0)
llEM <- numeric(0)


aEM[1] <- a0
bEM[1] <- b0
LLerror <- 1

sum1=0
for(j in 1:n){
	sum1=sum1+log(exp(-bEM[1]*tVec[j])*bEM[1]*aEM[1])
}

llEM[1] <- -(1-exp(-tn*bEM[1]))*aEM[1]+sum1
i <- 2
iterations <- 1

while(LLerror>10^-10 && iterations <= 10000){
	aEM[i] <- n+aEM[i-1]*exp(-bEM[i-1]*tn)
	bEM[i] <- (n+aEM[i-1]*exp(-bEM[i-1]*tn))/(sumT+aEM[i-1]*(tn+(1/bEM[i-1]))*exp(-bEM[i-1]*tn))
	
	sumi=0
for(j in 1:n){
	sumi=sumi+log(exp(-bEM[i]*tVec[j])*bEM[i]*aEM[i])
}
llEM[i] <- -(1-exp(-tn*bEM[i]))*aEM[i]+sumi
LLerror <- llEM[i]-llEM[i-1]

	i <- i+1
	
	if(is.na(aEM[length(aEM)])|is.na(bEM[length(bEM)])|is.na(llEM[length(llEM)])==TRUE){
		print("na")
	}
	iterations <- iterations+1
}
print(aEM[length(aEM)])
print(bEM[length(bEM)])
print(llEM[length(llEM)])



#plotting all the obtained values
#par(mfrow=c(2,2))
#plot(aEM, type="l",col="red",main="parameter a")
#plot(bEM, type="l",col="green",main="parameter b")
#plot(llEM, type="l",main="Improvements in log-likelihood")
#plot(aEM,bEM, type="b",main="parameter search")

#Newton's method routine-- doesnt' work !! :-(
#Model <- function(x){
 #c(
# f1=n+x[1]*exp(-x[2]*tn), f2=(n+x[1]*exp(-x[2]*tn))/(sumT+x[1]*(tn+(1/x[2]))*exp(-x[2]*tn))
# )	
#}
#multiroot(Model, c(a0,b0))
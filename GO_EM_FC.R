kVec <- c(15, 29, 22, 37, 2, 5, 36, 29, 4, 27, 17, 32, 21, 22, 6, 7, 9, 5, 3)
k <- length(kVec)
K <- sum(kVec)

tVec <- c(2.45, 4.9, 6.86, 7.84, 9.52, 12.89, 17.1, 20.47, 21.43, 23.35, 26.23, 27.67, 30.93, 34.77, 38.61, 40.91, 42.67, 44.66, 47.65)
tn <- tVec[length(tVec)]

#tVec[0]=0
tVec <- c(0,tVec)
sumT <- sum(tVec)
n<-length(tVec)

#Initial values of parameter 'a' and 'b'
a0 <- K	
b0 <- K/sumT

aEM <- numeric(0)
bEM <- numeric(0)
llEM <- numeric(0)

aEM[1] <- a0
bEM[1] <- b0

# Tau
#tau <- function(bEM){
#	taui=0
#	for(i in 2:k+1){
#taui=taui+((((tVec[i-1]+(1/bEM))*exp(-bEM*tVec[i-1]))-((tVec[i]+(1/bEM))*exp(-bEM*tVec[i])))/(exp(-bEM*tVec[i-1])-exp(-bEM*tVec[i])))
#}
#return(taui)
#}

#Splitting Tau -- 2
tau <- function(bEM){
	taui=0
	taui1=0
	taui2=0
	taui3=0
	taui4=0
	for(i in 2:k+1){
		taui1=((tVec[i-1]+(1/bEM))*exp(-bEM*tVec[i-1]))
		taui2=((tVec[i]+(1/bEM))*exp(-bEM*tVec[i]))
		taui3=(exp(-bEM*tVec[i-1]))
		taui4=(exp(-bEM*tVec[i]))
	taui=taui+((kVec[i-1]*(taui1-taui2))/(taui3-taui4))
}
return(taui)
}

#return(sum(taui,na.rm=TRUE))

kVecSum=0
kVectauSum=0

for(i in 1:k){
	kVecSum= kVecSum+kVec[i]
}


for(i in 2:29){
	aEM[i] <- kVecSum+(aEM[i-1]*exp(-bEM[i-1]*tn))
	bEM[i] <- aEM[i]/((tau(bEM[i-1]))+(aEM[i-1]*exp(-bEM[i-1]*tn)*(tn+(1/bEM[i-1]))))
}

print(aEM[length(aEM)])
print(bEM[length(bEM)])
#Splitting \tau
#((tVec[i-1]+(1/bEM[i-1]))*exp(-bEM[i-1]*tVec[i-1]))
#((tVec[i]+(1/bEM[i-1]))*exp(-bEM[i-1]*tVec[i]))
#exp(-bEM[i-1]*tVec[i-1])-exp(-bEM[i-1]*tVec[i])

mvf <- numeric(0)
mvf <- aEM[length(aEM)]*(1-exp(-bEM[length(bEM)]*tVec))





library(rootSolve)
a <- 200
b <- 0.001


mGO <- function(t){
	a*(1-exp(-b*t))
	}

mGOSolve <- function(t){
	 a*(1 - exp(-b*t))-0.99*a
	}
##print(mGO(4607.17))
test <- curve(mGO,0,4200)
##print(test)

t99 <- uniroot(mGOSolve,c(0,6000))$root

#print(t99)
points(t99,0,pch=16,cex=2)

##print(mGO(4607))

multiplier = 60

bins = ceiling(t99/multiplier)

#print(bins)

#curve(mGO,0,6000)

lstFC <- c()

for(i in 1:bins){
	lstFC[i] <- ppois((mGO(i*multiplier) - mGO((i-1)*multiplier)),1)
}
#print(lstFC)
sol <- qpois(lstFC,1)
#print(sol)
#print(sum(sol))
#print(sum(lstFC))

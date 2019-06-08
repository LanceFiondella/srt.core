# test_script3.R

require(gdata)

Mathematica_result <- function(model,dataset){
	for(i in length(get(paste(model,"methods",sep="_")))){
		method_name <- get(paste(model,"methods",sep="_"))[i]
	
		if(get(paste(model,"Results",sep="_"))$format=="xlsx"){
			d <- read.xls(paste(get(paste(model,"Results",sep="_"))$fileName,".xlsx",sep=''),sheet=paste(model,method_name,"Results",sep="_"))
		}
		else{
			d <- read.csv(paste(get(paste(model,"Results",sep="_"))$fileName,".csv",sep=''))
		}
	}
	d <- as.matrix(d)
	#!----> #print(d)
	#!----> #print(dimnames(d))
	dimnames(d) <- list(d[,1],dimnames(d)[[2]])
	#!----> #print(d)
	a <- data.frame()
	#!-----> dataset_row <<- as.matrix(d[paste(dataset),])
	for(i in 1:(length(get(paste(model,"params",sep="_"))))){
		a[1,i] <- d[dataset,get(paste(model,"params",sep="_"))[i]]
	}
	# !----> a <- data.frame(a[1])
	# !----> #print(a)
	# !----> #print(get(paste(model,"params",sep="_")))
	names(a) <- get(paste(model,"params",sep="_"))
	a
}

R_result <- function(){

}
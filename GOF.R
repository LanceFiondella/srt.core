#GOF.R


# Model Bias

model_bias <- function(x,y){
	t <- 0
	for(i in 1:length(x)){
		t <- ((x[i] - y[i]))/length(x) + t
	}
	t
}

mean_square_error <- function(x,y){
	t <- 0
	for(i in 1:length(x)){
		t <- ((x[i]-y[i])^2)/length(x) + t
	}
	t
}

mean_absolute_error <- function(x,y){
	t <- 0
	for(i in 1:length(x)){
		t <- abs((x[i]-y[i]))/length(x) + t
	}
	t
}

aic <- function(l,mle){
	return (2*p - 2*log(mle))
}

psse <- function(x,y){
	# takes mvf
	t <- 0
	n <- length(x)
	k <- n/10 + n%%10

	for( i in k:n){
		t <- (x[i] - y[i])^2 + t
	}
	t
}

library(gdata)


models_names_vector <-c("JM")

models_solution_list <- list(JM = c(.419029e+02, 3.601259e-05))

t <- read.xls('model_data.xlsx',sheet="SYS1")

header <- models_names_vector[1]
epsilon <- 5e-05
if(header=="JM"){
	sol <- JM_BM_MLE(t$IF)
	error <- sol - models_solution_list$JM
	error_check <- error < epsilon
	names(error_check) <- c("N0","phi")
	#print(error_check)

	if (sol==models_solution_list$JM){
		#print("JM - PASS")
	}
	else{
		#print("JM - FAIL")
	}
}
t$IF
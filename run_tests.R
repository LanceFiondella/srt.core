#!/usr/bin/env Rscript


source("tests/needed.R")
source('tests/generic_tests.R')
source('tests/test_functions.R')
model_directories <<- dir("./models", no..=TRUE)
cat("\nModels Detected: ")
cat(model_directories)
cat("\n\n")
listfunctions <-call("objects", envir = globalenv())
for(directory in model_directories){
	current_directory <<- paste(".","models",directory,sep="/")
	cat(paste("---------------------------------------------------","\n"))
	cat("|\tDIRECTORY:",paste(current_directory,"\n"))
	cat(paste("---------------------------------------------------","\n"))
	model_files <<- list.files(current_directory)
	if("Model_specifications.R" %in% model_files){
		cat(paste("|sourcing --> ",current_directory,"/Model_specifications.R","\n",sep=""))
		source(paste(current_directory,"/","Model_specifications.R",sep=""))
		current_model_input <<- paste(directory,"_input",sep="")
		modelspecifications_test(directory,file)
	}
	else{
		cat("Model Specification must be specified in 'Model_specifications.R' file\n")
		cat("Please Refer the models Folder for its layout.\n")
		quit("no",status = -1)
	}
	model_files <- model_files[model_files!="Model_specifications.R"]
	for(file in model_files){
		current_file <<- paste(current_directory,file,sep="/")
		cat(paste("|sourcing -->",current_file,"\n"))
		source(current_file)
	}
	cat(paste("|Testing Functions of ",directory,"\n",sep=""))
	model_functions_test(directory)
	cat("\n\n")	
}
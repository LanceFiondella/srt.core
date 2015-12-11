library('testthat')

source('sample.R')
source('tests/needed.R')

model_directories <- list.dirs("./models")
model_directories <- tail(model_directories,-1)
print(model_directories)
for(directory in model_directories){
	print(directory)
	model_files <- list.files(directory)
	for(file in model_files){
		current_file <- paste(directory,file,sep="/")
		print(current_file)
		source(current_file)
	}
}


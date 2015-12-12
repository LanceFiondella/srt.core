library('testthat')
GM_input <<- NULL
source('sample.R')
source('tests/needed.R')
source('tests/test_1.R')
model_directories <<- dir("./models", no..=TRUE)
print(model_directories)
for(directory in model_directories){
	current_directory <<- paste(".","models",directory,sep="/")
	print(current_directory)
	model_files <<- list.files(current_directory)
	for(file in model_files){
		current_file <<- paste(current_directory,file,sep="/")
		print(current_file)
		source(current_file)
		if(!is.null(file)){
			if(length(try(grep("Model_specifications.R",file)))){
  				current_model_input <<- paste(directory,"_input",sep="")
  				print(get(current_model_input))
				curr_model_test(directory,file)
			}
		}
	}
	
}


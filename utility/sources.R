# This sources the Model Specifications of models folder

model_directories <<- dir("./models")
cat("\nModels Detected: ")
cat(model_directories)
cat("\n\n")
for(directory in model_directories){
	current_directory <<- paste(".","models",directory,sep="/")
	cat(paste("--------------------------------------------------","\n"))
	cat("|\tDIRECTORY:",paste(current_directory,"\n"))
	cat(paste("--------------------------------------------------","\n"))
	model_files <<- list.files(current_directory)
	for(file in model_files){
		current_file <<- paste(current_directory,file,sep="/")
		cat(paste("|sourcing -->",current_file,"\n"))
		source(current_file)
	}
	cat("\n\n")
	
}




# Trend tests utility functions
source("trend_tests/RA_Test.R")
source("trend_tests/Laplace_trend_test.R")

# Plots utility function
source("utility/plots/Plot_Raw_Data.R")
source("utility/plots/Plot_Trend_Tests.R")
source("utility/plots/PlotModelResults.R")

# Data utility functions
sys.source("utility/data/Data_Tools.R")

# Tables utility functions
source("utility/tables/DataAndTrendTables.R")
source("utility/tables/ModelResultTable.R")

# Other utilities
source("utility/RunModels.R")      # Models run flow
source("utility/ErrorMessages.R")
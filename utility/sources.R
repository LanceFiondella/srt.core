# This sources the Model Specifications of models folder

model_directories <<- dir("./models")
print(model_directories)
for(directory in model_directories){
	current_directory <<- paste(".","models",directory,sep="/")
	print(current_directory)
	model_files <<- list.files(current_directory)
	for(file in model_files){
		current_file <<- paste(current_directory,file,sep="/")
		print(current_file)
		source(current_file)
	}
	
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
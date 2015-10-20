library(openxlsx)

source("Sim_FT_Data.R")

data <- generate_FT()

sheet_1 <- data.frame("FT"=data,"FN"=c(1:length(data)))

sheet_name <- "data_set"

file <- paste('Generated_sheets',paste("generated_sheets","xlsx",sep="."),sep="/")

#file <- paste('Generated_sheets',paste("generated_sheets","xls",sep="."),sep="/")

write.xlsx(sheet_1,file,sheetName="sheet1",col.names=TRUE)
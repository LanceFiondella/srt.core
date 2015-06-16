library(gdata)

library(crayon)
model_names <- c("JM","GM","GO","DSS","Wei")

model_types <- c("FT","FC","IF")
data_set_names <- c("SYS1",
                    "SYS2", 
                    "SYS3", 
                    "SS3", 
                    "CSR1", 
                    "CSR2", 
                    "CSR3", 
                    "S2", 
                    "S27", 
                    "DATA1", 
                    "DATA2", 
                    "DATA4", 
                    "DATA5", 
                    "DATA11", 
                    "DATA12", 
                    "DATA3", 
                    "DATA6", 
                    "DATA7", 
                    "DATA8", 
                    "DATA9", 
                    "DATA10")
model_method <- c("bisection","EM","AEM")

models_names_vector <-c("JM_SYS1",
                        "JM_SYS2", 
                        "JM_SYS3", 
                        "JM_SS3", 
                        "JM_CSR1", 
                        "JM_CSR2", 
                        "JM_CSR3", 
                        "JM_S2", 
                        "JM_S27", 
                        "JM_DATA1", 
                        "JM_DATA2", 
                        "JM_DATA4", 
                        "JM_DATA5", 
                        "JM_DATA11", 
                        "JM_DATA12", 
                        "JM_DATA3", 
                        "JM_DATA6", 
                        "JM_DATA7", 
                        "JM_DATA8", 
                        "JM_DATA9", 
                        "JM_DATA10")

models_solution_list <- list(JM_SYS1    = c(141.9029, 3.601259e-05), 
                             JM_SYS2    = c(105.01280722878384,0.0000164498),
                             JM_SYS3    = c(254.13334076433418,0.0001006408476738966),
                             JM_SS3     = c(397.48274208985634,0.000021827504063861793),
                             JM_CSR1    = c(400.406070734606,0.00004250615182355535),
                             JM_CSR2    = c(132.29530970924213,0.00003989427859673823),
                             JM_CSR3    = c(116.61453419346368,0.00014243744480465153),
                             JM_S2      = c(56.09914109172357,0.00002828501022091042),
                             JM_S27     = c(42.19600665119829,0.00004468942331113204),
                             JM_DATA1   = c(32.5018457134723,0.0012736176088952288),
                             JM_DATA2   = c(29.16614077277697,0.000032310338176006154),
                             JM_DATA4   = c(100.95291767198884,0.004351842800945552),
                             JM_DATA5   = c(16.353841085982356,0.0691563247314858),
                             JM_DATA11  = c(159.12229328426466,0.016320447110786857),
                             JM_DATA12  = c(346.02837265505735,0.00451452588777933 ),
                             JM_DATA3   = c(9.732609037430701e14,9.78546398728433e-16),
                             JM_DATA6   = c(3.071818460719228e8,3.255400774231519e-9 ),
                             JM_DATA7   = c(1.3819010374296608e9,7.236408483610434e-10 ),
                             JM_DATA8   = c(2.450677961052022e9,4.0805035926201086e-10 ),
                             JM_DATA9   = c(2.9546127360729012e9,3.384538424606103e-10 ),
                             JM_DATA10  = c(4.4892293017454785e8,2.2275538839547345e-9))

source("JM_BM.R")        # hardcoded must be changed



#model <- "JM"
#data_set <- "SYS1"

epsilon <- 1e-5
console_out_u<- function(model,data_set,request){
  begin <- make_style("yellow",bg=TRUE)
  
  cat(begin(format(model,width=9)))
  input_data <- read.xls('model_data.xlsx',sheet=data_set)
  #print(model)
  #print(data_set)
  info <- make_style("blue",bg = TRUE)
  cat(info(format(paste("TESTING Data: ",data_set),width=50)))
  #cat('\t',paste("Testing",model,data_set,sep="-"),'\n')
  sol <- JM_BM_MLE(input_data$IF)
  names(sol) <- c("N0","phi")
  cat('\n','\t',blue(format("Result [N0,phi]",width=15),":"),sol,'\n')
  error <- abs(sol - models_solution_list[[request]])
  names(error) <- c("N0","phi")
  cat('\n','\t',blue(format("Error [N0,phi]",width=15),":"),error,'\n')
  
  error_check <- error < epsilon
  names(error_check) <- c("N0","phi")
  cat('\n','\t',blue(format("PASS [N0,phi]",width=15),":"),error_check,'\n')
  PASS <- make_style("green",bg=TRUE)
  FAIL <- make_style("red",bg=TRUE)
  if (all(error_check)){
    result <- "PASS"  
    log <- paste(model,data_set,sep="-")
    cat('\n','\t',blue(format("Test ",width=15),":"),PASS(result),'\n')
  }
  else{
    result <- "FAIL"
    log <- paste(model,data_set,sep="-")
    cat('\n','\t',blue(format("Test ",width=15),":"),FAIL(result),'\n')
  }
}

for(model in model_names){
  for(data_set in data_set_names){
    request <- paste(model,data_set,sep="_")
    console_out_u(model,data_set,request)
  }
}
#console_out_u(model,data_set,request)


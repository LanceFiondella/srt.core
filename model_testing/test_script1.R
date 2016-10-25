#library(Rmpfr)
library(gdata)

library(crayon)
model_names <- c("JM","GM","GO","DSS","Wei")
#model_names <- c("GO")
model_types <- c("FT")#,"FC","IF")
data_set_names <- c("SYS1",
                    "SYS2", 
                    "SYS3", 
                    "CSR1", 
                    "CSR2", 
                    "CSR3", 
                    "S2", 
                    "S27",
                    "SS1",
                    "SS3",
                    "SS4",
                    "CDS",
                    "DATA1", 
                    "DATA2", 
                    "DATA3",
                    "DATA4", 
                    "DATA5", 
                    "DATA6", 
                    "DATA7", 
                    "DATA8", 
                    "DATA9", 
                    "DATA10",
                    "DATA11", 
                    "DATA12",
                    "DATA13",
                    "DATA14",
                    "J1",
                    "J2",
                    "J3",
                    "J4",
                    "J5")
model_method <- c("BM","EM")#,"AEM")

#model_method <- c("bisection","EM","AEM")

models_names_vector <-c("JM_SYS1",
                        "JM_SYS2", 
                        "JM_SYS3",
                        "JM_CSR1", 
                        "JM_CSR2", 
                        "JM_CSR3", 
                        "JM_S2", 
                        "JM_S27", 
                        "JM_SS3",
                        "JM_SS4",
                        #"JM_CDS",
                        "JM_DATA1", 
                        "JM_DATA2", 
                        "JM_DATA3",
                        "JM_DATA4", 
                        "JM_DATA5", 
                        "JM_DATA6", 
                        "JM_DATA7", 
                        "JM_DATA8", 
                        "JM_DATA9", 
                        "JM_DATA10",
                        "JM_DATA11", 
                        "JM_DATA12",
                        "JM_DATA13",
                        "JM_DATA14",
                        "JM_J1",
                        "JM_J2",
                        "JM_J3",
                        "JM_J4",
                        "JM_J5",
                        "GM_SYS1")

models_solution_list <- list(JM_SYS1    = c(141.9029, 3.601259e-05), 
                             JM_SYS2    = c(105.01280722878384,0.0000164498),
                             JM_SYS3    = c(254.13334076433418,0.0001006408476738966),
                             JM_SS3     = c(397.48274208985634,0.000021827504063861793),
                             JM_CSR1    = c(400.406070734606,0.00004250615182355535),
                             JM_CSR2    = c(132.29530970924213,0.00003989427859673823),
                             JM_CSR3    = c(116.61453419346368,0.00014243744480465153),
                             JM_S2      = c(56.09914109172357,0.00002828501022091042),
                             JM_S27     = c(42.19600665119829,0.00004468942331113204),
                             JM_SS4     = c(458.3133570927,0.0000111673309194836),
                             JM_CDS     = c(416.0175079834,0.045669176),
                             JM_SS1     = c(525.5945767,0.025958138),
                             JM_DATA1   = c(52.1575464989,0.0009140796),
                             JM_DATA2   = c(141.8919017386,3.62056881412066e-05),
                             JM_DATA3   = c(0,0),
                             JM_DATA4   = c(465.546093269,0.0258038579),
                             JM_DATA5   = c(324.504920638,0.1509708626),
                             JM_DATA6   = c(4957.8027003669,0.0801535662),
                             JM_DATA7   = c(603.9594098455,0.0200796608),
                             JM_DATA8   = c(496.8983624536,0.0308783265 ),
                             JM_DATA9   = c(0,0),
                             JM_DATA9   = c(2.9546127360729012e9,3.384538424606103e-10 ),
                             JM_DATA10  = c(1382.8789446542,0.0098073969),
                             JM_DATA11  = c(158.93512229328426466,0.0165275024),
                             JM_DATA12  = c(346.02837265505735,0.004663053),
                             JM_DATA13  = c(343.6221599303,0.0044672272),
                             JM_DATA14  = c(343.6221599303,0.0274737344),
                             JM_J1      = c(394.9823632328,0.0066384123),
                             JM_J2      = c(341.3959969171,0.005898621),
                             JM_J3      = c(416.0175079834,0.045669176),
                             JM_J4      = c(0,0),
                             JM_J5      = c(1539.0033457319,0.0037389305),
                             # GM answers are yet to be acquired
                             GM_SYS1    = c(0.0106303732, 0.9771147717),
                             GM_SYS2    = c(0.0021771701,0.9804820934),
                             GM_SYS3    = c(0.0303113058,0.9924125719),
                             GM_SS3     = c(0.0091426511,0.9961207799),
                             GM_CSR1    = c(0.0416559068,0.9908647468),
                             GM_CSR2    = c(0.0102250436,0.9763933617),
                             GM_CSR3    = c(0.0318841814,0.9748326506),
                             GM_S2      = c(0.0035296678,0.9420790458),
                             GM_S27     = c(0.003233156,0.9331362996),
                             GM_SS1     = c(15.5500415,0.99628735),
                             GM_SS4     = c(0.0050859569,0.9974684623),
                             GM_CDS     = c(18.1597071079,0.9963256067),
                             GM_DATA1   = c(0.0548838226,0.9665716468),
                             GM_DATA2   = c(0.0108873887,0.9772488448),
                             GM_DATA3   = c(1.8961963202,1.0077846808),
                             GM_DATA4   = c(11.9714348724,0.9969854446),
                             GM_DATA5   = c(55.674127861,0.9941380964),
                             GM_DATA6   = c(404.8064390009,0.9997160979),
                             GM_DATA7   = c(11.4574007252,0.9973296452),
                             GM_DATA8   = c(14.8038340894,0.9959414064),
                             GM_DATA9   = c(0.2494220303,1.0073770609),
                             GM_DATA10  = c(13.2939404153,0.999440071),
                             GM_DATA11  = c(3.1046119063,0.9883340033),
                             GM_DATA12  = c(1.7395502497,0.9953019486),
                             GM_DATA13  = c(1.9002980114,0.9942892986),
                             GM_DATA14  = c(11.1060742291,0.9958577314),
                             GM_J1      = c(2.6088654045,0.9971962224),
                             GM_J2      = c(1.7019800871,0.99739872890),
                             GM_J3      = c(18.1597071079,0.9963256067),
                             GM_J4      = c(1.0858372001,1.0049953002),
                             GM_J5      = c(5.7481376336,0.999296654),
                             GO_SYS1    = c(142.8809143,3.42038e-05),
                             GO_SYS2    = c(107.5452992,1.5671e-05),
                             GO_SYS3    = c(256.6252883,9.86501e-05),
                             GO_SS3     = c(402.0465808,2.14061e-05),
                             GO_CSR1    = c(401.0571356,4.21859e-05),
                             GO_CSR2    = c(133.1347672,3.89929e-05),
                             GO_CSR3    = c(118.2344546,0.000137741),
                             GO_S2      = c(57.12977887,2.67171e-05),
                             GO_SS1     = c(527.2946584,0.02575963),
                             GO_S27     = c(43.19763613,4.14377e-05),
                             GO_SS4     = c(478.800823,1.05517e-05),
                             GO_CDS     = c(418.1039286,0.045172529),
                             GO_DATA1   = c(69.53775702,0.000624892),
                             GO_DATA2   = c(142.8692884,3.5416e-05),
                             GO_DATA3   = c(-63.71227786,-0.026511414),
                             GO_DATA4   = c(469.9737114,0.025386904),
                             GO_DATA5   = c(326.4109023,469.9737114),
                             GO_DATA6   = c(4963.656759,0.080001587),
                             GO_DATA7   = c(605.5655154,0.019950081),
                             GO_DATA8   = c(497.7539634,0.030692046),
                             GO_DATA9   = c(-58.59123801,-0.003627513),
                             GO_DATA10  = c(1696.149663,0.007881269),
                             GO_DATA11  = c(162.6964276,0.015846835),
                             GO_DATA12  = c(350.378977,0.004451449),
                             GO_DATA13  = c(350.5791635,0.004330746),
                             GO_DATA14  = c(383.9826534,0.026921516),
                             GO_J1      = c(435.4677074,0.005901962),
                             GO_J2      = c(347.1424059,0.005741793),
                             GO_J3      = c(418.1039286,0.045172529),
                             GO_J4      = c(-77.65382974,-0.0108126),
                             GO_J5      = c(1625.411154,0.00351365))

epsilon <- 1e-3
console_out_u<- function(model,data_set,request){
  begin <- make_style("yellow",bg=TRUE)
  
  cat(begin(format(model,width=9)))
  input_data <- read.xls('model_data.xlsx',sheet=data_set)
  ##print(model)
  ##print(data_set)
  if(model=="JM"){
    if(length(grep("[DATA]",data_set)) > 0){
      
      source("Data_Format.R")
      CFC <- input_data$CFC > 0
      FC <- CumulativeFailureC_to_failureC(CFC)
      FT <-failureC_to_failureT(input_data$T,FC)
      IF <- failureT_to_interF(failure_T = FT)
      sol <- JM_BM_MLE(IF)
    }
    else if(length(grep("J",data_set))>0){
      source("Data_Format.R")
      CFC <- input_data$CFC > 0
      FC <-CumulativeFailureC_to_failureC(CFC)
      FT <-failureC_to_failureT(input_data$TI,FC)
      IF <- failureT_to_interF(failure_T = FT)
      sol <- JM_BM_MLE(IF)
    }
    else if(data_set=="CDS"){
      IF <- failureT_to_interF(input_data$FT)
      sol <- JM_BM_MLE(IF)
    }
    else{
      sol <- JM_BM_MLE(input_data$IF)
    }
    
    if(typeof(sol)=="double"){ 
    info <- make_style("blue",bg = TRUE)
    cat(info(format(paste("TESTING Data: ",data_set),width=50)))
    #cat('\t',paste("Testing",model,data_set,sep="-"),'\n')
    
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
  }
  else{
    CONV <- make_style('black',bg=TRUE)
    cat(CONV(format(paste("NONCONVERGENCE :",data_set),width=50)),'\n')
    return
  }
  }

  else if(model=="GM"){
    if(length(grep("[DATA]",data_set)) >0){
      source("Data_Format.R")
      CFC <- input_data$CFC > 0
      FC <- CumulativeFailureC_to_failureC(CFC)
      FT <-failureC_to_failureT(input_data$T,FC)
      IF <- failureT_to_interF(failure_T = FT)
      sol <- GM_BM_MLE(IF)
    }
    else if(length(grep("J",data_set))>0){
      source("Data_Format.R")
      CFC <- input_data$CFC > 0
      FC <-CumulativeFailureC_to_failureC(CFC)
      FT <-failureC_to_failureT(input_data$TI,FC)
      IF <- failureT_to_interF(failure_T = FT)
      sol <- GM_BM_MLE(IF)
    }
    else if(data_set=="CDS"){
      IF <- failureT_to_interF(input_data$FT)
      sol <- GM_BM_MLE(IF)
    }
    else{
      sol <- GM_BM_MLE(input_data$IF)
    }
    ##print(sol)
    if(typeof(sol)=="double"){ 
    info <- make_style("blue",bg = TRUE)
    cat(info(format(paste("TESTING Data: ",data_set),width=50)))
    #cat('\t',paste("Testing",model,data_set,sep="-"),'\n')
    
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
  }
  else{
    CONV <- make_style('black',bg=TRUE)
    cat(CONV(format(paste("NONCONVERGENCE :",data_set),width=50)),'\n')
    return
  }
  }



  else if(model=="GO"){
    if(length(grep("[DATA]",data_set)) >0){
      source("Data_Format.R")
      CFC <- input_data$CFC > 0
      FC <- CumulativeFailureC_to_failureC(CFC)
      FT <-failureC_to_failureT(input_data$T,FC)
      IF <- failureT_to_interF(failure_T = FT)
      sol <- GO_MLE(FT)
    }
    else if(length(grep("J",data_set))>0){
      source("Data_Format.R")
      CFC <- input_data$CFC > 0
      FC <-CumulativeFailureC_to_failureC(CFC)
      FT <-failureC_to_failureT(input_data$TI,FC)
      IF <- failureT_to_interF(failure_T = FT)
      sol <- GO_MLE(FT)
    }
    else if(data_set=="CDS"){
      IF <- failureT_to_interF(input_data$FT)
      sol <- GO_MLE(FT)
    }
    else{
      sol <- GO_MLE(input_data$FT)
    }
    ##print(sol)
    if(typeof(sol)=="double"){ 
    info <- make_style("blue",bg = TRUE)
    cat(info(format(paste("TESTING Data: ",data_set),width=50)))
    #cat('\t',paste("Testing",model,data_set,sep="-"),'\n')
    
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
  }
  else{
    CONV <- make_style('black',bg=TRUE)
    cat(CONV(format(paste("NONCONVERGENCE :",data_set),width=50)),'\n')
    return
  }
  }




  if(typeof(sol)=="double"){
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
}

for(model in model_names){
  
  for(data_set in data_set_names){
      if(model=="JM"){
        source("JM_BM.R")
        # if(data_set=="SS1"){
        #   next
        # }
        request <- paste(model,data_set,sep="_")
        console_out_u(model,data_set,request)       # hardcoded must be changed
      }
      else if(model =="GM"){
        source("GM_BM.R")
        # if(data_set=="SS1"){
        #   next
        # }
        request <- paste(model,data_set,sep="_")
        console_out_u(model,data_set,request)
      }

     
      else if(model=="GO"){
        for(type in model_types){
          for(method in model_method){
            source(paste(paste(model,method,type,sep="_"),".R",sep=""))
            # if(data_set=="SS1"){
            #   next
            # }
            # else if(data_set=="SS4"){
            #   next
            # }
            request <- paste(model,data_set,sep="_")
            console_out_u(model,data_set,request)
          }          
        }
      }
  }
}
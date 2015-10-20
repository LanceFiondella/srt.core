# Jelinksi-Moranda Model
JM_input <- c("IF")
JM_methods <- c("BM")
JM_params <- c("N0","Phi")
JM_numfailsparm <- c(1)
JM_fullname <- c("Jelinski-Moranda")
JM_plotcolor <- c("red")
JM_type <- c("FR","Exp")
JM_Finite <- TRUE
JM_Results<- data.frame("format"="xlsx","fileName"="model_data") # Sheet name is generated in test script, based on method.
# JM_Results<- data.frame("format"="xlsx","fileName"="model_data","sheet"="JM_Results")



# JM_MVF <- 
# JM_MTTF <-
# JM_FI <-
# JM_R <-
# JM_
# JM_prefer_method <- c("BM")


# Geometric Model
GM_input <- c("IF")
GM_methods <- c("BM")
GM_params <- c("D0","Phi")
GM_type <- c("Exp")
GM_numfailsparm <- c(0)
GM_fullname <- c("Geometric")
GM_plotcolor <- c("blue")
GM_Finite <- FALSE
GM_Results <- data.frame("format"="xlsx","fileName"="model_data")
#GM_prefer_method <- c("BM")


# Goel_okumoto model
GO_input <- c("FT")
GO_methods <- c("BM")
GO_params <- c("aMLE","bMLE")
GO_type <- c("NHPP","Exp")
GO_numfailsparm <- c(1)
GO_fullname <- c("Goel-Okumoto")
GO_plotcolor <- c("green")
GO_Finite <- TRUE
# GO_prefer_method <- c("BM")


# DSS model
DSS_input <- c("FT")
DSS_methods <- c("BM")
DSS_params <- c("aMLE","bMLE")
DSS_type <- c("")
DSS_numfailsparm <- c(1)
DSS_fullname <- c("Delayed S-Shape")
DSS_plotcolor <- c("yellow")
DSS_Finite <- TRUE

# Weibul model
Wei_input <- c("FT")
Wei_methods <- c("NM")
Wei_params <- c("aMLE","bMLE","cMLE")
Wei_type <- c("Exp")
Wei_numfailsparm <- c(1)
Wei_fullname <- c("Weibull")
Wei_plotcolor <- c("orange")
Wei_Finite <- TRUE
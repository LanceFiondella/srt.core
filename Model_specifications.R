# Jelinksi-Moranda Model
JM_input <- c("IF")
JM_methods <- c("BM")
JM_params <- c("N0","Phi")
JM_type <- c("FR","Exp")
JM_Finite <- TRUE
# JM_MVF <- 
# JM_MTTF <-
# JM_FI <-
# JM_R <-
# JM_
# JM_prefer_method <- c("BM")


# Geometric Model
GM_input <- c("IF")
GM_methods <- c("BM","EM")
GM_params <- c("D0","Phi")
GM_type <- c("Exp")
GM_Finite <- FALSE
#GM_prefer_method <- c("BM")


# Goel_okumoto model
GO_input <- c("FT")
GO_methods <- c("BM")
GO_params <- c("aMLE","bMLE")
GO_type <- c("NHPP","Exp")
# GO_Finite <- 
# GO_prefer_method <- c("BM")


# DSS model
DSS_input <- c("FT")
DSS_methods <- c("BM")
DSS_params <- c("aMLE","bMLE")
DSS_type <- c("")
# DSS_Finite <- 

# Weibul model
Wei_input <- c("FT")
Wei_methods <- c("NM")
Wei_params <- c("aMLE","bMLE","cMLE")
Wei_type <- c("Exp")
# Wei_Finite <-
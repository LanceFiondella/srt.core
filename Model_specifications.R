# Jelinksi-Moranda Model
JM_input <- c("IF")
JM_methods <- c("BM")
JM_params <- c("N0","Phi")
<<<<<<< HEAD
JM_type <- c("FR","exp")
JM_failcount <- c("finite")
JM_numfailsparm <- c(1)
JM_fullname <- c("Jelinski-Moranda")
JM_plotcolor <- c("red")
#JM_prefer_method <- c("BM")
=======
JM_type <- c("FR","Exp")
JM_Finite <- TRUE
# JM_MVF <- 
# JM_MTTF <-
# JM_FI <-
# JM_R <-
# JM_
# JM_prefer_method <- c("BM")
>>>>>>> pr/5


# Geometric Model
GM_input <- c("IF")
GM_methods <- c("BM","EM")
GM_params <- c("D0","Phi")
GM_type <- c("Exp")
<<<<<<< HEAD
GM_failcount <- c("infinite")
GM_numfailsparm <- c(0)
GM_fullname <- c("Geometric")
GM_plotcolor <- c("blue")
=======
GM_Finite <- FALSE
>>>>>>> pr/5
#GM_prefer_method <- c("BM")


# Goel_okumoto model
GO_input <- c("FT")
GO_methods <- c("BM")
GO_params <- c("aMLE","bMLE")
GO_type <- c("NHPP","Exp")
<<<<<<< HEAD
GO_failcount <- c("finite")
GO_numfailsparm <- c(1)
GO_fullname <- c("Goel-Okumoto")
GO_plotcolor <- c("green")
#GO_prefer_method <- c("BM")
=======
# GO_Finite <- 
# GO_prefer_method <- c("BM")
>>>>>>> pr/5


# DSS model
DSS_input <- c("FT")
DSS_methods <- c("BM")
DSS_params <- c("aMLE","bMLE")
DSS_type <- c("")
<<<<<<< HEAD
DSS_failcount <- c("finite")
DSS_numfailsparm <- c(1)
DSS_fullname <- c("Delayed S-Shape")
DSS_plotcolor <- c("yellow")
=======
# DSS_Finite <- 
>>>>>>> pr/5

# Weibul model
Wei_input <- c("FT")
Wei_methods <- c("NM")
Wei_params <- c("aMLE","bMLE","cMLE")
Wei_type <- c("Exp")
<<<<<<< HEAD
Wei_failcount <- c("finite")
Wei_numfailsparm <- c(1)
Wei_fullname <- c("Weibull")
Wei_plotcolor <- c("orange")
=======
# Wei_Finite <-
>>>>>>> pr/5

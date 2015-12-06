if (version$major < 3 || (version$major == 3 && version$minor < 2.2)){ #checks if R is already up to date
  #update R
  if(.Platform$OS.type == "windows"){#automatic update for windows users
    if(!require(installr)) {
      install.packages("installr"); library(installr)} #load / install+load installr
    
    # using the package:
    updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
  }
  else{
    #print explainationfor non-windows users
    stop("Your machine requires a newer version of R. Please download the updated version for your platform at https://cran.rstudio.com/")
  }
  
}

if(!require(testthat)) {install.packages("testthat")} #DT
#This file should be run the first time using this program to ensure R is up to date and all dependencies are intalled and loaded
#will aotomatically update R if on windows, and install all packages for any platform.
#run via source with echo in the top right drop down menu, or with Ctrl+Shift+Enter

if (version$major < 3 || (version$major == 3 && version$minor < 2.2)){ #checks if R is already up to date
  #update R
  if(.Platform$OS.type == "windows"){#automatic update for windows users
    if(!require(installr)) {
      install.packages("installr"); require(installr)} #load / install+load installr
    
    # using the package:
    updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
  }
  else{
    #print explainationfor non-windows users
    stop("Your machine requires a newer version of R. Please download the updated version for your platform at https://cran.rstudio.com/")
  }
  
}

#checks for each dependancy and loads or installs then loads
if(!require(shiny)) {install.packages("shiny");require(shiny)} #shiny is the main interface package
if(!require(DT)) {install.packages("DT");require(DT)} #DT
if(!require(gdata)) {install.packages("gdata");require(gdata)} #gdata is used for manipulating the data out of an excel sheet
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)} #ggplot2 is used for crerating various plots
if(!require(rootSolve)) {install.packages("rootSolve");require(rootSolve)} #rootSolve is used for finding a root of an equation

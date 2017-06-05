#!/usr/bin/env Rscript
#if(!require(testthat)) {
#	install.packages("testthat", repos="http://cran.rstudio.com/")
#	library("testthat")
#} #DT

#This file should be run the first time using this program to ensure R is up to date and all dependencies are intalled and loaded
#will aotomatically update R if on windows, and install all packages for any platform.
#run via source with echo in the top right drop down menu, or with Ctrl+Shift+Enter

if (version$major < 3 || (version$major == 3 && version$minor < 2.2)){ #checks if R is already up to date
  #update R
  if(.Platform$OS.type == "windows"){#automatic update for windows users
    if(!require(installr)) {
      install.packages("installr", repos="http://cran.rstudio.com/"); library(installr)} #load / install+load installr
    
    # using the package:
    updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
  }
  else{
    stop("Your machine requires a newer version of R. Please download the updated version for your platform at https://cran.rstudio.com/")
  }
  
}

#Uncomment the next 3 lines if docker has issues using an old version of
#htmltools. Warning: Travis will fail if the lines are uncommented
#remove.packages("htmltools")
#install.packages("htmltools")
#library(htmltools)

#checks for each dependancy and loads or installs then loads
install.packages("htmltools")
if(!require(shiny)) {install.packages("shiny", repos="http://cran.rstudio.com/");library(shiny)} #shiny is the main interface package
if(!require(DT)) {install.packages("DT", repos="http://cran.rstudio.com/");library(DT)} #DT
if(!require(knitr)) {install.packages("knitr", repos="http://cran.rstudio.com/");library(knitr)} #DT
if(!require(gdata)) {install.packages("gdata", repos="http://cran.rstudio.com/");library(gdata)} #gdata is used for manipulating the data out of an excel sheet
if(!require(ggplot2)) {install.packages("ggplot2", repos="http://cran.rstudio.com/");library(ggplot2)} #ggplot2 is used for crerating various plots
if(!require(rootSolve)) {install.packages("rootSolve", repos="http://cran.rstudio.com/");library(rootSolve)} #rootSolve is used for finding a root of an equation
if(!require(numDeriv)) {install.packages("numDeriv", repos="http://cran.rstudio.com/");library(numDeriv)} #numDeriv is used to build the Hessian for computing confidence intervals 
if(!require(emdbook)) {install.packages("emdbook", repos="http://cran.rstudio.com/", dep = TRUE);library(emdbook)}


Software Reliabilty Testing (http://srt.umassd.edu)

A web oriented software reliabilty testing suite. Made to parse data sets and predict failure models based on common statistical approaches.

Made by Lance Fiondella at the University of Massachusetts Dartmouth

Allen P. Nikora at Jet propulsion laboratory/ California Institute of technology
Team:
  Daniel Garcia
  Robert Davis
  Richard Muri
  Miles Wilhelms-Tricarico
  George Haddad
  Brain Etienne
  Eric J Pires
  Jarred Trottier
  Jimmy Rodriguez
  Rebecca E Field	
  William P Foley
  Karthik Katipally
  Sameer Bharadwaj Kencham
  Varun Kumar Reddy Garlapati
  Vidhyashree Nagaraju
  
Dependancies:
R version >= 3.0
shiny  R Package
gdata R Package
ggplot2 R Package
rootSolve R Package
knitr R Package
These can be resolved using the install.Packages() function from the R command Line
perl 5 version 16 or later

To Run:
Clone this git
Resolve the dependancies above
if you only have the r command line interpreter:
  From the R command line type:
    >>library(shiny)
    >>runApp("/Path/To/Dir/SRT/")
  This will launch the app on localhost on a random port and attempt to open your browser.

if you have the full RStudio environment:
  simply click Run App 

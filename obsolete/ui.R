library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
tags$head(includeScript("analytics/google-analytics.js")),
  # Application title
  titlePanel("Software Reliability Testing"),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Choose Input File',
                accept=c('text/csv','text/comma-separated-values,text/plain','Excel Spreadsheet','.csv','.xlsx')),
     # tags$hr(),
      radioButtons('type', 'File Type',
                   c('Excel (.xlsx)'=1,
                     'CSV (.csv)'=2),
        ),
      conditionalPanel(
      condition = "input.type == 2",
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ',')
      ),
     selectInput("TrendTest"," Select trend test",
                 c("Laplace"="LP","Running Average"="RA"),selected="LP"),
     selectInput("DataSet"," Select data set",
                 c("SYS1" = "SYS1",
                   "SYS2" = "SYS2",
                   "SYS3" = "SYS3",
                   "CSR1" = "CSR1",
                   "CSR2" = "CSR2",
                   "CSR3" = "CSR3",
                   "J1"   = "J1"  ,
                   "J2"   = "J2"  ,
                   "J3"   = "J3"  ,
                   "J4"   = "J4"  ,
                   "J5"   = "J5"  ,
                   "DATA1"= "DATA1",
                   "DATA2"= "DATA2", 
                   "DATA3"= "DATA3",
                   "DATA4"= "DATA4", 
                   "DATA5"= "DATA5", 
                   "DATA6"= "DATA6", 
                   "DATA7"= "DATA7", 
                   "DATA8"= "DATA8", 
                   "DATA9"= "DATA9", 
                   "DATA10"= "DATA10",
                   "DATA11"= "DATA11", 
                   "DATA12"= "DATA12",
                   "DATA13"= "DATA13",
                   "DATA14"= "DATA14"),
                 selected="SYS1"),
     # checkboxInput('reverse', "Reverse x and y", FALSE),
      selectInput("Model", "Select implementation model",
            c("No Model Selected"="NM","Jolinski-Moranda Model" = "JM",
              "Goel-Okumoto Model" = "GO",
              "Yamada S-Shaped" = "YS",
              "Geometric" = "GEO"),selected = NULL),
     
       conditionalPanel(condition="input.Model != 'NM'",checkboxInput('OD', 'View Original Data', TRUE)),
    width=3),


    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = "700px")
   ,width=8)
  ),
  includeHTML("analytics/clustrmaps.html"),
  includeHTML("analytics/statcounter.html"),

  h5( a("About Us", href="About"))

))


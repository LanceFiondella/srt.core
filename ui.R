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
      checkboxInput('reverse', "Reverse x and y", FALSE),
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
   ,width=9)
  ),
  includeHTML("analytics/clustrmaps.html"),
  includeHTML("analytics/statcounter.html"),

  h5( a("About Us", href="About"))

))


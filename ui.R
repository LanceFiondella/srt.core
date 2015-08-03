library(shiny)

#source("custom_functions.R")
shinyUI(navbarPage("Software Reliability Assessment in R",
                   tabPanel("Select, Analyze, and Filter Data",
                            
                            sidebarLayout(
                              sidebarPanel(h4("Select, Analyze, and Subset Failure Data"),
                                           fluidRow(
                                             column(10, 
                                                    h5("Specify the input file format"),
                                                    
                                                    radioButtons("type", label = "", 
                                                                 choices = list('Excel (.xlsx)' = 1, " CSV (.csv)" = 2), inline = TRUE, selected = 1)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(8, fileInput("file", label = h5("Select a failure data file"),
                                                                 accept=c('text/csv','text/comma-separated-values,text/plain','Excel Spreadsheet','.csv','.xlsx')))
                                           ),
                                           
                                           fluidRow(
                                             column(10,
                                                    uiOutput("sheetChoice")
                                                    
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(11, 
                                                    h5("Choose a view of the failure data."),
                                                    
                                                    selectInput("dataPlotChoice", label = "", 
                                                                choices = list("Times Between Failures" = "IF", "Cumulative Failures" = "CF",
                                                                               "Failure Intensity" = "FI"), selected = "CF")
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(11, 
                                                    radioButtons("DataPlotType", label = h6("Draw the plot with data points only, lines only, or both?"),
                                                                 choices = list("Both" = 1, "Points" = 2, "Lines" = 3), inline = TRUE,
                                                                 selected = 1)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(10,
                                                    radioButtons("PlotDataOrTrend", label = h6("Plot Data or Trend Test?"),
                                                                 choices = list("Data" = 1, "Trend test" = 2), inline = TRUE,
                                                                 selected = 1)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(11, 
                                                    h5("Does data show reliability growth?"),
                                                    selectInput("trendPlotChoice", label = "", 
                                                                choices = list("Laplace Test" = "LP", "Running Arithmetic Average" = "RA"))
                                             ),
                                             column(8, textOutput("trendMessage"))
                                           ),
                                           
                                           fluidRow(
                                             br(),
                                             column(8, downloadButton('saveDataOrTrend', 'Save Display'))
                                           ),
                                           fluidRow(
                                             column(8,
                                                    uiOutput("message")
                                             )
                                           ),
                                           fluidRow(
                                             br(),
                                             column(9, h5("Subset the failure data by category or data range")),
                                             column(9,
                                                    sliderInput("sliderDataSubsetChoice", h6("Select one or more failure categories to retain"),
                                                                min = 1, max = 5, value = c(1, 5), step = 1)),
                                             column(9,
                                                    sliderInput("modelDataRange", h6("Specify the data range to which models will be applied."),
                                                                min = 1, max = 5, value = c(1, 5), step = 1))
                                           )
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Plot", textOutput("InputFileError"), plotOutput("distPlot",height="700px")), 
                                  tabPanel("Data and Trend Test Table", dataTableOutput("dataAndTrendTable")),
                                  id="DataPlotAndTableTabset"),
                                width=8
                              )
                            )
                   ),
                   
                   tabPanel("Set Up and Apply Models",
                            
                            sidebarLayout(
                              sidebarPanel(h4("Configure and Apply Models"),
                                           h5("Set up the the initial parameter estimation interval and the number of failures for which the models will make predictions"),
                                           fluidRow(
                                             column(12,
                                                    sliderInput("parmEstIntvl", h6("Specify the last data point for the initial parameter estimation interval."),
                                                                min = 1, max = 4, value = 3)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12,
                                                    numericInput("modelNumPredSteps", 
                                                                 label = h6("Specify for how many failures into the future the models will predict"),
                                                                 min = 1, value = 1)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12,
                                                    actionButton("runModels", label = "Run Models")
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    br(),
                                                    h5("Choose the model results to display."),
                                                    selectInput("modelResultChoice", label = h6("Choose one or more sets of model results"), 
                                                                choices = list("Musa Basic" = 1, 
                                                                               "Musa Okumoto" = 2,
                                                                               "Geometric" = 3, 
                                                                               "Littlewood-Verrall Linear"=4,
                                                                               "Littlewood-Verrall Quadratic"=5,
                                                                               "Jelinksi-Moranda"=6,
                                                                               "Goel-okumoto"=7
                                                                               ), 
                                                                multiple=TRUE,
                                                                selected=6
                                                                )
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    h5("Choose the type of plot for model results."),
                                                    selectInput("modelPlotChoice", label = h6("Choose a plot type"), 
                                                                choices = list("Times Between Failures" = 1, "Cumulative Failures" = 2,
                                                                               "Failure Intensity" = 3, "Reliability" = 4), selected = 2)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    checkboxInput("checkboxDataOnPlot", label = "Show data on plot", value = TRUE)
                                             )
                                           ),
                                           
                                           # ModelPlotType was used .. may thats more right but changed it to rapid functionality programming
                                           fluidRow(
                                             column(12, 
                                                    radioButtons("ModelDataPlotType", label = h6("Specify how to draw the plot"),
                                                                 choices = list("Data points and lines" = 1, "Data points only" = 2, "Lines only" = 3),
                                                                 selected = 1)
                                             )
                                           ),width=4
                                           
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Model Result Plot", textOutput("ModelConfigError"), plotOutput("ModelPlot", height = "700px")), 
                                  tabPanel("Model Result Table", dataTableOutput("ModelResultTable")),
                                  id="ModelPlotAndTableTabset"), width=8
                            )
                          )
                   ),
                   
                   tabPanel("Query Model Results",
                            sidebarLayout(
                              sidebarPanel(h4("Make Detailed Predictions From Model Results"),
                                           fluidRow(
                                             column(12, 
                                                    br(),
                                                    h5("Choose one or more models for which detailed predictions will be made."),
                                                    selectInput("modelDetailChoice", label = h6("Choose one or more sets of model results"), 
                                                                choices = list("Musa Basic" = "Musa", 
                                                                               "Musa Okumoto" = 2,
                                                                               "Geometric" = "Geometric", 
                                                                               "Littlewood-Verrall Linear"=4,
                                                                               "Littlewood-Verrall Quadratic"=5,
                                                                               "Jelinksi-Moranda"="Jelinski-Moranda",
                                                                               "Goel-okumoto"=7
                                                                               ), 
                                                                multiple=TRUE,
                                                                )
                                             ),
                                             
                                             column(12, 
                                                    h5("How much time will be required to observe the next N failures")
                                             ),
                                             
                                             column(12,
                                                    numericInput("modelDetailPredFailures", 
                                                                 label = h6("Specify the number of failures that are to be observed."),
                                                                 min = 1, value = 1)
                                             ),
                                             
                                             column(12, 
                                                    h5("How many failures will be observed over the next N seconds?")
                                             ),
                                             
                                             column(12,
                                                    numericInput("modelDetailPredTime", 
                                                                 label = h6("Specify the amount of additional time for which the software will run."),
                                                                 min = 1, value = 1)
                                             ),
                                             
                                             column(12, 
                                                    h5("How much more test time to achieve a specified reliability?")
                                             ),
                                             
                                             column(12,
                                                    numericInput("modelTargetReliability", 
                                                                 label = h6("Specify the desired reliability."),
                                                                 min = 0, max = 1, value = 0.9, step = 0.01)
                                             ),
                                             
                                             column(12,
                                                    numericInput("modelRelMissionTime", 
                                                                 label = h6("Specify the length of the interval for which reliability will be computed"),
                                                                 min = 0, value = 1)
                                             )
                                           )
                              ),
                              
                              mainPanel(
                                dataTableOutput('mytable1')
                              )
                            )
                   ),
                   
                   tabPanel("Evaluate Models",
                            sidebarLayout(
                              sidebarPanel(h4("Evaluate Model Goodness-of-Fit and Applicability"),
                                           fluidRow(
                                             column(12, 
                                                    h5("Select a model evaluation technique to apply"),
                                                    selectInput("modelEvalChoice", label = h6("Choose a model evaluation test"), 
                                                                choices = list("Kolmogorov-Smirnov GOF Test" = "KS", "-ln Prequential Likelihood" = "LPL",
                                                                               "Prequential Likelihood Ratio" = "PLR", "Akaike Information Criterion" = "AIC",
                                                                               "Model Bias" = "MB", "Model Bias Trend" = "BT"), selected = "PLR")
                                             ),
                                             
                                             column(12,
                                                    numericInput("numericEvalSigValue", 
                                                                 label = h6("Specify the significance level for the selected test"),
                                                                 min = 0, max = 1, step = 0.001,
                                                                 value = .05)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    radioButtons("radioEvalPlotType", label = h6("Draw plots with data points only, lines only, or both?"),
                                                                 choices = list("Both" = 1, "Points" = 2, "Lines" = 3), inline = TRUE,
                                                                 selected = 1)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             h5("Rank models by evaluation criteria"),
                                             column(1, ""),
                                             column(11, 
                                                    radioButtons("radioGOFRankEvalOrder", label = h6("Goodness of Fit"),
                                                                 choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                                                 selected = 1, inline = TRUE)
                                             ),
                                             column(2, ""),
                                             column(10, 
                                                    checkboxInput("checkboxGOFScreen", label = "Use GOF test as screen", value = TRUE)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(1, ""),
                                             column(11, 
                                                    radioButtons("radioAICRankEvalOrder", label = h6("Akaike Information Criterion"),
                                                                 choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                                                 selected = 2, inline = TRUE)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(1, ""),
                                             column(11, 
                                                    radioButtons("radioPLRankEvalOrder", label = h6("Prequential Likelihood"),
                                                                 choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                                                 selected = 3, inline = TRUE)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(1, ""),
                                             column(11, 
                                                    radioButtons("radioBiasRankEvalOrder", label = h6("Model Bias"),
                                                                 choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                                                 selected = 4, inline = TRUE)
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(1, ""),
                                             column(11, 
                                                    radioButtons("radioBiasTrendRankEvalOrder", label = h6("Model Bias Trend"),
                                                                 choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                                                 selected = 5, inline = TRUE)
                                             )
                                           )
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Table',dataTableOutput('mytable2')),
                                  tabPanel("Plot",plotOutput("Evalationplot"))
                                )                              
                              )
                            )
                   )
))
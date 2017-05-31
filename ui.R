library(shiny)
#models <- list("Geometric"="GM", "Jelinski-Moranda"="JM", "Goel-okumoto"="GO","Delayed-S"="DSS", "Weibull"="Wei","Example New Model"='ZZZZ')
#source("custom_functions.R")
tags$head(includeScript("analytics/google-analytics.js"))
tags$head()
shinyUI(navbarPage("Software Reliability Assessment in R",
                   tabPanel("Select, Analyze, and Filter Data",
                            
                            sidebarLayout(
                              sidebarPanel(h4("Select, Analyze, and Subset Failure Data"),
                                           #fluidRow(
                                           #  column(10, 
                                           #         h5("Upload an input file in csv or Excel format (.xls or .xlsx)"),
                                           #         
                                           #         radioButtons("type", label = "", 
                                           #                      choices = list('Excel (.xlsx)' = 1, " CSV (.csv)" = 2), inline = TRUE, selected = 1)
                                           #  )
                                           #),
                                           
                                           fluidRow(
                                             column(8, 
                                              h5("Upload an input file in CSV (Comma Separated Value) or Excel format (.xls or .xlsx)"),
                                             fileInput("file", label = h5("Select a failure data file"),
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
                                                    radioButtons("DataPlotType", label = h6("Draw the plot with data points and lines, points only, or lines only?"),
                                                                 choices = list("Both" = "points_and_lines", "Points" = "points", "Lines" = "lines"),
                                                                 inline=TRUE,
                                                                 selected = "points_and_lines")
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
                                             column(11,
                                                    conditionalPanel(
                                                      condition = "input.trendPlotChoice == 'LP'",
                                                      numericInput("confidenceLP", 
                                                                   label = h6("Specify the confidence level for the Laplace Test"),
                                                                   min = 0, max=1, value = 0.9, step=0.01)
                                                    )
                                             ),
                                             column(8, textOutput("trendMessage"))
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    radioButtons("saveDataFileType", label = h6("Choose the type of file to save plots.  Tables are saved as CSV files."),
                                                                 choices = list("JPEG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                                                                 selected = "JPG")
                                             ),
                                             column(8, downloadButton('saveDataOrTrend', 'Save Display'))
                                           ),
                                           fluidRow(
                                             column(8,
                                                    uiOutput("message")
                                             )
                                           ),
                                           
                                           fluidRow(
                                             br(),
                                             column(9, h5("Subset the failure data by data range")),
                                             #     column(9,
                                             #            sliderInput("sliderDataSubsetChoice", h6("Select one or more failure categories to retain"),
                                             #                        min = 1, max = 5, value = c(1, 5), step = 1)),
                                             column(9,
                                                    sliderInput("modelDataRange", h6("Specify the data range to which models will be applied."),
                                                                min = 1, max = 5, value = c(1, 5), step = 1))
                                           )
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Plot", textOutput("InputFileError"), textOutput("DataSubsetError"), plotOutput("DataAndTrendPlot",width="100%",height="50%",dblclick="DTPdblclick", brush=brushOpts(id="DTP_brush", resetOnNew=TRUE))), 
                                  tabPanel("Data and Trend Test Table", DT::dataTableOutput("dataAndTrendTable")),
                                  id="DataPlotAndTableTabset")
                              )
                            )
                   ),
                   
                   tabPanel("Set Up and Apply Models",
                            
                            sidebarLayout(
                              sidebarPanel(h4("Configure and Apply Models"),
                                           h5("Specify the number of failures for which the models will make predictions"),
                                           
                                           fluidRow(
                                             column(12,
                                                    uiOutput("ParameterInterval")
                                             ),
                                             
                                             column(12,
                                                    numericInput("parmConfInterval",
                                                                 label=h6("Specify a confidence interval for parameter estimates."),
                                                                 min=0.01, max=0.9999, step=0.01, value=0.95)
                                             ),
                                             
                                             column(12,
                                                    numericInput("modelNumPredSteps", 
                                                                 label = h6("Specify for how many failures into the future the models will predict"),
                                                                 min = 1, value = 1)
                                             ),

                                             column(12, 
                                                    selectInput(
                                                      "modelsToRun", label = h6("Choose one or more models to run, or exclude one or more models."), 
                                                      choices=list("Open a data set to run models"="None"),
                                                      multiple=TRUE, selected="None"
                                                    )
                                             )
                                           ),
                                           
                                           fluidRow(
                                             column(12,
                                                    actionButton("runModels", label = "Run Selected Models")
                                             )
                                           ),
                                           
                                           fluidRow(
                                             br(),
                                             h4("Display Model Results")
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    #<<<<<<< HEAD
                                                    selectInput(
                                                      "modelResultChoice", label = h6("Choose one or more sets of model results to display."), 
                                                      choices=list("No model results to display"="None"),
                                                      multiple=TRUE, selected="None"
                                                    )
                                                    #=======
                                                    #           br(),
                                                    #           h5("Choose the model results to display."),
                                                    #           selectInput("modelResultChoice", label = h6("Choose one or more sets of model results"), 
                                                    #                       choices= models,
                                                    #                       multiple=TRUE
                                                                
                                                    #           )
                                                    ##>>>>>>> lfiondella/master
                                             )
                                           ),
                                           
                                           fluidRow(
                                             #column(12, 
                                             #       h5("Choose the type of plot for model results."),
                                             #      selectInput("modelPlotChoice", label = h6("Choose a plot type"), 
                                             #                   choices = list("Times Between Failures" = "MTTF", "Cumulative Failures" = "MVF",
                                             #                                 "Failure Intensity" = "FI", "Reliability" = "R","Reliability Growth"="R_growth"), selected = "MVF")
                                             #),
                                             column(12, 
                                                    h5("Choose the type of plot for model results."),
                                                    selectInput("modelPlotChoice", label = h6("Choose a plot type"), 
                                                                choices = list("Times Between Failures" = "MTTF", "Cumulative Failures" = "MVF",
                                                                               "Failure Intensity" = "FI", "Reliability Growth"="R_growth"), selected = "MVF")
                                             ),
                                             column(12,
                                                    conditionalPanel(
                                                      condition = "input.modelPlotChoice == 'R_growth'",
                                                      numericInput("modelRelMissionTime", 
                                                                   label = h6("Specify the length of the interval for which reliability will be computed"),
                                                                   min = 0, value = 1)
#                                                      numericInput("modelTargetReliability",
#                                                                   label=h6("Specify the reliability to be achieved"),
#                                                                   min=0, max=1, step=0.01, value=0.9) 
                                                      
                                                    )
                                             ),
                                             column(12,
                                                    numericInput("modelCurveAdditionalTime",
                                                                 label=h6("Enter the duration for which the model results curves should extend beyond the last prediction point."),
                                                                 min=0, value=100, step=1000)
                                                    )
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    checkboxInput("checkboxDataOnPlot", label = "Show data on plot", value = TRUE)
                                             ),
                                             column(12, 
                                                    checkboxInput("checkboxDataEndOnPlot", label = "Show end of data on plot", value = TRUE)
                                             )
                                           ),
                                           
                                           # ModelPlotType was used .. may thats more right but changed it to rapid functionality programming
                                           fluidRow(
                                             column(12, 
                                                    radioButtons("ModelDataPlotType", label = h6("Draw the plot with data points and lines, points only, or lines only?"),
                                                                 choices = list("Both" = "points_and_lines", "Points" = "points", "Lines" = "lines"),
                                                                 inline=TRUE,
                                                                 selected = "points_and_lines")
                                             )
                                             
                                           ),
                                           
                                           fluidRow(
                                             column(12, 
                                                    radioButtons("saveModelResultsType", label = h6("Choose the type of file to save plots.  Tables are saved as CSV files."),
                                                                 choices = list("JPEG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                                                                 selected = "JPG")
                                             ),
                                             column(8, downloadButton(outputId = "saveModelResults", label = "Save"))
                                           ),width=4
                                           
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Model Result Plot", 
                                           textOutput("ModelConfigError"),
                                           textOutput("UnsuccessfulModels"),
                                           fluidRow (
                                             column(6,
                                                    h6("Show most likely parameter values or confidence bounds.")
                                             ),
                                             column(1,
                                                    checkboxInput("LowConfOnRsltPlot", label = "Low", value = FALSE)
                                             ),
                                             column(3,
                                                    checkboxInput("MLEOnRsltPlot", label = "Most Likely", value = TRUE)
                                             ),
                                             column(2,
                                                    checkboxInput("HighConfOnRsltPlot", label = "High", value = FALSE)
                                             )
                                           ),
                                           plotOutput("ModelPlot", dblclick="MPdblclick", brush=brushOpts(id="MP_brush", resetOnNew=TRUE))), 
                                  tabPanel("Model Result Table",
                                           fluidRow (
                                             column(6,
                                                    selectInput(
                                                      "AllModelsRun", label = h6("Choose one or more sets of model results to display."), 
                                                      choices=list("No model results to display"="None"),
                                                      multiple=TRUE, selected="None")
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h6("Show most likely parameter values or confidence bounds.")
                                                      ),
                                               column(1,
                                                      checkboxInput("LowConfOnTable", label = "Low", value = FALSE)
                                               ),
                                               column(3,
                                                      checkboxInput("MLEOnTable", label = "Most Likely", value = TRUE)
                                               ),
                                               column(2,
                                                      checkboxInput("HighConfOnTable", label = "High", value = FALSE)
                                               )
                                             )
                                           ),
                                           DT::dataTableOutput("ModelResultTable")),
                                  id="ModelPlotAndTableTabset"), width=8
                              )
                            )
                   ),
                   
                   tabPanel("Query Model Results",
                            sidebarLayout(
                              sidebarPanel(h4("Make Detailed Predictions From Model Results"),
                                           fluidRow(
                                             column(12,
                                                    selectInput(
                                                      "modelDetailChoice", label = h6("Choose one or more sets of model results to display."), 
                                                      choices=list("No model results to display"="None"),
                                                      multiple=TRUE, selected="None"
                                                    )
                                                    #           br(),
                                                    #           h5("Choose one or more models for which detailed predictions will be made."),
                                                    #           selectInput("modelDetailChoice", label = h6("Choose one or more sets of model results"), 
                                                    #                       choices=models,
                                                    #                       multiple=TRUE,
                                                    #                       )
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
                                                    h5("How many failures will be observed over the next N time units?")
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
                                                    numericInput("modelRelMissionTime2", 
                                                                 label = h6("Specify the length of the interval for which reliability will be computed"),
                                                                 min = 0, value = 1)
                                             ),
                                             column(12,
                                                    h5("Optimal Release Time Input")
                                                    
                                             ),
                                             column(12,
                                                    numericInput("T", 
                                                                 label = h6("Software lifecycle"),
                                                                 min = 0, value = 100000)
                                             ),
                                             
                                             column(12,
                                                    numericInput("C0", 
                                                                 label = h6("Expected cost of removing fault during testing."),
                                                                 min = 0, value = 50)
                                             ),
                                             column(12,
                                                    numericInput("C1", 
                                                                 label = h6("Expected cost of removing the fault during operation."),
                                                                 min = 0, value = 1000)
                                             ),
                                             column(12,
                                                    numericInput("C2", 
                                                                 label = h6("Expected cost per unit time of software testing."),
                                                                 min = 0, value = 1)
                                             ),
                                             column(12, 
                                                    radioButtons("queryResultsPlotType", label = h6("Draw the plot with data points and lines, points only, or lines only?"),
                                                                 choices = list("Both" = "points_and_lines", "Points" = "points", "Lines" = "lines"),
                                                                 inline=TRUE,
                                                                 selected = "points_and_lines"),
                                                    radioButtons("saveQueryResultsType", label = h6("Choose the type of file to save plots."),
                                                                 choices = list("JPG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                                                                 selected = "JPG")
                                             ),
                                             
                                             column(8, downloadButton(outputId = "saveQueryResults", label = "Save")),
                                             
                                             column(12, 
                                                    radioButtons("saveModelDetailsType", label = h6("Save detailed model results as PDF or CSV?"),
                                                                 choices = list("CSV" = "CSV", "PDF" = "PDF"), inline = TRUE,
                                                                 selected = "CSV"),
                                                    downloadButton('downloadData', 'Save Model Predictions')
                                             )
                                           )
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Prediction table",
                                    DT::dataTableOutput('mytable1')
                                    ),
                                  tabPanel("Model Prediction Result Plot", 
                                    # textOutput("ModelConfigError"), 
                                    # textOutput("UnsuccessfulModels"), 
                                    plotOutput("ModelPredictionPlot", dblclick="MPdblclick1", brush=brushOpts(id="MP_brush1", resetOnNew=TRUE))
                                    ),
                                 # DT::dataTableOutput('mytable1')
                                 id="ModelPredictionPlotAndTableTabset"
                                ), width=8
                              )
                        )
                            
                   ),
                   
                   tabPanel("Evaluate Models",
                            sidebarLayout(
                                sidebarPanel(h4("Evaluate Model goodness of fit and Applicability"),
                                    fluidRow(
                                     column(12, 
                                            br(),
                                            h5("Choose one or more models for which the results will be evaluated."),
                                            selectInput(
                                              "modelResultsForEval", label = h6("Choose one or more sets of model results"), 
                                              choices=list("No model results to display"="None"),
                                              multiple=TRUE, selected="None"
                                            )
                                     )
                                    ),
                                             
                                    fluidRow(
#                                             column(12, 
#                                                    h5("Select a model evaluation technique to apply"),
#                                                    selectInput("modelEvalChoice", label = h6("Choose a model evaluation test"), 
#                                                                choices = list("Kolmogorov-Smirnov GOF Test" = "KS", "-ln Prequential Likelihood" = "LPL",
#                                                                               "Prequential Likelihood Ratio" = "PLR", "Akaike Information Criterion" = "AIC",
#                                                                               "Predictive Sum Of Squares" = "PSSE"
#                                                                               ), selected = "PSSE")
#                                             ),
                                             
                                             column(12,
#                                                    numericInput("numericEvalSigValue", 
#                                                                 label = h6("Specify the significance level for the selected test"),
#                                                                 min = 0, max = 1, step = 0.001,
#                                                                 value = .05),
                                                    numericInput("percentData", 
                                                                 label = h6("Specify the Percent Data for PSSE"),
                                                                 min = 0.1, max = 1.0, step = 0.001,
                                                                 value = .90)
                                             ),
                                             
                                             column(12, 
                                                    radioButtons("saveModelEvalType", label = h6("Save model evaluations as PDF or CSV?"),
                                                                 choices = list("CSV" = "CSV", "PDF" = "PDF"), inline = TRUE,
                                                                 selected = "CSV"),
                                                    downloadButton('saveModelEvals', 'Save Model Evaluations')
                                             )

                                           )

                                  ),
                              
                            # sidebarLayout(
                            #   sidebarPanel(h4("Evaluate Model Goodness-of-Fit and Applicability"),
                            #                 fluidRow(
                            #                  column(12, 
                            #                         br(),
                            #                         h5("Choose the model results to display."),
                            #                         selectInput("EvalResultChoice", label = h6("Choose one or more sets of model results"), 
                            #                                     models,
                            #                                     multiple=TRUE
                                                              
                            #                                     )
                            #                  )
                            #                ),
                                           
                                           
                            #                fluidRow(
                            #                  column(12, 
                            #                         radioButtons("radioEvalPlotType", label = h6("Draw the plot with data points and lines, points only, or lines only?"),
                            #                                      choices = list("Both" = 1, "Points" = 2, "Lines" = 3), inline = TRUE,
                            #                                      selected = 1)
                            #                  )
                            #                ),
                                           
                            #                fluidRow(
                            #                  column(12, 
                            #                         radioButtons("radioEvalPlotType", label = h6("Draw the plot with data points and lines, points only, or lines only??"),
                            #                                      choices = list("Both" = 1, "Points" = 2, "Lines" = 3), inline = TRUE,
                            #                                      selected = 1)
                            #                  )
                            #                ),
                                           
                            #                fluidRow(
                            #                  h5("Rank models by evaluation criteria"),
                            #                  column(1, ""),
                            #                  column(11, 
                            #                         radioButtons("radioGOFRankEvalOrder", label = h6("Goodness of Fit"),
                            #                                      choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                            #                                      selected = 1, inline = TRUE)
                            #                  ),
                            #                  column(2, ""),
                            #                  column(10, 
                            #                         checkboxInput("checkboxGOFScreen", label = "Use GOF test as screen", value = TRUE)
                            #                  )
                            #                ),
                                           
                            #                fluidRow(
                            #                  column(1, ""),
                            #                  column(11, 
                            #                         radioButtons("radioAICRankEvalOrder", label = h6("Akaike Information Criterion"),
                            #                                      choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                            #                                      selected = 2, inline = TRUE)
                            #                  )
                            #                ),
                                           
                            #                fluidRow(
                            #                  column(1, ""),
                            #                  column(11, 
                            #                         radioButtons("radioPLRankEvalOrder", label = h6("Prequential Likelihood"),
                            #                                      choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                            #                                      selected = 3, inline = TRUE)
                            #                  )
                            #                ),
                                           
                            #                fluidRow(
                            #                  column(1, ""),
                            #                  column(11, 
                            #                         radioButtons("radioBiasRankEvalOrder", label = h6("Model Bias"),
                            #                                      choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                            #                                      selected = 4, inline = TRUE)
                            #                  )
                            #                ),
                                           
                            #                fluidRow(
                            #                  column(1, ""),
                            #                  column(11, 
                            #                         radioButtons("radioBiasTrendRankEvalOrder", label = h6("Model Bias Trend"),
                            #                                      choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                            #                                      selected = 5, inline = TRUE)
                            #                  )
                            #                )
                            #   ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel('Table',DT::dataTableOutput('mytable2'))
                                  #tabPanel("Plot",plotOutput("Evalationplot"))
                                )                              
                              )
                            )
                   )
#tags$footer(includeHTML("analytics/clustrmaps.html")),
#tags$footer(includeHTML("analytics/statcounter.html"))
))

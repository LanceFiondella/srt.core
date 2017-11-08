library(shiny)
#models <- list("Geometric"="GM", "Jelinski-Moranda"="JM", "Goel-okumoto"="GO","Delayed-S"="DSS", "Weibull"="Wei","Example New Model"='ZZZZ')
#source("custom_functions.R")
tags$head(includeScript("analytics/google-analytics.js"))
tags$head()

shinyUI(navbarPage("Software Reliability Assessment in R",
                   tabPanel("Select, Analyze, and Filter Data",
                            
                            sidebarLayout(
                               sidebarPanel( 
                                uiOutput("languageChoice"),
                                uiOutput("tab1UI")

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
                              sidebarPanel(
                               uiOutput("tab2UI")
                               
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Model Result Plot", textOutput("ModelConfigError"), textOutput("UnsuccessfulModels"), plotOutput("ModelPlot", dblclick="MPdblclick", brush=brushOpts(id="MP_brush", resetOnNew=TRUE))), 
                                  tabPanel("Model Result Table",
                                           selectInput(
                                             "AllModelsRun", label = h6("Choose one or more sets of model results to display."), 
                                             choices=list("No model results to display"="None"),
                                             multiple=TRUE, selected="None"),
                                           DT::dataTableOutput("ModelResultTable")),
                                  id="ModelPlotAndTableTabset"), width=8
                              )
                            )
                   ),
                   
                   tabPanel("Query Model Results",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("tab3UI")
                                
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
                                sidebarPanel(
                                  uiOutput("tab4UI")
                                  
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

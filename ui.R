library(shiny)
#models <- list("Geometric"="GM", "Jelinski-Moranda"="JM", "Goel-okumoto"="GO","Delayed-S"="DSS", "Weibull"="Wei","Example New Model"='ZZZZ')
#source("custom_functions.R")
tags$head(includeScript("analytics/google-analytics.js"))
tags$head()
#uiOutput("languageChoice"),
source("utility/UIsettings.R")


shinyUI(navbarPage(translate("str_title"),
                   tabPanel(translate("str_tab1"),
                            
                            sidebarLayout(
                               sidebarPanel( 
                               fluidRow(
                                h4(translate("str_select")),
                                    column(10,
                                        uiOutput("sheetChoice")    
                                    ),
                                    column(8, 
                                    h5(translate("str_upload")),
                                    fileInput("file", label = h5(translate("str_failure")),
                                                        accept=c('text/csv','text/comma-separated-values,text/plain','Excel Spreadsheet','.csv','.xlsx'))),
                                    
                                    column(11, 
                                    
                                        selectInput("dataPlotChoice", label = translate("str_choose"), 
                                                    #choices = list(interF = "IF", "Cumulative Failures" <- "CF","Failure Intensity" = "FI"), 
                                                    choices = setNames(list("IF", "CF", "FI"), c(translate("sel_tfailure"), translate("sel_cfailure"), translate("sel_ifailure"))),
                                                    #"Inter failure","Cumulative Failure", "Failure Intensity"
                                                    #
                                                    selected = "CF")
                                    ),
                                    column(11, 
                                        radioButtons("DataPlotType", label = h6(translate("str_draw")),
                                                        choices = setNames(list("points_and_lines", "points", "lines"), c(translate("r_both"), translate("r_points"), translate("r_lines"))),
                                                        #list("Both" = "points_and_lines", "Points" = "points", "Lines" = "lines"),
                                                        inline=TRUE,
                                                        selected = "points_and_lines")
                                    ),
                                    column(10,
                                        radioButtons("PlotDataOrTrend", label = h6(translate("str_trend")),
                                                        choices = setNames(list(1, 2), c(translate("r_data"), translate("r_trendtest"))),
                                                        #list("Data" = 1, "Trend test" = 2), 
                                                        inline = TRUE,
                                                        selected = 1)
                                    ),
                                    column(11, 
                                        selectInput("trendPlotChoice", label = translate("str_growth"), 
                                                    choices = setNames(list("LP", "RA"), c(translate("sel_laplace"), translate("sel_raa"))))
                                                    #list("Laplace Test" = "LP", "Running Arithmetic Average" = "RA"))
                                    ),
                                    column(11,
                                        conditionalPanel(
                                            condition = "input.trendPlotChoice == 'LP'",
                                            numericInput("confidenceLP", 
                                                        label = translate("str_conflevel"),
                                                        min = 0, max=1, value = 0.9, step=0.01)
                                        )
                                    ),
                                    column(8, textOutput("trendMessage")),
                                
                                
                                
                                    column(12, 
                                        radioButtons("saveDataFileType", label = h6(translate("str_filetype")),
                                                        choices = list("JPEG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                                                        selected = "JPG")
                                    ),
                                    column(8, downloadButton('saveDataOrTrend', translate("b_savedisplay"))),
                                
                                
                                    column(8,
                                        uiOutput("message")
                                    ),
                                
                                
                                
                                    br(),
                                    column(9, h5(translate("str_subset"))),
                                    #     column(9,
                                    #            sliderInput("sliderDataSubsetChoice", h6("Select one or more failure categories to retain"),
                                    #                        min = 1, max = 5, value = c(1, 5), step = 1)),
                                    column(9,
                                        sliderInput("modelDataRange", h6(translate("str_specify")),
                                                    min = 1, max = 5, value = c(1, 5), step = 1))
                                )
                              ),
                              mainPanel(
                                tabsetPanel(
                                    tabPanel(translate("str_plottab"), textOutput("InputFileError"), textOutput("DataSubsetError"), plotOutput("DataAndTrendPlot",width="100%",height="50%",dblclick="DTPdblclick", brush=brushOpts(id="DTP_brush",resetOnNew=TRUE))),
                                    tabPanel(translate("str_trendtab"), DT::dataTableOutput("dataAndTrendTable")),
                                    id="DataPlotAndTableTabset"
                                    
                                )
                                
                              )
                            )
                   ),
                   
                   tabPanel(translate("str_tab2"),
                            
                            sidebarLayout(
                              sidebarPanel(
                               fluidRow(
                                    
                                    h4(translate("str_configure")),
                                    h5(translate("str_specfail")),
                                    
                                    column(12,
                                        numericInput("modelNumPredSteps", 
                                                        label = h6(translate("str_howmany")),
                                                        min = 1, value = 1)
                                    ),
                                    
                                    column(12, 
                                        selectInput(
                                            "modelsToRun", label = h6(translate("str_choosemodels")), 
                                            choices=setNames(list("None"), c(translate("str_opendata"))),
                                            multiple=TRUE, selected="None"
                                        )
                                    ),
                                    
                                    column(12,
                                        actionButton("runModels", label = translate("x_runmodels"))
                                    ),
                                    
                                    #Heading is to close to button, needs formatting
                                    
                                    h4(translate("str_display")),
                                    
                                    column(12, 
                                    
                                        selectInput(
                                            "modelResultChoice", label = h6(translate("str_chooseresult")), 
                                            choices=setNames(list("None"), c(translate("str_nomodels"))),
                                            multiple=TRUE, selected="None"
                                        )
                                    
                                    ),
                                    
                                    column(12, 
                                        h5(translate("str_chooseplot")),
                                        selectInput("modelPlotChoice", label = h6(translate("str_plottype")), 
                                                    choices = setNames(list("MTTF", "MVF", "FI", "R_growth"), c(translate("sel_tfailure"), translate("sel_cfailure"), translate("sel_ifailure"), translate("sel_relgrowth"))),selected = "MVF")
                                                    #choices = list("Times Between Failures" = "MTTF", "Cumulative Failures" = "MVF",
                                                    #                "Failure Intensity" = "FI", "Reliability Growth"="R_growth"), 
                                    ),
                                    column(12,
                                        conditionalPanel(
                                            condition = "input.modelPlotChoice == 'R_growth'",
                                            numericInput("modelRelMissionTime", 
                                                        label = h6(translate("str_interval")),
                                                        min = 0, value = 1)
                                        )
                                    ),
                                    column(12,
                                        numericInput("modelCurveAdditionalTime",
                                                        label=h6(translate("str_duration")),
                                                        min=0, value=100, step=1000)
                                    ),
                                    
                                    column(12, 
                                        checkboxInput("checkboxDataOnPlot", label = translate("chk_showdata"), value = TRUE)
                                    ),
                                    column(12, 
                                        checkboxInput("checkboxDataEndOnPlot", label = translate("chk_showend"), value = TRUE)
                                    ),
                                    
                                    column(12, 
                                        radioButtons("ModelDataPlotType", label = h6(translate("str_draw")),
                                                        choices = setNames(list("points_and_lines", "points", "lines"), c(translate("r_both"), translate("r_points"), translate("r_lines"))),
                                                        inline=TRUE,
                                                        selected = "points_and_lines")
                                    ),
                                    
                                    column(12, 
                                        radioButtons("saveModelResultsType", label = h6(translate("str_filetype")),
                                                        choices = list("JPEG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                                                        selected = "JPG")
                                    ),
                                    column(8, downloadButton(outputId = "saveModelResults", label = translate("b_savedisplay")))
                                )
                               
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel(translate("str_rplottab"), textOutput("ModelConfigError"), textOutput("UnsuccessfulModels"), plotOutput("ModelPlot", dblclick="MPdblclick", brush=brushOpts(id="MP_brush", resetOnNew=TRUE))), 
                                  tabPanel(translate("str_rtabletab"),
                                           selectInput(
                                             "AllModelsRun", label = h6(translate("str_chooseresult")), 
                                             choices=list("No model results to display"="None"),
                                             multiple=TRUE, selected="None"),
                                           DT::dataTableOutput("ModelResultTable")),
                                  id="ModelPlotAndTableTabset"), width=8
                              )
                            )
                   ),
                   
                   tabPanel(translate("str_tab3"),
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                        h4(translate("str_predict")),
                                        column(12,
                                            selectInput(
                                                "modelDetailChoice", label = h6(translate("str_chooseresult")), 
                                                choices=list("No model results to display"="None"),
                                                multiple=TRUE, selected="None"
                                            )
     
                                        ),
                                        
                                        column(12, 
                                            h5(translate("str_time"))
                                        ),
                                        
                                        column(12,
                                            numericInput("modelDetailPredFailures", 
                                                            label = h6(translate("str_numfail")),
                                                            min = 1, value = 1)
                                        ),
                                        
                                        column(12, 
                                            h5(translate("str_obsfail"))
                                        ),
                                        
                                        column(12,
                                            numericInput("modelDetailPredTime", 
                                                            label = h6(translate("str_addtime")),
                                                            min = 1, value = 1)
                                        ),
                                        
                                        column(12, 
                                            h5(translate("str_testtime"))
                                        ),
                                        
                                        column(12,
                                            numericInput("modelTargetReliability", 
                                                            label = h6(translate("str_reliability")),
                                                            min = 0, max = 1, value = 0.9, step = 0.01)
                                        ),
                                        
                                        column(12,
                                            numericInput("modelRelMissionTime2", 
                                                            label = h6(translate("str_interval")),
                                                            min = 0, value = 1)
                                        ),
                                        column(12,
                                            h5(translate("str_optimal"))
                                            
                                        ),
                                        column(12,
                                            numericInput("T", 
                                                            label = h6(translate("str_lifecycle")),
                                                            min = 0, value = 100000)
                                        ),
                                        
                                        column(12,
                                            numericInput("C0", 
                                                            label = h6(translate("str_cost")),
                                                            min = 0, value = 50)
                                        ),
                                        column(12,
                                            numericInput("C1", 
                                                            label = h6(translate("str_costop")),
                                                            min = 0, value = 1000)
                                        ),
                                        column(12,
                                            numericInput("C2", 
                                                            label = h6(translate("str_softtest")),
                                                            min = 0, value = 1)
                                        ),
                                        column(12, 
                                            radioButtons("queryResultsPlotType", label = h6(translate("str_draw")),
                                                            choices = setNames(list("points_and_lines", "points", "lines"), c(translate("r_both"), translate("r_points"), translate("r_lines"))),
                                                            inline=TRUE,
                                                            selected = "points_and_lines"),
                                            radioButtons("saveQueryResultsType", label = h6(translate("str_filetype")),
                                                            choices = list("JPG" = "JPG", "PDF" = "PDF", "PNG" = "PNG", "TIFF" = "TIFF"), inline = TRUE,
                                                            selected = "JPG")
                                        ),
                                        
                                        column(8, downloadButton(outputId = "saveQueryResults", label = translate("b_savedisplay"))),
                                        
                                        column(12, 
                                            radioButtons("saveModelDetailsType", label = translate("str_eval"),
                                                            choices = list("CSV" = "CSV", "PDF" = "PDF"), inline = TRUE,
                                                            selected = "CSV"),
                                            downloadButton('downloadData', translate("str_savepred"))
                                        )
                                    )
                                
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel(translate("x_predicttab"),
                                    DT::dataTableOutput('mytable1')
                                    ),
                                  tabPanel(translate("str_resultplottab"), 
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
                   
                   tabPanel(translate("str_tab4"),
                            sidebarLayout(
                                sidebarPanel(
                                  fluidRow(
                                        h4(translate("str_fitness")),
                                        column(12, 
                                                br(),
                                                h5(translate("str_chooseeval")),
                                                selectInput(
                                                "modelResultsForEval", label = h6(translate("str_sets")), 
                                                choices=list("No model results to display"="None"),
                                                multiple=TRUE, selected="None"
                                                )
                                        ),
                                        
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
                                                            label = h6(translate("str_percent")),
                                                            min = 0.1, max = 1.0, step = 0.001,
                                                            value = .90)
                                        ),
                                        
                                        column(12, 
                                                radioButtons("saveModelEvalType", label = translate("str_eval"),
                                                            choices = list("CSV" = "CSV", "PDF" = "PDF"), inline = TRUE,
                                                            selected = "CSV"),
                                                downloadButton('saveModelEvals', translate("x_saveeval"))
                                        )
                                        
                                        )
                                  
                                  ),
                              

                              
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

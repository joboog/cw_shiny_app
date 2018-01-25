navbarPage("LRB Research Data",
           
 # WQ Data ==========================================
 tabPanel("WQ",
          sidebarLayout(
            sidebarPanel("",
                         dateRangeInput(inputId = "WQ_dateInput", label = "Date",
                                        start = substr((as.character(min(lRaw.dat$DateTime, na.rm = TRUE))),1,10),
                                        end = substr((as.character(max(lRaw.dat$DateTime, na.rm = TRUE))),1,10),
                                        #end = "2017-12-31",
                                        min = substr((as.character(min(lRaw.dat$DateTime, na.rm = TRUE))),1,10),
                                        max = substr((as.character(max(lRaw.dat$DateTime, na.rm = TRUE))),1,10),,
                                        format = "yyyy-mm-dd",
                                        startview = "month",
                                        weekstart = 1),
                         
                         
                         uiOutput("WQ_systemOutput"),
                         
                         actionButton(inputId = "WQ_goInput", label = "Update"),
                         
                         uiOutput("WQ_samplepointOutput"),
                         radioButtons(inputId = "WQ_facetInput",
                                      label = "Multiple Plots.",
                                      choices = c("no multiple Plots","System", "Sampling Point"),
                                      selected = "no multiple Plots"),
                         uiOutput("WQ_parameterOutput"),
                         radioButtons(inputId = "plotextInput",
                                      label = "Plot Ext.",
                                      choices = c("pdf", "png"),
                                      selected = "pdf")
                         
            ),
            
            mainPanel("",
                      plotOutput(outputId = "WQ_tsPlot"),
                      downloadButton(outputId = "WQ_downtsPlot",
                                     label = "Download the plot"),
                      downloadButton(outputId = "WQ_downsummarytable",
                                     label = "Download the summary table"),
                      downloadButton(outputId = "WQ_downdata",
                                     label = "Download the data"),
                      tableOutput(outputId = "WQ_summarytable"),
                      tableOutput(outputId = "WQ_table")
            )
            
          )
 ),
 
 # Flow data ========================================
 tabPanel("Flow",
          sidebarLayout(
            sidebarPanel("",
                         dateRangeInput(inputId = "FLOW_dateInput", label = "Date",
                                        start = substr((as.character(min(lflow_daily.df$RDate, na.rm = TRUE))),1,10),
                                        end = substr((as.character(max(lflow_daily.df$RDate, na.rm = TRUE))),1,10),
                                        min = substr((as.character(min(lflow_daily.df$RDate, na.rm = TRUE))),1,10),
                                        max = substr((as.character(max(lflow_daily.df$RDate, na.rm = TRUE))),1,10),
                                        format = "yyyy-mm-dd",
                                        startview = "month",
                                        weekstart = 1),
                         
                         radioButtons(inputId = "FLOW_scaleInput",
                                      label = "Data Source",
                                      choices = c("Hourly","Daily Summary", "Monthly Summary"),
                                      selected = "Daily Summary"),
                         
                         checkboxGroupInput(inputId = "FLOW_systemInput", label = "Systems",
                                            choiceNames =  as.list(as.character(sort(unique(lflow_daily.df$System)))),
                                            choiceValues = as.list(as.character(sort(unique(lflow_daily.df$System))))),
                         
                         checkboxGroupInput(inputId = "FLOW_positionInput", label = "Position",
                                            choiceNames =  as.list(as.character(sort(unique(lflow_daily.df$Position)))),
                                            choiceValues = as.list(as.character(sort(unique(lflow_daily.df$Position))))),
                         
                         radioButtons(inputId = "FLOW_facetInput",
                                      label = "Multiple Plots.",
                                      choices = c("no multiple Plots","System", "In/Out"),
                                      selected = "no multiple Plots"),
                         
                         actionButton(inputId = "FLOW_goInput", label = "Update"),
                         
                         radioButtons(inputId = "plotextInput",
                                      label = "Plot Ext.",
                                      choices = c("pdf", "png"),
                                      selected = "pdf")
                         
                         
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput(outputId = "FLOW_Plot"),
              
              downloadButton(outputId = "FLOW_downPlot",
                             label = "Download the plot"),
              downloadButton(outputId = "FLOW_downdata",
                             label = "Download the data"),
              downloadButton(outputId = "FLOW_downsummarytable",
                             label = "Download the summary table"),
              
              tableOutput(outputId = "FLOW_summarytable")
            )
            
          )
 ),
 
 # Weather data =====================================
 tabPanel("Weather",
          sidebarLayout(
            sidebarPanel("",
                         dateRangeInput(inputId = "WEATHER_dateInput", label = "Date",
                                        start = substr((as.character(min(lweather_daily.df$Date, na.rm = TRUE))),1,10),
                                        end = substr((as.character(max(lweather_daily.df$Date, na.rm = TRUE))),1,10),
                                        min = substr((as.character(min(lweather_daily.df$Date, na.rm = TRUE))),1,10),
                                        max = substr((as.character(max(lweather_daily.df$Date, na.rm = TRUE))),1,10),
                                        format = "yyyy-mm-dd",
                                        startview = "month",
                                        weekstart = 1),
                         
                         radioButtons(inputId = "WEATHER_scaleInput",
                                      label = "Data Source",
                                      choices = c("10 min Raw Data","Daily Summary", "Monthly Summary"),
                                      selected = "Daily"),
                         
                         checkboxGroupInput(inputId = "WEATHER_parameterInput", label = "Parameters",
                                            choiceNames =  as.list(as.character(sort(unique(lweather_daily.df$Parameter)))),
                                            choiceValues = as.list(as.character(sort(unique(lweather_daily.df$Parameter))))),
                         
                         actionButton(inputId = "WEATHER_goInput", label = "Update"),
                         
                         radioButtons(inputId = "plotextInput",
                                      label = "Plot Ext.",
                                      choices = c("pdf", "png"),
                                      selected = "pdf")
                         
                         
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput(outputId = "WEATHER_Plot"),
              
              downloadButton(outputId = "WEATHER_downPlot",
                             label = "Download the plot"),
              downloadButton(outputId = "WEATHER_downdata",
                             label = "Download the data"),
              downloadButton(outputId = "WEATHER_downsummarytable",
                             label = "Download the summary table"),
              
              tableOutput(outputId = "WEATHER_summarytable")
            )
          )         
  )
)

function(input, output){
  
# WQ Data ===========================================================================================
  
  # output for System
  output$WQ_systemOutput <- renderUI({
    checkboxGroupInput(inputId = "WQ_systemInput", label = "System",
                       choices = (unique(lRaw.dat$System)),
                       selected = NULL)
  })
  
  # dynamic checkboxGrouInput output for SamplePoint
  output$WQ_samplepointOutput <- renderUI({
    checkboxGroupInput(inputId = "WQ_samplepointInput", label = "Sample Point",
                       choices = sort(unique(df_WQ()$SamplePoint)),
                       selected = NULL)  #sort(unique(df()$SamplePoint))[1])
  })
  
  # dynamic checkboxGrouInput for wq parameter
  output$WQ_parameterOutput <- renderUI({
    checkboxGroupInput(inputId = "WQ_parameterInput", label = "WQ Parameters",
                       choiceNames =  as.list(as.character(sort(unique(df1_WQ()$Parameter)))),
                       choiceValues = as.list(as.character(sort(unique(df1_WQ()$Parameter)))))
  })
  
  
  # filter lRaw.dat data
  df_WQ <- eventReactive(input$WQ_goInput, {
    
    if (is.null(input$WQ_systemInput)) {
      return(NULL)
    }
    
    lRaw.dat %>%
      filter(
        DateTime >= strptime(input$WQ_dateInput[1], format="%Y-%m-%d"),
        DateTime <= strptime(input$WQ_dateInput[2], format="%Y-%m-%d"),
        System %in% input$WQ_systemInput
      )
  })
  
  # now filter data for sample point
  df1_WQ <- reactive({
    if (is.null(input$WQ_samplepointInput) | is.null(df_WQ()) ) {
      return(NULL)
    }
    df_WQ() %>%
      filter(
        SamplePoint %in% input$WQ_samplepointInput
      )
  })
  
  # now filter data for wq parameter
  df2_WQ <- reactive({
    if (is.null(input$WQ_parameterInput) ) {
      return(NULL)
    }
    df1_WQ() %>%
      filter(
        Parameter %in% input$WQ_parameterInput
      )
  })
  
  # plot ===============================================
  output$WQ_tsPlot <- renderPlot({
    
    if (is.null(input$WQ_samplepointInput) | is.null(input$WQ_systemInput) | is.null(input$WQ_parameterInput)) {
      return(NULL)
    }
    
    # create faceting if wanted
    # dunno why case_when() is not working
    if (input$WQ_facetInput == "System") {
      facet_var <- "Parameter~System" 
      # colorInp <- "System"
    }
    if (input$WQ_facetInput == "Sampling Point") {
      facet_var <- "Parameter~SamplePoint"
      #colorInp <- "SamplePoint"
    }
    if (input$WQ_facetInput == "no multiple Plots") {
      facet_var <- "Parameter~."
      #colorInp <- ""
    }
    
    #plot
    ts_plot_wq <- ggplot(data = na.omit(df2_WQ()), aes(x=DateTime, y=value, color=SamplePoint)) +
      geom_line()+geom_point() + facet_grid(facet_var, scales = "free_y") + theme_bw() + labs(x="")
    
    #plot
    ts_plot_wq
  })
  
  
  # create an output for downloading the plot
  output$WQ_downtsPlot <- downloadHandler(
    filename = function(){
      paste("WQ_tsPlot", input$plotextInput, sep = ".")
    },
    content = function(file){
      if(input$plotextInput == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(last_plot())
      dev.off() # turn the device off
    }
  )
  
  
  # output of summarytable =====================================
  df2_summary_WQ <- reactive({
    
    if (is.null(df2_WQ())) {
      return(NULL)
    }
    
    df3 <- df2_WQ() %>% spread(Parameter,value)
    
    # summary_tabl2(Dataframe, columns range, Factorname, col number of factor to summarize)
    summary_table2(df3, (which(colnames(df3)=="SamplePoint")+1):length(colnames(df3)),
                   "SamplePoint", which(colnames(df3)=="SamplePoint"))
  })
  
  output$WQ_summarytable <- renderTable(df2_summary_WQ())
  
  # output for summary table download
  output$WQ_downsummarytable<- downloadHandler(
    filename = function(){
      paste("WQ_SummaryTable", "csv", sep = ".")
    },
    content = function(file){
      write.table(df2_summary_WQ(), file, dec = ".", sep = ";", row.names = FALSE, na = "NA", quote = FALSE)
    }
  ) 
  
  # output for data download ====
  output$WQ_downdata <- downloadHandler(
    filename = function(){
      paste("WQ_Data", "csv", sep = ".")
    },
    content = function(file){
      write.table(df2_WQ(), file, dec = ".", sep = ";", row.names = FALSE, na = "NA", quote = FALSE)
    }
  ) 
  
  
  
# FLOW data ==============================================================================
  
  # filter 
  df_flow <- eventReactive(input$FLOW_goInput, {
    
    if (is.null(input$FLOW_scaleInput) | is.null(input$FLOW_systemInput) | is.null(input$FLOW_sampletypeInput)) {
      return(NULL)
    }
    
    #now choose source data due to input$scaleInput
    
    
    if (input$FLOW_scaleInput == "Hourly") {
      lflow_hourly.df %>%
        filter(
          RDate >= strptime(input$FLOW_dateInput[1], format="%Y-%m-%d"),
          RDate <= strptime(input$FLOW_dateInput[2], format="%Y-%m-%d"),
          System %in% input$FLOW_systemInput,
          SampleType %in% input$FLOW_sampletypeInput
        )
    }
    else if (input$FLOW_scaleInput == "Daily Summary") {
      lflow_daily.df %>%
        filter(
          RDate >= strptime(input$FLOW_dateInput[1], format="%Y-%m-%d"),
          RDate <= strptime(input$FLOW_dateInput[2], format="%Y-%m-%d"),
          System %in% input$FLOW_systemInput,
          SampleType %in% input$FLOW_sampletypeInput
        )
    }
    else {
      
      lflow_monthly.df %>%
        filter(
          RDate >= strptime(input$FLOW_dateInput[1], format="%Y-%m-%d"),
          RDate <= strptime(input$FLOW_dateInput[2], format="%Y-%m-%d"),
          System %in% input$FLOW_systemInput,
          SampleType %in% input$FLOW_sampletypeInput
        )
      
    }
    
  })
  
  # plot ================================================
    
  output$FLOW_Plot <- renderPlot({
    
    if (is.null(input$FLOW_scaleInput) | is.null(input$FLOW_systemInput) | is.null(input$FLOW_sampletypeInput) | is.null(df_flow())) {
      return(NULL)
    }
    
    # create faceting if wanted
    # dunno why case_when() is not working
    if (input$FLOW_facetInput == "System") {
      facet_var_flow <- "System~." 
      # colorInp <- "System"
      
      ts_plot_flow <- ggplot(data = na.omit(df_flow()), aes(x=RDate, y=value, color=SampleType)) +
        geom_line()+geom_point() + facet_grid(facet_var_flow, scales = "free_y") + theme_bw() + labs(x="")
    }
    
    if (input$FLOW_facetInput == "In/Out") {
      facet_var_flow <- ".~SampleType"
      #colorInp <- "SamplePoint"
      
      ts_plot_flow <- ggplot(data = na.omit(df_flow()), aes(x=RDate, y=value, color=System)) +
        geom_line()+geom_point() + facet_grid(facet_var_flow, scales = "free_y") + theme_bw() + labs(x="")
    }
    
    if (input$FLOW_facetInput == "no multiple Plots") {
      facet_var_flow <- ".~."
      #colorInp <- ""
      ts_plot_flow <- ggplot(data = na.omit(df_flow()), aes(x=RDate, y=value, color=SampleType, shape=System)) +
        geom_line()+geom_point()+ theme_bw() + labs(x="")#+ labs(title="")
    }
    
    ts_plot_flow
  })
  
  # create an output for downloading the plot
  output$FLOW_downPlot <- downloadHandler(
    filename = function(){
      paste("Flow_tsPlot", input$plotextInput, sep = ".")
    },
    content = function(file){
      if(input$plotextInput == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(last_plot())
      dev.off() # turn the device off
    }
  )
  
  
  # summary table =====================================
  df_summary_flow <- eventReactive(input$FLOW_goInput, {
    
    if (is.null(df_flow())) {
      return(NULL)
    }
    
    df2 <- select(df_flow(), c(RDate, System, SampleType, value)) %>% spread(SampleType,value)
    
    # summary_tabl2(Dataframe, columns range, Factorname, col number of factor to summarize)
    summary_table2(df2, (which(colnames(df2)=="System")+1):length(colnames(df2)),
                   "System", which(colnames(df2)=="System"))
  })
  
  # summary table output
  output$FLOW_summarytable <- renderTable(
    df_summary_flow()
  )
  
  # output for summary table download
  output$FLOW_downsummarytable <- downloadHandler(
    filename = function(){
      paste("Flow_SummaryTable", "csv", sep = ".")
    },
    content = function(file){
      write.table(df_summary_flow(), file, dec = ".", sep = ";", row.names = FALSE, na = "NA", quote = FALSE)
    }
  ) 
  
  # data ===============================================
  # output for data download
  output$FLOW_downdata <- downloadHandler(
    filename = function(){
      paste("Flow_Data", "csv", sep = ".")
    },
    content = function(file){
      write.table(df_flow(), file, dec = ".", sep = ";", row.names = FALSE, na = "NA", quote = FALSE)
    }
  )
  
  
# WEATHER data ================================================================================
  
  # filter lweather...
  df_weather <- eventReactive(input$WEATHER_goInput, {
    
    if (is.null(input$WEATHER_scaleInput) | is.null(input$WEATHER_parameterInput)) {
      return(NULL)
    }
    
    if (input$WEATHER_scaleInput == "Daily Summary") {
      lweather_daily.df %>%
        filter(
          Date >= strptime(input$WEATHER_dateInput[1], format="%Y-%m-%d"),
          Date <= strptime(input$WEATHER_dateInput[2], format="%Y-%m-%d"),
          Parameter %in% input$WEATHER_parameterInput
        )
    }
    else if (input$WEATHER_scaleInput == "Monthly Summary") {
      lweather_monthly.df %>%
        filter(
          Date >= strptime(input$WEATHER_dateInput[1], format="%Y-%m-%d"),
          Date <= strptime(input$WEATHER_dateInput[2], format="%Y-%m-%d"),
          Parameter %in% input$WEATHER_parameterInput
        )
    }
    else {
      
      # subset before
      df1 <- weather_raw.df %>%
        filter(
          RDate_Xlt >= strptime(input$WEATHER_dateInput[1], format="%Y-%m-%d"),
          RDate_Xlt <= strptime(input$WEATHER_dateInput[2], format="%Y-%m-%d")
        )
      
      # transfrom data to longformat
      lweather_raw.df <- melt(data=df1, id.vars=c("DateTime"), 
                              measure.vars=c(colnames(df1)[-c(1,14,15)]), 
                              variable.name="Parameter")
      colnames(lweather_raw.df)[1] <- "Date"
      lweather_raw.df$Date <- as.POSIXct(strptime(lweather_raw.df$Date, format = "%d. %m. %Y %R"))
      
      #subset
      lweather_raw.df %>%
        filter(
          #Date >= strptime(input$dateInput[1], format="%Y-%m-%d"),
          #Date <= strptime(input$dateInput[2], format="%Y-%m-%d"),
          Parameter %in% input$WEATHER_parameterInput
        )
      
    }
    
  })
  
  # plot ==============================
  output$WEATHER_Plot <- renderPlot({
    
    if (is.null(input$WEATHER_scaleInput) | is.null(input$WEATHER_parameterInput) | is.null(df_weather())) {
      return(NULL)
    }
    
    #plot
    ts_plot_weather <- ggplot(data = na.omit(df_weather()), aes(x=Date, y=value)) +
      geom_line()+geom_point() + facet_grid(Parameter~., scales = "free_y") + theme_bw() + labs(x="")
    
    #plot
    ts_plot_weather
  })
  
  # create an output for downloading the plot
  output$WEATHER_downPlot <- downloadHandler(
    filename = function(){
      paste("Weather_tsPlot", input$plotextInput, sep = ".")
    },
    content = function(file){
      if(input$plotextInput == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(last_plot())
      dev.off() # turn the device off
    }
  )
  
  
  # summary table ================================================
  df_summary_weather <- eventReactive(input$WEATHER_goInput, {
    
    if (is.null(df_weather())) {
      return(NULL)
    }
    
    df2 <- df_weather() %>% spread(Parameter,value)
    
    Parameter <- c("Mean", "Sd", "n")
    
    cbind(
      Parameter,
      rbind(
        format(
          (summarise_all(select(df2, -Date), funs(mean), na.rm=TRUE)),
          digits = 1, nsmall = 1
        ),
        format((summarise_all(select(df2, -Date), funs(sd), na.rm=TRUE)[1,]),
               digits=1, nsmall=1
        ),
        format((summarise_all(select(df2, -Date), funs(countValues))[1,]),
               digits=0, nsmall = 1
        )
      )
    )
    
  })
  
  # summary table output
  output$WEATHER_summarytable <- renderTable(
    df_summary_weather()
  )
  
  # output for summary table download
  output$WEATHER_downsummarytable <- downloadHandler(
    filename = function(){
      paste("Weather_SummaryTable", "csv", sep = ".")
    },
    content = function(file){
      write.table(df_summary_weather(), file, dec = ".", sep = ";", row.names = FALSE, na = "NA", quote = FALSE)
    }
  ) 
  
  # data ===========================================================
  # output for data download
  output$WEATHER_downdata <- downloadHandler(
    filename = function(){
      paste("Weather_Data", "csv", sep = ".")
    },
    content = function(file){
      write.table(df_weather(), file, dec = ".", sep = ";", row.names = FALSE, na = "NA", quote = FALSE)
    }
  )
  
}
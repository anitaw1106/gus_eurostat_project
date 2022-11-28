library(formatR)
library(shiny)
library(gsubfn)
library(knitr)
library(markdown)
library(rmarkdown)

source("download_data.R")
source("db_functions.R")
source("graph_functions.R")
Sys.setlocale("LC_ALL", "Polish")
# Sys.setenv(LANG = "POLISH_POLAND.UTF-8")
options(warn=-1)

server <-shinyServer(function(input, output) {
  # clean graphs folder at the beginning
  do.call(file.remove, list(list.files(file.path(getwd(), "graphs"), full.names = TRUE)))
  
  dataDir <- file.path(getwd(), "data")
  dbName <- "deads.db"

  urlVar <- reactiveValues(
    gusurlText = NULL,
    eurostaturlText = NULL
  )

  sqlVar <- reactiveValues(
    sqlText = NULL,
    sqlResult = NULL,
    csvFileName = NULL
  )
  
  reportVars <- reactiveValues(
    EUGUSPLOT = NULL,
    SQLTABLE = NULL,
    TSGUS = NULL,
    TSGUSW = NULL,
    TSEU = NULL,
    TSEUW = NULL
  )


  observeEvent(input$sqlQueryInput, {
    sqlVar$sqlText <- input$sqlQueryInput
  })

  output$plainSQLText <- renderPrint({
    return(cat(paste(sqlVar$sqlText, "\n")))
  })

  observeEvent(input$open_page_GUS, {
    browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
  })

  observeEvent(input$open_page_EUROSTAT, {
    browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
  })

  observeEvent(input$createTables, {
    createTables(dataDir, dbName)
  })

  observeEvent(input$sqlQueryInput, {
    sqlVar$sqlText <- input$sqlQueryInput
  })

  observeEvent(input$fileInPath, {
    file.copy(input$fileInPath$datapath, file.path(dataDir, input$fileInPath$name))
  })

  observeEvent(input$query, {
    sqlVar$sqlResult <- queryDB(dataDir, dbName, sqlVar$sqlText)

    reportVars$SQLTABLE<-DT::datatable(sqlVar$sqlResult,
                                       options = list(lengthMenu = seq(10, 100, 10)))
      
    output$tbTable <-DT::renderDataTable(reportVars$SQLTABLE)
    if(grepl("gus", tolower(sqlVar$sqlText), fixed = TRUE)){
      reportVars$EUGUSPLOT <- renderGraphIfThreeCols(sqlVar$sqlResult,"gus")
      output$plotEUGUS <-  renderPlotly(ggplotly(reportVars$EUGUSPLOT))
    }else{
      reportVars$EUGUSPLOT <- renderGraphIfThreeCols(sqlVar$sqlResult,"eu")
      output$plotEUGUS <-  renderPlotly(ggplotly(reportVars$EUGUSPLOT))
    }
    
    
  })

  observeEvent(input$csvFileName, {
    sqlVar$csvFileName <- paste(input$csvFileName, ".csv")
  })

  observeEvent(input$exportToCSV, {
    write.csv(sqlVar$sqlResult, file.path(dataDir, sqlVar$csvFileName))
  })

  # MAPA GUS TAB
  observeEvent(input$visualizeGUS, {
    list[GUS, GUSW] <- visualizeGUSMap(
      dataDir, dbName, input$dateFromGUS, input$dateToGUS, "GUS",
      input$grupaWiekGUS, input$plecGUS, input$dateFromGUSW, input$dateToGUSW
    )
    output$GUS <- renderGvis(GUS)
    reportVars$GUSMAP <- GUS
    output$GUSW <- renderGvis(GUSW)
  })


  # MAPA EU TAB
  observeEvent(input$visualizeEU, {
    list[EU, EUW] <- visualizeEUMap(
      dataDir, dbName, input$dateFromEU, input$dateToEU, "EUROSTAT",
      input$plecEU, input$dateFromEUW, input$dateToEUW
    )
    output$EU <- renderGvis(EU)
    output$EUW <- renderGvis(EUW)
  })

  # SZEREGI CZASOWE GUS TAB
  observeEvent(input$visualizeTSGUS, {
    list[TSGUS, TSGUSW] <- visualizeGUSTS(
      dataDir, dbName, input$dateFromTSGUS, input$dateToTSGUS, "GUS",
      input$grupaWiekTSGUS, input$plecTSGUS, input$wojewodztwoTSGUS,
      input$granulacjaTSGUS, input$dateFromTSGUSW, input$dateToTSGUSW
    )
    reportVars$TSGUS <- TSGUS
    reportVars$TSGUSW <- TSGUSW
    output$TSGUS <- renderPlotly(ggplotly(TSGUS))
    output$TSGUSW <- renderPlotly(ggplotly(TSGUSW))
  })

  # SZEREGI CZASOWE EU TAB
  observeEvent(input$visualizeTSEU, {
    list[TSEU, TSEUW] <- visualizeEUTS(
      dataDir, dbName, input$dateFromTSEU, input$dateToTSEU, "EUROSTAT",
      input$plecTSEU, input$panstwoTSEU, input$granulacjaTSEU,
      input$dateFromTSEUW, input$dateToTSEUW
    )
    reportVars$TSEU <- TSEU
    reportVars$TSEUW <- TSEUW
    output$TSEU <- renderPlotly(ggplotly(TSEU))
    output$TSEUW <- renderPlotly(ggplotly(TSEUW))
  })


  # RAPORT NA ZADANIE
  observeEvent(input$generateReport, {
    params <- list(sqlgraph=reportVars$EUGUSPLOT,
                   sqlquery=sqlVar$sqlText,
                   sqltable=reportVars$SQLTABLE,
                   tsgus=reportVars$TSGUS,
                   tsgusw=reportVars$TSGUSW,
                   tseu=reportVars$TSEU,
                   tseuw=reportVars$TSEUW
    )
    
    rmarkdown::render("template.Rmd",
                      output_file = file.path(getwd(), "report.html"),
                      envir = new.env(parent = globalenv()),
                      params = params
    )
    
  })
})

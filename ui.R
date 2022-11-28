library(shiny)
library(plotly)

ui <- shinyUI(fluidPage(fluidRow(
  column(2, titlePanel("EUROSTAT i GUS")),
  column(
    2,
    actionButton("generateReport", label = "Utworz raport na zadanie")
  )
),
mainPanel(
  width = 12,
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Pobieranie danych",
      HTML(paste0("<hr>Pobierz dane ze strony GUS:<hr>")),
      actionButton(inputId = "open_page_GUS", label = "Otworz polaczenie http"),
    
      HTML(paste0("<hr>Pobierz dane ze strony EUROSTAT:<hr>")),
      actionButton(inputId = "open_page_EUROSTAT", label = "Otworz polaczenie http"),
      fileInput("fileInPath", label = h4("Import danych")),
      actionButton("createTables", label = "Utworz tabele GUS i EUROSTAT")
    ),
    tabPanel(
      "SQL",
      textInput("sqlQueryInput",
                label = "Zapytanie SQL"),
      verbatimTextOutput("plainSQLText"),
      actionButton("query", label = "Wykonaj zapytanie"),
      textInput("csvFileName",
                label = "Nazwa pliku", value =
                  "test"),
      actionButton("exportToCSV", label = "Eksportuj do CSV"),
      mainPanel(tabsetPanel(tabPanel(
        "Data", DT::dataTableOutput("tbTable")
      )),
      plotlyOutput("plotEUGUS"))
    ),
    tabPanel(
      "Mapa GUS",
      fluidRow(
        column(
          3,
          dateInput("dateFromGUS", label = "Poczatek", value = "2020-01-01")
        ),
        column(
          3,
          dateInput("dateFromGUSW", label = "Poczatek wzgledny", value = "2015-01-01")
        ),
        column(3, selectInput(
          "plecGUS",
          "Wybierz plec: ",
          list("Ogolem", "Kobiety", "Mezczyzni")
        )),
        column(3, actionButton("visualizeGUS",
                               label =
                                 "Dokonaj wizualizji")),
      ),
      fluidRow(
        column(3, dateInput(
          "dateToGUS",
          label = "Koniec", value = "2021-12-01"
        )),
        column(
          3,
          dateInput("dateToGUSW", label = "Koniec wzgledny", value = "2019-12-31")
        ),
        column(3, selectInput(
          "grupaWiekGUS",
          "Wybierz grupe wiekowa:",
          list(
            "0 - Inf",
            "00 - 04",
            "05 - 09",
            "10 - 14",
            "15 - 19",
            "20 - 24",
            "25 - 29",
            "30 - 34",
            "35 - 39",
            "40 - 44",
            "45 - 49",
            "50 - 54",
            "55 - 59",
            "60 - 64",
            "65 - 69",
            "70 - 74",
            "75 - 79",
            "80 - 84",
            "85 - 89",
            "90 - Inf"
          )
        ))
      ),
      mainPanel(htmlOutput("GUS"),
                htmlOutput("GUSW"))
    ),
    tabPanel(
      "Mapa EU",
      fluidRow(
        column(
          3,
          dateInput("dateFromEU", label = "Poczatek", value = "2020-01-01")
        ),
        column(3, dateInput(
          "dateToEU",
          label = "Koniec", value = "2021-12-01"
        )),
        column(3, selectInput(
          "plecEU", "Wybierz plec: ", list("Total", "Females", "Males")
        ))
      ),
      fluidRow(
        column(
          3,
          dateInput("dateFromEUW", label = "Poczatek wzgledny", value = "2015-01-01")
        ),
        column(
          3,
          dateInput("dateToEUW", label = "Koniec wzgledny", value = "2019-12-31")
        ),
        column(3, actionButton("visualizeEU",
                               label =
                                 "Dokonaj wizualizji"))
      ),
      mainPanel(htmlOutput("EU"),
                htmlOutput("EUW"))
    ),
    tabPanel(
      "Szeregi czasowe GUS",
      fluidRow(
        column(
          3,
          dateInput("dateFromTSGUS", label = "Poczatek", value = "2020-01-01")
        ),
        column(
          3,
          dateInput("dateFromTSGUSW", label = "Poczatek wzgledny", value = "2015-01-01")
        ),
        column(
          3,
          selectInput(
            "grupaWiekTSGUS",
            "Wybierz grupe wiekowa:",
            list(
              "0 - Inf",
              "00 - 04",
              "05 - 09",
              "10 - 14",
              "15 - 19",
              "20 - 24",
              "25 - 29",
              "30 - 34",
              "35 - 39",
              "40 - 44",
              "45 - 49",
              "50 - 54",
              "55 - 59",
              "60 - 64",
              "65 - 69",
              "70 - 74",
              "75 - 79",
              "80 - 84",
              "85 - 89",
              "90 - Inf"
            )
          )
        ),
        column(
          3,
          selectInput(
            "granulacjaTSGUS",
            "Wybierz jednostke granulacji",
            list("tydzien", "miesiac", "rok")
          )
        )
      ),
      fluidRow(
        column(
          3,
          dateInput("dateToTSGUS", label = "Koniec", value = "2021-12-01")
        ),
        column(
          3,
          dateInput("dateToTSGUSW", label = "Koniec wzgledny", value = "2019-12-31")
        ),
        column(3, selectInput(
          "plecTSGUS",
          "Wybierz plec: ",
          list("Ogolem", "Kobiety", "Mezczyzni")
        )),
        column(
          3,
          selectInput(
            "wojewodztwoTSGUS",
            "Wybierz wojewodztwo",
            list(
              "Polska",
              "Makroregion Województwo Mazowieckie",
              "Małopolskie",
              "Śląskie",
              "Wielkopolskie",
              "Zachodniopomorskie",
              "Lubuskie",
              "Dolnośląskie",
              "Opolskie",
              "Kujawsko-Pomorskie",
              "Warmińsko-Mazurskie",
              "Pomorskie",
              "ŁÓdzkie",
              "Świętokrzyskie",
              "Lubelskie",
              "Podkarpackie",
              "Podlaskie"
            )
          )
        ),
        column(3, actionButton("visualizeTSGUS",
                               label =
                                 "Dokonaj wizualizji")),
        mainPanel(plotlyOutput("TSGUS"),
                  plotlyOutput("TSGUSW"))
      )
    ),
    tabPanel(
      "Szeregi czasowe EU",
      fluidRow(
        column(
          3,
          dateInput("dateFromTSEU", label = "Poczatek", value = "2020-01-01")
        ),
        column(
          3,
          dateInput("dateFromTSEUW", label = "Poczatek wzgledny", value = "2015-01-01")
        ),
        column(3, selectInput(
          "plecTSEU", "Wybierz plec: ", list("Total", "Females", "Males")
        )),
        column(3, selectInput(
          "panstwoTSEU",
          "Wybierz panstwo: ",
          list(
            "Belgium",
            "Bulgaria",
            "Czechia",
            "Denmark",
            "Germany (until 1990 former territory of the FRG)",
            "Estonia",
            "Ireland",
            "Greece",
            "Spain",
            "France",
            "Croatia",
            "Italy",
            "Cyprus",
            "Latvia",
            "Lithuania",
            "Luxembourg",
            "Hungary",
            "Malta",
            "Netherlands",
            "Austria",
            "Poland",
            "Portugal",
            "Romania",
            "Slovenia",
            "Slovakia",
            "Finland",
            "Sweden",
            "Iceland",
            "Liechtenstein",
            "Norway",
            "Switzerland",
            "United Kingdom",
            "Montenegro",
            "Albania",
            "Serbia",
            "Andorra",
            "Armenia",
            "Georgia"
          )
        )),
      ),
      fluidRow(
        column(
          3,
          dateInput("dateToTSEU", label = "Koniec", value = "2021-12-01")
        ),
        column(
          3,
          dateInput("dateToTSEUW", label = "Koniec wzgledny", value = "2019-12-31")
        ),
        column(
          3,
          selectInput(
            "granulacjaTSEU",
            "Wybierz jednostke granulacji",
            list("tydzien", "miesiac", "rok")
          )
        ),
        column(3, actionButton("visualizeTSEU",
                               label =
                                 "Dokonaj wizualizji"))
      ),
      mainPanel(plotlyOutput("TSEU"),
                plotlyOutput("TSEUW"))
    ),
  ) # tabsetpanel
)))

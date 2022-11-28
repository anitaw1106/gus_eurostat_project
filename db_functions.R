library("readxl")
library("data.table")
library("reshape")
library(RSQLite)
library(ISOweek)

source("download_data.R")

createTables <- function(dataDir, databaseFileName) {
  dbName <- file.path(dataDir, databaseFileName)
  # stawianie danych do bazy danych
  # polaczenie z baza danych
  con <- dbConnect(
    dbDriver("SQLite"),
    dbname = dbName,
    extended_types = T,
  )
  downloadGUS(dataDir)
  downloadEUROSTAT(dataDir)
  
  try({
    
    # odczyt danych
    Sys.setlocale(category = "LC_ALL", locale = "Polish")
    gus <- read.table(file = file.path(dataDir, "GUS_dane_przetworzone_pelne.csv"), sep = ";", dec = ",", header = T)
    gus$Od <- as.Date(gus$Od, format = "%Y-%m-%d")
    gus$Do <- as.Date(gus$Do, format = "%Y-%m-%d")
    gus$Od <- as.character(gus$Od)
    gus$Do <- as.character(gus$Do)
    
    dbWriteTable(con, "GUS", gus,
      overwrite = TRUE, row.names = FALSE,
      field.types = c(Od = "Date", Do = "Date", Plec = "character", Grupa_wiekowa = "character", Region_id = "character", Region = "character", Liczba = "numeric")
    )

    eurostat <- read.table(file = file.path(dataDir, "demo_r_mwk_ts_1_Data.csv"), sep = ",", dec = ",", header = T, stringsAsFactors = F, encoding = "UTF-8")
    splitted <- strsplit(eurostat$TIME, "W")

    eurostat$Od <- paste(sapply(splitted, "[[", 1), "-W", sapply(splitted, "[[", 2), "-1", sep = "")
    eurostat$Do <- paste(sapply(splitted, "[[", 1), "-W", sapply(splitted, "[[", 2), "-7", sep = "")
    eurostat$Od <- ISOweek::ISOweek2date(eurostat$Od)
    eurostat$Do <- ISOweek::ISOweek2date(eurostat$Do)
    eurostat$Od <- as.Date(eurostat$Od, format = "%Y-%m-%d")
    eurostat$Do <- as.Date(eurostat$Do, format = "%Y-%m-%d")
    eurostat$Od <- as.character(eurostat$Od)
    eurostat$Do <- as.character(eurostat$Do)
    eurostat$Value <- gsub(":", 0, eurostat$Value)
    eurostat$Value <- as.numeric(gsub(",", "", eurostat$Value))
    eurostat <- eurostat[c("Od", "Do", "GEO", "SEX", "UNIT", "Value", "Flag.and.Footnotes")]
    dbWriteTable(con, "EUROSTAT", eurostat,
      overwrite = TRUE, row.names = FALSE,
      field.types = c(Od = "Date", Do = "Date", GEO = "character", SEX = "character", UNIT = "character", Flag.and.Footnotes = "character", Value = "numeric")
    )
  })
  # rozlaczenie polaczenia
  dbDisconnect(con)
}


queryDB <- function(dataDir, databaseFileName, query) {
  con <- dbConnect(
    dbDriver("SQLite"),
    dbname = file.path(dataDir, databaseFileName),
    extended_types = T,
  )
  ret <- data.frame()
  try({
    ret <- dbGetQuery(con, query)
  })
  dbDisconnect(con)
  return(ret)
}

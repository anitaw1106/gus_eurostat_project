library(plyr)
library(googleVis)
library(zoo)
library(rccdates)
library(plotly)
source("db_functions.R")

renderGraphIfThreeCols <- function(sqlResult,table){
  if(table=="eu"){
    if(ncol(sqlResult)==3){
      if("Value" %in% colnames(sqlResult)){
        if ("Od" %in% colnames(sqlResult)){
          if ("GEO" %in% colnames(sqlResult)){
            result <- ggplot(sqlResult,aes(x=Od,y=Value,col=GEO))+ geom_line()+theme_bw()+ggtitle("EUROSTAT") +xlab("Od") + ylab("Value")
          } else if("SEX" %in% colnames(sqlResult)){
            result <- ggplot(sqlResult,aes(x=Od,y=Value,col=SEX))+ geom_line()+theme_bw()+ggtitle("EUROSTAT") +xlab("Od") + ylab("Value")
          }
        }else if ("Do" %in% colnames(sqlResult)){
          if ("GEO" %in% colnames(sqlResult)){
            result <- ggplot(sqlResult,aes(x=Do,y=Value,col=GEO))+ geom_line()+theme_bw()+ggtitle("EUROSTAT") +xlab("Do") + ylab("Value")
          } else if("SEX" %in% colnames(sqlResult)){
            result <- ggplot(sqlResult,aes(x=Do,y=Value,col=SEX))+ geom_line()+theme_bw()+ggtitle("EUROSTAT") +xlab("Do") + ylab("Value")
          }
        }
      
    }
    return(result)
    }
  }
  if(table=="gus"){
    if(ncol(sqlResult)==3){
      if("Liczba" %in% colnames(sqlResult)){
        if ("Od" %in% colnames(sqlResult)){
          if ("Region" %in% colnames(sqlResult)){
            sqlResult_agg <- aggregate(sqlResult$Liczba, by=list(sqlResult$Od,sqlResult$Region), FUN=sum)
            colnames(sqlResult_agg) <-c("Od","Region","Liczba") 
            sqlResult_agg[is.na(sqlResult_agg)] <- 0
            result <- ggplot(sqlResult_agg,aes(x=Od,y=Liczba,col=Region)) + geom_line()+theme_bw()+ggtitle("GUS") +xlab("Od") + ylab("Liczba")
          } else if("Plec" %in% colnames(sqlResult)){
            sqlResult_agg <- aggregate(sqlResult$Liczba, by=list(sqlResult$Od,sqlResult$Plec), FUN=sum)
            colnames(sqlResult_agg) <-c("Od","Plec","Liczba") 
            sqlResult_agg[is.na(sqlResult_agg)] <- 0
            result <- ggplot(sqlResult_agg,aes(x=Od,y=Liczba,col=Plec))+ geom_line()+theme_bw()+ggtitle("GUS") +xlab("Od") + ylab("Liczba")
          }
        }else if ("Do" %in% colnames(sqlResult)){
          if ("Region" %in% colnames(sqlResult)){
            sqlResult_agg <- aggregate(sqlResult$Liczba, by=list(sqlResult$Do,sqlResult$Region), FUN=sum)
            colnames(sqlResult_agg) <-c("Do","Region","Liczba") 
            sqlResult_agg[is.na(sqlResult_agg)] <- 0
            result <- ggplot(sqlResult_agg,aes(x=Do,y=Liczba,col=Region)) + geom_line()+theme_bw()+ggtitle("GUS") +xlab("Do") + ylab("Liczba")
          } else if("Plec" %in% colnames(sqlResult)){
            sqlResult_agg <- aggregate(sqlResult$Liczba, by=list(sqlResult$Do,sqlResult$Plec), FUN=sum)
            colnames(sqlResult_agg) <-c("Do","Plec","Liczba") 
            sqlResult_agg[is.na(sqlResult_agg)] <- 0
            result <- ggplot(sqlResult_agg,aes(x=Do,y=Liczba,col=Plec))+ geom_line()+theme_bw()+ggtitle("GUS") +xlab("Do") + ylab("Liczba")
          }
        }
        
      }
      return(result)
    }
  }
}

queryDBwithDateRange <- function(dataDir,databaseFileName,dateFrom, dateTo, dbName){
  query = paste("select * from ",dbName," where Od>'",dateFrom,"' and Do<'",dateTo,"'",sep="")
  result <-  queryDB(dataDir,databaseFileName,query)
  return(result)
  
}


visualizeGUSMap <- function(dataDir,databaseFileName,dateFrom, dateTo, dbName,ageGroup,sex,dateFromW,dateToW){
  query = paste("select * from ",dbName," where Od>'",dateFrom,"' and Do<'",dateTo,"' and Grupa_Wiekowa='",ageGroup,"' and Plec='",sex,"'",sep="")
  result <-  queryDB(dataDir,databaseFileName,query)
  Sys.setlocale("LC_ALL", "Polish")
  list_of_voivodships <- c("Makroregion Województwo Mazowieckie","Małopolskie","Śląskie","Wielkopolskie","Zachodniopomorskie",
                           "Lubuskie","Dolnośląskie","Opolskie","Kujawsko-Pomorskie","Warmiśsko-Mazurskie","Pomorskie",
                           "Łódzkie","Świętokrzyskie","Lubelskie","Podkarpackie","Podlaskie" )
  
  list_of_voivodships_map <- c("mazowieckie", "malopolskie", "slaskie", "wielkopolskie", "zachodniopomorskie",
                               "lubuskie","dolnoslaskie","opolskie","kujawsko-pomorskie","warminsko-mazurskie",
                               "pomorskie","lodzkie","swietokrzyskie", "lubelskie","podkarpackie","podlaskie")
  
  result_filtered <- result %>%
    filter(Region  %in% list_of_voivodships)
  
  result_filtered$Region <- mapvalues(result_filtered$Region, 
                                   from=list_of_voivodships, 
                                   to=list_of_voivodships_map)
  
  result_filtered[is.na(result_filtered)] <- 0
  print(result_filtered)
  aggregated_result <- aggregate(result_filtered$Liczba, by=list(Region=result_filtered$Region), FUN=sum)
  colnames(aggregated_result) <- c("Region","Liczba")
  GeoStates <- gvisGeoChart(aggregated_result, "Region", "Liczba",
                            options=list(region="PL",
                                         displayMode="regions",
                                         resolution="provinces",
                                         colorAxis="{colors: ['#00853f','yellow', '#e31b23']}",
                                         width=1000, height=1000
                            ))

  queryW = paste("select * from ",dbName," where Od>'",dateFromW,"' and Do<'",dateToW,"' and Grupa_Wiekowa='",ageGroup,"' and Plec='",sex,"'",sep="")
  resultW <-  queryDB(dataDir,databaseFileName,queryW)
  
  result_filteredW <- resultW %>%
    filter(Region  %in% list_of_voivodships)
  
  result_filteredW$Region <- mapvalues(result_filteredW$Region, 
                                      from=list_of_voivodships, 
                                      to=list_of_voivodships_map)

  result_filteredW[is.na(result_filteredW)] <- 0
  
  aggregated_resultW <- aggregate(result_filteredW$Liczba, by=list(Region=result_filteredW$Region), FUN=mean)
  colnames(aggregated_resultW) <- c("Region","LiczbaW")
  
  joined_result <- join(aggregated_result, aggregated_resultW,
       type = "inner")
  joined_result$division <- joined_result$Liczba/joined_result$LiczbaW
  
  GeoStatesW <- gvisGeoChart(joined_result, "Region", "division",
                            options=list(region="PL",
                                         displayMode="regions",
                                         resolution="provinces",
                                         colorAxis="{colors: ['#00853f','yellow', '#e31b23']}",
                                         width=1000, height=1000
                            ))
  
  return(list(GeoStates,GeoStatesW))
}


visualizeEUMap <- function(dataDir,databaseFileName,dateFrom, dateTo, dbName,sex,dateFromW,dateToW){
  query = paste("select * from ",dbName," where Od>'",dateFrom,"' and Do<'",dateTo,"' and SEX='",sex,"'",sep="")
  result <-  queryDB(dataDir,databaseFileName,query)
  
  list_of_countries <- c("Germany (until 1990 former territory of the FRG)" )
  list_of_countries_map <- c("Germany")
  
  result$GEO <- mapvalues(result$GEO, 
                                      from=list_of_countries, 
                                      to=list_of_countries_map)
  result[is.na(result)] <- 0
  aggregated_result <- aggregate(result$Value, by=list(GEO=result$GEO), FUN=sum)
  colnames(aggregated_result) <- c("GEO","Value")
  GeoStates <- gvisGeoChart(aggregated_result, "GEO", "Value",
                            options=list(locationvar="GEO",
                                         colorvar="Value",
                                         colorAxis="{colors: ['#00853f','yellow', '#e31b23']}",
                                         region="150",
                                         width=1000, height=1000
                            ))
  
  queryW = paste("select * from ",dbName," where Od>'",dateFromW,"' and Do<'",dateToW,"' and SEX='",sex,"'",sep="")
  resultW <-  queryDB(dataDir,databaseFileName,queryW)
  resultW$GEO <- mapvalues(resultW$GEO, 
                          from=list_of_countries, 
                          to=list_of_countries_map)
  resultW[is.na(resultW)] <- 0
  aggregated_resultW <- aggregate(resultW$Value, by=list(GEO=resultW$GEO), FUN=mean)
  colnames(aggregated_resultW) <- c("GEO","ValueW")
  
  joined_result <- join(aggregated_result, aggregated_resultW,
                        type = "inner")
  joined_result$division <- joined_result$Value/joined_result$ValueW
  
  GeoStatesW <- gvisGeoChart(joined_result, "GEO", "division",
                            options=list(locationvar="GEO",
                                         colorvar="division",
                                         colorAxis="{colors: ['#00853f','yellow', '#e31b23']}",
                                         region="150",
                                         width=1000, height=1000
                            ))
  
  return(list(GeoStates,GeoStatesW))
}


visualizeGUSTS <- function(dataDir,databaseFileName,dateFrom, dateTo, dbName,ageGroup,sex,voivodship,granularity,dateFromW,dateToW){
  
  query = paste("select * from ",dbName," where Od>'",dateFrom,"' and Do<'",dateTo,"' and Grupa_Wiekowa='",
                ageGroup,"' and Plec='",sex,"' and Region='",voivodship,"'",sep="")
  result <-  queryDB(dataDir,databaseFileName,query)
  result[is.na(result)] <- 0
  
  queryW = paste("select * from ",dbName," where Od>'",dateFromW,"' and Do<'",dateToW,"' and Grupa_Wiekowa='",
                ageGroup,"' and Plec='",sex,"' and Region='",voivodship,"'",sep="")
  resultW <-  queryDB(dataDir,databaseFileName,queryW)
  resultW[is.na(resultW)] <- 0
  avgFromTotal <- mean(resultW$Liczba)
  
  fmt <- "%Y/%m/%d"
  DF <- transform(result, Od = as.Date(Od, fmt), Do = as.Date(Do, fmt))
  
  # convert to daily zoo series
  to.day <- function(i) with(DF, zoo(Liczba[i], seq(Od[i], Do[i], "day")))
  z.day <- do.call(c, lapply(1:nrow(DF), to.day))
  if (granularity=="tydzien"){
    plotly_result <- ggplot(DF,aes(x=Do,y=Liczba,group=1)) + geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE") +xlab("Tydzien") + ylab("Liczba")
    plotly_resultW <- ggplot(DF,aes(x=Do,y=Liczba/avgFromTotal,group=1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE - WZGLÃÂDNY") +xlab("Tydzien") + ylab("WspÄÅÄ¹âczynnik")
    
    return(list(plotly_result,plotly_resultW))
  }
  if (granularity=="miesiac"){
    monthly_df <- as.data.frame(aggregate(z.day, as.yearmon, sum))
    colnames(monthly_df) <- c("Date")
    plotly_result <- ggplot(monthly_df,aes(x=row.names(monthly_df),y=Date,group = 1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE") +xlab("Miesiac") + ylab("Liczba")
    plotly_resultW <- ggplot(monthly_df,aes(x=row.names(monthly_df),y=Date/avgFromTotal,group = 1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE - WZGLÃÂDNY") +xlab("Miesiac") + ylab("WspÄÅÄ¹âczynnik")

    return(list(plotly_result,plotly_resultW))
  }
  if (granularity=="rok"){
    yearly_df <- as.data.frame(aggregate(z.day, as.year, sum))
    colnames(yearly_df) <- c("Date")
    plotly_result <- ggplot(yearly_df,aes(x=row.names(yearly_df),y=Date,group = 1)) + geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE") +xlab("Rok") + ylab("Liczba")
    plotly_resultW <- ggplot(yearly_df,aes(x=row.names(yearly_df),y=Date/avgFromTotal,group = 1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE - WZGLÃÂDNY") +xlab("Rok") + ylab("WspÄÅÄ¹âczynnik")

    return(list(plotly_result,plotly_resultW))
  }
}

visualizeEUTS <- function(dataDir,databaseFileName,dateFrom, dateTo, dbName,sex,country,granularity,dateFromW,dateToW){
  
  query = paste("select * from ",dbName," where Od>'",dateFrom,"' and Do<'",dateTo,
                "' and SEX='",sex,"' and GEO='",country,"'",sep="")
  result <-  queryDB(dataDir,databaseFileName,query)
  result[is.na(result)] <- 0
  
  queryW = paste("select * from ",dbName," where Od>'",dateFromW,"' and Do<'",dateToW,
                "' and SEX='",sex,"' and GEO='",country,"'",sep="")
  resultW <-  queryDB(dataDir,databaseFileName,queryW)
  resultW[is.na(resultW)] <- 0
  avgFromTotal <- mean(resultW$Value)
  
  fmt <- "%Y/%m/%d"
  DF <- transform(result, Od = as.Date(Od, fmt), Do = as.Date(Do, fmt))
  
  # convert to daily zoo series
  to.day <- function(i) with(DF, zoo(Value[i], seq(Od[i], Do[i], "day")))
  z.day <- do.call(c, lapply(1:nrow(DF), to.day))
  if (granularity=="tydzien"){
    plotly_result <- ggplot(DF,aes(x=Do,y=Value,group=1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE") +xlab("Tydzien") + ylab("Liczba")
    plotly_resultW <- ggplot(DF,aes(x=Do,y=Value/avgFromTotal,group=1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE - WZGLÃÂDNY ") +xlab("Tydzien") + ylab("WspÄÅÄ¹âczynnik")

    return(list(plotly_result,plotly_resultW))
  }
  if (granularity=="miesiac"){
    monthly_df <- as.data.frame(aggregate(z.day, as.yearmon, sum))
    colnames(monthly_df) <- c("Date")
    plotly_result <- ggplot(monthly_df,aes(x=row.names(monthly_df),y=Date,group = 1)) + geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE") +xlab("Miesiac") + ylab("Liczba")
    plotly_resultW <- ggplot(monthly_df,aes(x=row.names(monthly_df),y=Date/avgFromTotal,group = 1)) + geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE - WZGLÃÂDNY ") +xlab("Miesiac") + ylab("WspÄÅÄ¹âczynnik")

    return(list(plotly_result,plotly_resultW))
  }
  if (granularity=="rok"){
    yearly_df <- as.data.frame(aggregate(z.day, as.year, sum))
    colnames(yearly_df) <- c("Date")
    plotly_result <- ggplot(yearly_df,aes(x=row.names(yearly_df),y=Date,group = 1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE") +xlab("Rok") + ylab("Liczba")
    plotly_resultW <- ggplot(yearly_df,aes(x=row.names(yearly_df),y=Date/avgFromTotal,group = 1))+ geom_line()+theme_bw()+ggtitle("EUROSTAT SZEREGI CZASOWE - WZGLÃÂDNY ") +xlab("Rok") + ylab("WspÄÅÄ¹âczynnik")

    return(list(plotly_result,plotly_resultW))
  }
}


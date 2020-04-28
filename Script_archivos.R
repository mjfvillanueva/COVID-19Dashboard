library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)
library(htmltools)
library(httr)
library(jsonlite)
library(leaflet)
library(tidyverse)
#library(tidyr)

library(sf)
library(plotly)
#library(dplyr)
library(lubridate)
library(spData)

#script para generar el csv con toda la info junta que se baja de internet
#setwd("C:/Users/MJFerreyra/Documents/COVID-19/Deploy")
#df_paises <- st_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson",stringsAsFactors =FALSE)
#df_paises$geometry2 <- st_centroid(st_geometry(df_paises))

df_temp <- st_read("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",stringsAsFactors =FALSE)
df_full <-read.csv("./df_full.csv", stringsAsFactors = FALSE)

if (length(unique(df_full$Date)) == (length(names(df_temp))-4))
{
  rm(df_temp)

}else
{
  rm(df_full)
  df_confirmed <-gather(df_temp, "Date", "confirmed", 5:ncol(df_temp))
  df_confirmed$Date <-gsub("X", "", df_confirmed$Date)
  df_confirmed$Date <-mdy(df_confirmed$Date)
  
  df_confirmed$confirmed <- as.integer(df_confirmed$confirmed)
  
  df_temp <- st_read("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",stringsAsFactors =FALSE)
  
  df_deaths <-gather(df_temp, "Date", "deaths", 5:ncol(df_temp))
  df_deaths$Date <-gsub("X", "", df_deaths$Date)
  df_deaths$Date <-mdy(df_deaths$Date)
  
  df_deaths$deaths <- as.integer(df_deaths$deaths)
  
  
  df_temp <- st_read("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",stringsAsFactors =FALSE)
  
  df_recovered <-gather(df_temp, "Date", "recovered", 5:ncol(df_temp))
  df_recovered$Date <-gsub("X", "", df_recovered$Date)
  df_recovered$Date <-mdy(df_recovered$Date)
  
  df_recovered$recovered <- as.integer(df_recovered$recovered)
  
  rm(df_temp)
  

  df_confirmed <-df_confirmed%>%group_by(Country.Region,Date)%>%
    summarise(confirmed=sum(confirmed, na.rm=T))
  
  df_confirmed <-df_confirmed%>%
    mutate(grouping=sprintf("%s-%s",Country.Region,Date))%>% 
    mutate(anterior = lag(confirmed, order_by=grouping))
  
  
  df_confirmed$anterior[is.na(df_confirmed$anterior)] <-0
  
  df_confirmed$new_confirmed <-df_confirmed$confirmed-df_confirmed$anterior
  
  df_confirmed$new_confirmed[df_confirmed$new_confirmed<0]<-0
  
  
  
  #Muertes
  df_deaths <-df_deaths%>%group_by(Country.Region,Date)%>%
    summarise(deaths=sum(deaths, na.rm=T))
  #%>%    select(deaths,Country.Region,Date)
  
  
  df_deaths <-df_deaths%>%
    mutate(grouping=sprintf("%s-%s",Country.Region,Date))%>% 
    mutate(anterior = lag(deaths, order_by=grouping))
  #%>% select(-grouping)
  
  df_deaths$anterior[is.na(df_deaths$anterior)] <-0
  
  df_deaths$new_deaths <-df_deaths$deaths-df_deaths$anterior
  
  df_deaths$new_deaths[df_deaths$new_deaths<0]<-0
  
  
  
  ## Recuperados
 
  df_recovered <-df_recovered%>%group_by(Country.Region,Date)%>%
    summarise(recovered=sum(recovered, na.rm=T))
  #%>%    select(recovered,Country.Region,Date)
  
  df_recovered <-df_recovered%>%
    mutate(grouping=sprintf("%s-%s",Country.Region,Date))%>% 
    mutate(anterior = lag(recovered, order_by=grouping)) 
  #%>% select(-grouping)
  
  df_recovered$anterior[is.na(df_recovered$anterior)] <-0
  
  df_recovered$new_recovered <-df_recovered$recovered-df_recovered$anterior
  
  
 
  df_full <-df_confirmed%>%left_join(df_deaths, by = c("Country.Region","Date" ))%>%
    left_join(df_recovered, by = c("Country.Region","Date" ))%>%
    mutate(
      confirmed=replace_na(confirmed,0), 
      new_confirmed=replace_na(new_confirmed,0), 
      deaths=replace_na(deaths,0), 
      new_deaths=replace_na(new_deaths,0), 
      recovered=replace_na(recovered,0), 
      new_recovered=replace_na(new_recovered,0)
    )%>%
    select(Country.Region,Date,confirmed, new_confirmed, deaths, 
           new_deaths, recovered, new_recovered)
  
  
  df_full$active<-df_full$confirmed-df_full$deaths-df_full$recovered
  
  df_full$active[df_full$active<0]<-0
  
  df_full$new_active<-df_full$new_confirmed-df_full$new_deaths-df_full$new_recovered
  
  
  df_active <-df_full%>%
    mutate(grouping=sprintf("%s-%s",Country.Region,Date))%>% 
    mutate(anterior = lag(active, order_by=grouping)) 

  df_full$new_active <-df_full$active-df_active$anterior
  
  df_full$new_active[is.na(df_full$new_active)] <-0

  df_full[(which(df_full$Country.Region =="Cote d'Ivoire")),"Country.Region"] <-"Ivory Coast"
  df_full[(which(df_full$Country.Region =="US")),"Country.Region"] <-"United States of America"
  df_full[(which(df_full$Country.Region =="Congo (Kinshasa)")),"Country.Region"] <-"Republic of Congo"
  df_full[(which(df_full$Country.Region =="Congo (Brazzaville)")),"Country.Region"] <-"Republic of Congo"
  df_full[(which(df_full$Country.Region =="Serbia")),"Country.Region"] <-"Republic of Serbia"
  df_full[(which(df_full$Country.Region =="Bahamas, The")),"Country.Region"] <-"The Bahamas"
  df_full[(which(df_full$Country.Region =="North Macedonia")),"Country.Region"] <-"Macedonia"
  df_full[(which(df_full$Country.Region =="Korea, South")),"Country.Region"] <-"South Korea"
  df_full[(which(df_full$Country.Region =="Czechia")),"Country.Region"] <-"Czech Republic"
  df_full[(which(df_full$Country.Region =="Gambia, The")),"Country.Region"] <-"Gambia"
  df_full[(which(df_full$Country.Region =="Taiwan*")),"Country.Region"] <-"Taiwan"
  df_full[(which(df_full$Country.Region =="Cabo Verde")),"Country.Region"] <-"Cape Verde"
  df_full[(which(df_full$Country.Region =="Tanzania")),"Country.Region"] <-"United Republic of Tanzania"
  
  
  #write.csv(df_paises,"./df_paises.csv", row.names = TRUE)
  write.csv(df_full,"./df_full.csv", row.names = TRUE)
  
  rm(df_confirmed,df_recovered,df_deaths,df_active)
  
  #pruebas,comentar
  # ult_fecha <-max(ymd(df_full$Date))
  # 
  # df_temp <-df_full%>%filter(Date ==ult_fecha)
  # 
  # sum(df_temp$confirmed)
  # sum(df_temp$deaths)
  # sum(df_temp$new_confirmed)
  # sum(df_temp$new_deaths)
  # sum(df_temp$active)
  # sum(df_temp$new_active)
  # ult_fecha
  # df_full[df_full$Country.Region=="Argentina"& df_full$Date ==ult_fecha,]
  
  #rm(list=ls())
  
}  


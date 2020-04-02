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
library(tidyr)

library(sf)
library(plotly)
library(dplyr)
library(lubridate)
library(spData)

#script para generar el csv con toda la info junta que se baja de internet
setwd("C:/Users/MJFerreyra/Documents/COVID-19/Deploy")
#df_paises <- st_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson",stringsAsFactors =FALSE)
#df_paises$geometry2 <- st_centroid(st_geometry(df_paises))


df_confirmed <-read.csv("./time_series_covid19_confirmed_global_narrow.csv", stringsAsFactors = FALSE)
names(df_confirmed)[6] <- "confirmed"
df_confirmed$confirmed <- as.integer(df_confirmed$confirmed )

df_deaths <-read.csv("./time_series_covid19_deaths_global_narrow.csv", stringsAsFactors = FALSE)
names(df_deaths)[6] <- "deaths"
df_deaths$deaths <- as.integer(df_deaths$deaths )

df_recovered <-read.csv("./time_series_covid19_recovered_global_narrow.csv", stringsAsFactors = FALSE)
names(df_recovered)[6] <- "recovered"
df_recovered$recovered <- as.integer(df_recovered$recovered )

# Confirmados

df_confirmed[df_confirmed$Province.State=="","Province.State"]<-df_confirmed[df_confirmed$Province.State=="","Country.Region"]

df_confirmed <-df_confirmed [-1,] 

df_confirmed$Date <- ymd(df_confirmed$Date)

df_confirmed <-df_confirmed%>%
  mutate(grouping=sprintf("%s-%s-%s",Country.Region,Province.State,Date))%>% 
  mutate(anterior = lag(confirmed, order_by=grouping)) %>% select(-grouping)

df_confirmed$new_confirmed <-df_confirmed$confirmed-df_confirmed$anterior


df_confirmed$new_confirmed[df_confirmed$new_confirmed<0]<-0

                                                           
#head(df_confirmed)   

#Muertes

df_deaths[df_deaths$Province.State=="","Province.State"]<-df_deaths[df_deaths$Province.State=="","Country.Region"]

df_deaths <-df_deaths [-1,] 

df_deaths$Date <- ymd(df_deaths$Date)

df_deaths <-df_deaths%>%
  mutate(grouping=sprintf("%s-%s-%s",Country.Region,Province.State,Date))%>% 
  mutate(anterior = lag(deaths, order_by=grouping)) %>% select(-grouping)

df_deaths$new_deaths <-df_deaths$deaths-df_deaths$anterior

df_deaths$new_deaths[df_deaths$new_deaths<0]<-0



## Recuperados

df_recovered[df_recovered$Province.State=="","Province.State"]<-df_recovered[df_recovered$Province.State=="","Country.Region"]

df_recovered <-df_recovered [-1,] 

df_recovered$Date <- ymd(df_recovered$Date)

df_recovered <-df_recovered%>%
  mutate(grouping=sprintf("%s-%s-%s",Country.Region,Province.State,Date))%>% 
  mutate(anterior = lag(recovered, order_by=grouping)) %>% select(-grouping)

df_recovered$new_recovered <-df_recovered$recovered-df_recovered$anterior


df_recovered$new_recovered[df_recovered$new_recovered<0]<-0


#head(df_deaths)  

df_full <-df_confirmed%>%left_join(df_deaths, by = c("Province.State","Country.Region","Lat","Long","Date" ))%>%
  left_join(df_recovered, by = c("Province.State","Country.Region","Lat","Long","Date" ))%>%
  mutate(
         confirmed=replace_na(confirmed,0), 
         new_confirmed=replace_na(new_confirmed,0), 
         deaths=replace_na(deaths,0), 
         new_deaths=replace_na(new_deaths,0), 
         recovered=replace_na(recovered,0), 
         new_recovered=replace_na(new_recovered,0)
         )%>%
  select(Province.State,Country.Region,Lat,Long,Date,confirmed, new_confirmed, deaths, new_deaths, recovered, new_recovered)

df_full$active<-df_full$confirmed-df_full$deaths-df_full$recovered
df_full$new_active<-df_full$new_confirmed-df_full$new_deaths-df_full$new_recovered


df_full$new_active[df_full$new_active<0]<-0

#df_full <-df_full [-1,] 

#head(df_full) 


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

rm(df_confirmed,df_deaths,df_recovered)

#write.csv(df_paises,"./df_paises.csv", row.names = TRUE)
write.csv(df_full,"./df_full.csv", row.names = TRUE)

#pruebas,comentar
ult_fecha <-max(ymd(df_full$Date))

df_temp <-df_full%>%filter(Date ==ult_fecha)

sum(df_temp$confirmed)
sum(df_temp$deaths)
sum(df_temp$new_confirmed)
sum(df_temp$new_deaths)
sum(df_temp$active)
sum(df_temp$new_active)
ult_fecha

#rm(df_full,df_temp, ult_fecha)
rm(list=ls())

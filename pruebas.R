# 
# 
# 
# dftemp <- st_read("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-21-2020.csv",stringsAsFactors =FALSE)
# 
# names(dftemp)[3]<-"Province.State"
# names(dftemp)[4]<-"Country.Region"
# names(dftemp)[5]<-"Date"
# names(dftemp)[7]<-"Long"
# names(dftemp)[8]<-"confirmed"
# names(dftemp)[9]<-"deaths"
# names(dftemp)[10]<-"recovered"
# names(dftemp)[11]<-"active"
# 
# 
# dftemp$confirmed <- as.integer(dftemp$confirmed )
# dftemp$deaths <- as.integer(dftemp$deaths )
# dftemp$recovered <- as.integer(dftemp$recovered )
# dftemp$active <- as.integer(dftemp$active )
# 
# View(dftemp)
# 
# dftemp <-dftemp%>%
#   mutate(grouping=sprintf("%s-%s-%s",Country.Region,Province.State,Date))%>% 
#   mutate(anterior_deaths = lag(deaths, order_by=grouping),
#          anterior_confirmed = lag(confirmed, order_by=grouping),
#          anterior_recovered = lag(recovered, order_by=grouping)) %>% select(-grouping)
# 
# dftemp$new_deaths <-dftemp$deaths-dftemp$anterior_deaths
# dftemp$new_confirmed <-dftemp$confirmed-dftemp$anterior_confirmed
# dftemp$new_recovered <-dftemp$recovered-dftemp$anterior_recovered
# 
# dftemp$new_deaths[dftemp$new_deaths<0]<-0
# 
# 
# summary(dftemp)
# 
# 
# 
# df_full2 <-dftemp%>%mutate(
#     confirmed=replace_na(confirmed,0), 
#     new_confirmed=replace_na(new_confirmed,0), 
#     deaths=replace_na(deaths,0), 
#     new_deaths=replace_na(new_deaths,0), 
#     recovered=replace_na(recovered,0), 
#     new_recovered=replace_na(new_recovered,0)
#   )%>%
#   select(Province.State,Country.Region,Lat,Long,Date,confirmed, 
#          new_confirmed, deaths, new_deaths, recovered, new_recovered)
library(tidyr)
library(lubridate)


df_temp <- st_read("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",stringsAsFactors =FALSE)


df_confirmed <-gather(df_temp, "Date", "confirmed", 5:ncol(df_temp))
df_confirmed$Date <-gsub("X", "", df_confirmed$Date)
df_confirmed$Date <-mdy(df_confirmed$Date)

df_confirmed$confirmed <- as.integer(df_confirmed$confirmed)

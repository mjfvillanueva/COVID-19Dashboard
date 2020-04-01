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
library(DT)

library(sf)
library(plotly)
library(dplyr)
library(lubridate)
#library(spData)


setwd("C:/Users/MJFerreyra/Documents/COVID-19")
df_full <-read.csv("./df_full.csv", stringsAsFactors = FALSE)

#df_paises <- st_read("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson",stringsAsFactors =FALSE)
df_paises <- st_read("./countries.geojson",stringsAsFactors =FALSE)


#renombro saco columnas que no me sirven
names(df_paises)[1] <- "location"
df_paises$ISO_A3 <- NULL
df_paises$ISO_A2 <- NULL


df_paises <-df_full%>%group_by(Country.Region,Date)%>%
 # summarise(confirmed=sum(confirmed, na.rm=T), recovered=sum(recovered, na.rm=T), deaths=sum(deaths, na.rm=T))%>%
  summarise(confirmed=sum(confirmed, na.rm=T), 
            new_confirmed=sum(new_confirmed, na.rm=T),
            recovered=sum(recovered, na.rm=T), 
            new_recovered=sum(new_recovered, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T),
            active=sum(active, na.rm=T),
            new_active=sum(new_active, na.rm=T))%>%
  select(confirmed,recovered,deaths,active,new_confirmed,new_recovered,new_deaths,new_active,Country.Region,Date)%>%
  left_join(df_paises, by= c("Country.Region"="location"))


df_full <-df_full%>%group_by(Province.State,Date, Lat, Long)%>%
  summarise(confirmed=sum(confirmed, na.rm=T), 
            new_confirmed=sum(new_confirmed, na.rm=T),
            recovered=sum(recovered, na.rm=T), 
            new_recovered=sum(new_recovered, na.rm=T),
            deaths=sum(deaths, na.rm=T),
            new_deaths=sum(new_deaths, na.rm=T),
            active=sum(active, na.rm=T),
            new_active=sum(new_active, na.rm=T))%>%
  select(confirmed,recovered,deaths,active,new_confirmed,new_recovered,new_deaths,new_active,Province.State,Date, Lat, Long)


df_paises$Country.Region <- as.factor(df_paises$Country.Region)
df_full$Province.State <- as.factor(df_full$Province.State)

pal_graf <- c("#712b29"  ,"#7f401f", "#4a4945","#4c6627" )

ui <- shinyUI(fluidPage(
  #ojo, usar lo de abajo cuando necesito usar widget del plus y estoy en shiny clasico
  # sino no funcionan
  #useShinydashboardPlus(),
  
  theme = "https://stackpath.bootstrapcdn.com/bootswatch/4.4.1/sandstone/bootstrap.css",

 fluidRow(style = "padding: 0px;",
          column(style="padding:0px;",
            width = 6, h3("COVID-19 Dashboard"),
           # tagList(icon("globe-americas", "fa-2x"),
          #          h3("COVID-19 Dashboard")
          #          )
              offset = 0),
          column(style="padding:0px;",
            width = 6,align="right",
            uiOutput("actualizacion"),  offset = 0)
 
         
          ),
 
    fluidRow(
      column(class="alert alert-dismissible alert-danger", #
            width = 3,align="center",
            uiOutput("conf_html"),offset = 0),     
      
      column(class="alert alert-dismissible alert-warning", #  
             width = 3,align="center",
            uiOutput("act_html"),
             offset = 0),   
      
      column(class="alert alert-dismissible alert-secondary", #  
        width = 3,align="center",
        uiOutput("muer_html"), offset = 0),
      
      column(class="alert alert-dismissible alert-success", #

        width = 3,align="center",
        uiOutput("rec_html"), offset = 0)   
      
             ), 
    fluidRow(
      
     # splitLayout(

      #  cellWidths = c("65%", "35%"),
     
       # cellArgs = list(style = "padding: 1px;height:80vh; "),
      column(width=7,offset=0,style = "padding: 0px;",
        tabsetPanel(
          tabPanel("Paises",  leafletOutput("mapapais", height = 490)),
          tabPanel("Principales Ciudades", leafletOutput("mapaciudades", height = 490))
        )),
      column(width=5,offset=0,style = "padding: 0px;",
             
        tabsetPanel(
          tabPanel(style = "padding: 10px;",
            "Grafico", height ="100%",width = "100%",

                div(style = "display:inline-block; width: 40%;",
                    selectInput(label="Pais", inputId="sPais",choices=c("TODO EL MUNDO",levels(df_paises$Country.Region)),
                                selected="TODO EL MUNDO")),
                div(style = "display:inline-block; width: 40%;",
                    selectInput(label="Ver",inputId="sGraficoPor",choices=c("Acumulado","Casos Reportados"), 
                                selected="Acumulado")),
              plotlyOutput("graf_1", height = "400", width = "100%")
            ),
          tabPanel("Datos",height =490,
                   style = "padding: 10px;",
                   dataTableOutput("tab_1"))
        ))
      #)  
    )
  )
  )


server <- function(input, output,  session) {
 #options(shiny.trace=T)
  
   fecha_ult_dato <- reactive(
     max(ymd(df_full$Date))
   )
   
   df_ult_casos <- reactive({
     
     df_ult_casos <- df_paises%>%filter(Date ==fecha_ult_dato())
    
     df_ult_casos 
   })
   
   df_ult_casos_c <- reactive({
     
     df_ult_casos_ciudades <- df_full%>%filter(Date ==fecha_ult_dato())
     
     df_ult_casos_ciudades 
   })

   
   df_serie <- reactive({
     
    # periodo <- input$sel_periodo
     
    # if(periodo=="Ultimos 10 dias")
    #   fecha <-fecha_ult_dato()-10
    # else if (periodo=="Ultimo mes")
    #  fecha <-fecha_ult_dato()-30
    #  else if (periodo=="Historico")
      fecha <- ymd("1900-01-01")
      
     df_serie <- df_paises%>%filter(ymd(Date) > fecha)%>%
       group_by(Date=ymd(Date), Country.Region)%>%
       summarise(confirmed=sum(confirmed, na.rm=T), 
                 new_confirmed=sum(new_confirmed, na.rm=T),
                 recovered=sum(recovered, na.rm=T), 
                 new_recovered=sum(new_recovered, na.rm=T),
                 deaths=sum(deaths, na.rm=T),
                 new_deaths=sum(new_deaths, na.rm=T),
                 active=sum(active, na.rm=T),
                 new_active=sum(new_active, na.rm=T))
      # summarise(confirmed=sum(confirmed, na.rm=T), recovered=sum(recovered, na.rm=T), deaths=sum(deaths, na.rm=T),active=sum(active, na.rm=T))
     
     df_serie 
   })
  
   casos_conf <- reactive({sum(df_ult_casos()$confirmed, na.rm=T)})
   casos_act<- reactive({sum(df_ult_casos()$active, na.rm=T)})
   casos_rec <- reactive({sum(df_ult_casos()$recovered, na.rm=T)})
   casos_muer <- reactive({sum(df_ult_casos()$deaths, na.rm=T)})
   
   conf_porc <- reactive( {paste(as.character(round(sum(df_ult_casos()$new_confirmed, na.rm=T)/(sum(df_ult_casos()$confirmed, na.rm=T)-sum(df_ult_casos()$new_confirmed, na.rm=T))*100,2))," %") })
   
   act_porc <- reactive( {paste(as.character(round(sum(df_ult_casos()$new_active, na.rm=T)/(sum(df_ult_casos()$active, na.rm=T)-sum(df_ult_casos()$new_active, na.rm=T))*100,2))," %") })
   
   muer_porc <- reactive( {paste(as.character(round(sum(df_ult_casos()$new_deaths, na.rm=T)/(sum(df_ult_casos()$deaths, na.rm=T)-sum(df_ult_casos()$new_deaths, na.rm=T))*100,2))," %") })
   
   rec_porc <- reactive( {paste(as.character(round(sum(df_ult_casos()$new_recovered, na.rm=T)/(sum(df_ult_casos()$recovered, na.rm=T)-sum(df_ult_casos()$new_recovered, na.rm=T))*100,2))," %") })
   
   output$conf_html <- renderUI({
    # tags$div(HTML("<i class='fas fa-arrow-circle-down'></i> 58%")),
     tagList(
       h1(format(casos_conf(), big.mark=".",small.mark=".") ),
       h3("CONFIRMADOS"),
       icon('fas fa-arrow-circle-up'),
       conf_porc()
     )
     
   })
   output$act_html <- renderUI({
     tagList(
       h1(format(casos_act(), big.mark=".",small.mark=".") ),
       h3("ACTIVOS"),
       icon('fas fa-arrow-circle-up'),
       act_porc()
     )
     
   })
   
   output$muer_html <- renderUI({
     tagList(
       h1(format(casos_muer(), big.mark=".",small.mark=".") ),
       h3("MUERTES"),
       icon('fas fa-arrow-circle-up'),
       muer_porc()
     )
     
   })
   
   output$rec_html <- renderUI({
     tagList(
       h1(format(casos_rec(), big.mark=".",small.mark=".") ),
       h3("RECUPERADOS"),
       icon('fas fa-arrow-circle-up'),
       rec_porc()
     )
     
   })
  
   output$actualizacion<- renderUI({ 
     tagList("Actualizacion diaria, dataset ",
        a("HDX", href = "https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"),
        ". Última actualizacion", 
        format(fecha_ult_dato(),"%d/%m/%Y"), 
        tags$br(),
        a("by MJFV", href = "mailto:mjferreyra@hotmail.com")
     )
    })
  
  
    output$mapapais <- renderLeaflet({
      
      labels <- sprintf(
           "<strong>%s</strong><br/>Confirmados: %s<br/>Muertes: %s<br/>Recuperados: %s<br/>Activos: %s",
           df_ult_casos()$Country.Region,
           paste0(format(df_ult_casos()$confirmed, big.mark=".",small.mark="."),"(",
           ifelse((df_ult_casos()$confirmed-df_ult_casos()$new_confirmed) >0 ,
             "+", ""),
           round(ifelse((df_ult_casos()$confirmed-df_ult_casos()$new_confirmed)==0,0,df_ult_casos()$new_confirmed/(df_ult_casos()$confirmed-df_ult_casos()$new_confirmed)*100),1),
           "%)",
             sep=" "),
           paste0(format(df_ult_casos()$deaths, big.mark=".",small.mark="."),"(",
            ifelse((df_ult_casos()$deaths-df_ult_casos()$new_deaths) >0 ,
                   "+", ""),
            round(ifelse((df_ult_casos()$deaths-df_ult_casos()$new_deaths)==0,0,df_ult_casos()$new_deaths/(df_ult_casos()$deaths-df_ult_casos()$new_deaths)*100),1),
            "%)",
            sep=" "),
          
           paste0(format(df_ult_casos()$recovered, big.mark=".",small.mark="."),"(",
            ifelse((df_ult_casos()$recovered-df_ult_casos()$new_recovered) >0 ,
                   "+", ""),
            round(ifelse((df_ult_casos()$recovered-df_ult_casos()$new_recovered)==0,0,df_ult_casos()$new_recovered/(df_ult_casos()$recovered-df_ult_casos()$new_recovered)*100),1),
            "%)",
            sep=" "),
           paste0(format(df_ult_casos()$active, big.mark=".",small.mark="."),"(",
            ifelse((df_ult_casos()$active-df_ult_casos()$new_active) >0 ,
                   "+", ""),
            round(ifelse((df_ult_casos()$active-df_ult_casos()$new_active)==0,0,df_ult_casos()$new_active/(df_ult_casos()$active-df_ult_casos()$new_active)*100),1),
            "%)",
            sep=" ")%>% lapply(htmltools::HTML))
      
      bin1 <- c(1,100,500,3000,10000,50000,500000)
      palc <- colorBin(palette ="YlOrRd", domain = df_ult_casos()$confirmed, bins = bin1)
      
      bin2 <- c(0,1,500,1500,3000,10000)
      pald <- colorBin(palette ="Greys", domain = df_ult_casos()$deaths, bins = bin2)
      
      bin3 <- c(0,1,100,500,3000,10000,50000,300000)
      palr <- colorBin(palette ="Greens", domain = df_ult_casos()$recovered, bins = bin3)
      
      bin4 <- c(1,100,500,3000,10000,50000,500000)
      pala <- colorBin(palette ="Oranges", domain = df_ult_casos()$active, bins = bin4)
  

      
      leaflet()%>%
         addTiles()%>%
         setView(0,0,2)%>%
         addPolygons(data = df_ult_casos()$geometry,
                     weight = 0.5, 
                     smoothFactor = 0.5,
                     opacity = 2.0,
                     fillOpacity = 0.6,
                     color=palc(df_ult_casos()$confirmed),
                     popup=labels,
                     #stroke = FALSE,
                     layerId =df_ult_casos()$Country.Region,
                     highlightOptions = highlightOptions(color = "white", 
                                                         weight = 0.8, bringToFront = TRUE),
                     group="Confirmados")%>%
         addPolygons(data = df_ult_casos()$geometry,
                     weight = 0.5, 
                     smoothFactor = 0.5,
                     opacity = 2.0,
                     fillOpacity = 0.6,
                     color=pald(df_ult_casos()$deaths),
                     popup=labels,
                     #layerId =df_ult_casos()$Country.Region,
                     highlightOptions = highlightOptions(color = "white", 
                                                         weight = 0.8, bringToFront = TRUE),
                     group="Muertes")%>%
         addPolygons(data = df_ult_casos()$geometry,
                     weight = 0.5, 
                     smoothFactor = 0.5,
                     opacity = 2.0,
                     fillOpacity = 0.6,
                     color=palr(df_ult_casos()$recovered),
                     popup=labels,
                     highlightOptions = highlightOptions(color = "white", 
                                                         weight = 0.8, bringToFront = TRUE),
                     group="Recuperados")%>%
        addPolygons(data = df_ult_casos()$geometry,
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 2.0,
                    fillOpacity = 0.6,
                    color=pala(df_ult_casos()$active),
                    popup=labels,
                    #stroke = FALSE,
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 0.8, bringToFront = TRUE),
                    group="Activos")%>%
         addLayersControl(
            baseGroups = c("Confirmados","Activos","Muertes","Recuperados"),
            options = layersControlOptions(collapsed = FALSE))
         
    })
    observeEvent(input$mapapais_groups,{
      bin1 <- c(1,100,500,3000,10000,50000,500000)
      palc <- colorBin(palette ="YlOrRd", domain = df_ult_casos()$confirmed, bins = bin1)
      
      bin2 <- c(0,1,500,1500,3000,10000)
      pald <- colorBin(palette ="Greys", domain = df_ult_casos()$deaths, bins = bin2)
      
      bin3 <- c(0,1,100,500,3000,10000,50000,300000)
      palr <- colorBin(palette ="Greens", domain = df_ult_casos()$recovered, bins = bin3)
      
      bin4 <- c(1,100,500,3000,10000,50000,500000)
      pala <- colorBin(palette ="Oranges", domain = df_ult_casos()$active, bins = bin4)
      
      
    
      mapapais <- leafletProxy("mapapais") %>% clearControls()
      if (input$mapapais_groups == "Confirmados")
      {
        mapapais <- mapapais%>%
          addLegend(
            pal = palc, 
            values = df_ult_casos()$confirmed, 
            #opacity = 0.7, 
            opacity = 0.6,
            title = "Casos confirmados",
            position = "bottomleft",
            group="Confirmados")
      }
      
      else if (input$mapapais_groups == "Muertes")
      {
        mapapais<- mapapais%>%addLegend(
          pal = pald, 
          values = df_ult_casos()$deaths, 
          #opacity = 0.7, 
          opacity = 0.6,
          title = "Muertes",
          position = "bottomleft",
          group="Muertes")
      }
      else if (input$mapapais_groups == "Recuperados")
      {
        mapapais<- mapapais%>%addLegend(
          pal = palr, 
          values = df_ult_casos()$recovered, 
          #opacity = 0.7, 
          opacity = 0.6,
          title = "Recuperados",
          position = "bottomleft",
          group="Recuperados")
      }
      else if (input$mapapais_groups == "Activos")
      {
        mapapais<- mapapais%>%addLegend(
          pal = pala, 
          values = df_ult_casos()$active, 
          #opacity = 0.7, 
          opacity = 0.6,
          title = "Activos",
          position = "bottomleft",
          group="Activos")
      }
      
    })
    
   output$tab_1 <- renderDataTable({
      df <- df_ult_casos()%>%select(Pais=Country.Region,
                                  Confirmados=confirmed,
                                  Muertes=deaths,
                                  Recuperados=recovered)%>%
      mutate(Activos= Confirmados-Muertes-Recuperados)%>%
      arrange(desc(Confirmados))
   
     #le saco caracteres para que no se deforme la tabla en la pantalla 
      df$Pais <- substr(df$Pais,1,27)
    
      df<-datatable(df,
              rownames= FALSE,
              options = list(pageLength = 13,
                             paging = TRUE, 
                             lengthMenu = list(c(5, 13, -1), c('5', '13', 'All'))))

  })
  
  df_serie_todo <- reactive({
    
    df_serie_todo <-df_serie()%>%group_by(Date=ymd(Date))%>%
      summarise(confirmed=sum(confirmed, na.rm=T), 
                new_confirmed=sum(new_confirmed, na.rm=T),
                recovered=sum(recovered, na.rm=T), 
                new_recovered=sum(new_recovered, na.rm=T),
                deaths=sum(deaths, na.rm=T),
                new_deaths=sum(new_deaths, na.rm=T),
                active=sum(active, na.rm=T),
                new_active=sum(new_active, na.rm=T))
     # summarise(confirmed=sum(confirmed, na.rm=T), recovered=sum(recovered, na.rm=T), deaths=sum(deaths, na.rm=T))
    
    df_serie_todo 
  })

  
  ver_todo <- reactiveValues(data = NULL)
  
  observeEvent(input$mapapais_shape_click,{
    
    click <- input$mapapais_shape_click
    ver_todo$data <- NULL
    
    if (is.null(click))
    {
      
      ver_todo$data <- "TODO EL MUNDO"
    }
    else
    {
      ver_todo$data <-click$id
    }      
    
    updateSelectInput (session, "sPais",selected = ver_todo$data)
    
    #output$text <- renderPrint( 
    #  print(click))
    
  })
  
  #  output$text <- renderPrint( 
  #  print( output$text <- renderPrint( 
  #    print(input$mapapais_shape_click))))
  
  observeEvent( input$sPais, {
   
      ver_todo$data <- input$sPais
     
  })
  
  
  output$graf_1<- renderPlotly({
    

    a <- list(showticklabels = TRUE, tick0=min(df_serie()$Date), 
              tickformat = "%d %b",title = '',dtick=86400000.0 * 14, type="date"                )
    
    t <- list(
     # family = "sans serif",
      size = 10)
    #  color = 'blue')
    
    if (ver_todo$data=="TODO EL MUNDO")
    {
    
      df <- df_serie()%>%group_by(Date)%>%
        summarise(confirmed=sum(confirmed, na.rm=T), 
                  new_confirmed=sum(new_confirmed, na.rm=T),
                  recovered=sum(recovered, na.rm=T), 
                  new_recovered=sum(new_recovered, na.rm=T),
                  deaths=sum(deaths, na.rm=T),
                  new_deaths=sum(new_deaths, na.rm=T),
                  active=sum(active, na.rm=T),
                  new_active=sum(new_active, na.rm=T))
      titulo <- "TODO EL MUNDO"
      
    }
    else
    {
 
      df<-df_serie()%>%filter(Country.Region== ver_todo$data)
      
      titulo <- ver_todo$data
    }
      
    if (input$sGraficoPor=="Acumulado")
    {
    plot_ly(df, x=~Date, y=~confirmed, type="bar", orientation="v", name="Confirmados",colors = pal_graf,
      hovertemplate = paste('<b>Casos:</b>: %{y:.0f}',
                                  '<br><b>Fecha</b>: %{x}')
          )%>% 
      add_trace(y = ~active, name = 'Activos',
                hovertemplate = paste('<b>Casos:</b>: %{y:.0f}',
                                      '<br><b>Fecha</b>: %{x}'))%>%
      add_trace(y = ~deaths, name = 'Muertes',
                hovertemplate = paste('<b>Casos:</b>: %{y:.0f}',
                                      '<br><b>Fecha</b>: %{x}'))%>% 
      add_trace(y = ~recovered, name = 'Recuperados',
                hovertemplate = paste('<b>Casos:</b>: %{y:.0f}',
                                      '<br><b>Fecha</b>: %{x}'))%>%
     layout(
        barmode = 'group',
        yaxis = list(title = "Acumulado"),
        xaxis = a,
        title = titulo,font=t,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5,
                      font = list(size = 10)))
    }
   else
   {
     plot_ly(df, x=~Date, y=~new_confirmed, type="bar", orientation="v", name="Confirmados",colors = pal_graf)%>% 
       add_trace(y = ~new_active, name = 'Activos')%>%
       add_trace(y = ~new_deaths, name = 'Muertes')%>% 
       add_trace(y = ~new_recovered, name = 'Recuperados')%>%
       layout(
         barmode = 'group',
         yaxis = list(title = "Casos reportados"),
         xaxis = a,
         title = titulo,
         legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5,
                       font = list(size = 8)))
     
   }
    
  } )
  
  

  output$mapaciudades <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Confirmados: %s<br/>Muertes: %s<br/>Recuperados: %s<br/>Activos: %s",
     df_ult_casos_c()$Province.State,
     format(df_ult_casos_c()$confirmed, big.mark=".",small.mark="."),
     format(df_ult_casos_c()$deaths, big.mark=".",small.mark="."),
     format(df_ult_casos_c()$recovered, big.mark=".",small.mark="."),
     format(df_ult_casos_c()$active, big.mark=".",small.mark="."))%>% lapply(htmltools::HTML)
    
    const <- 20
    
    leaflet()%>%
      addTiles()%>%
      setView(0,0,2)%>%
        addCircles(data=df_ult_casos_c(),
                          lng =~df_ult_casos_c()$Long, 
                          lat = ~df_ult_casos_c()$Lat, 
                          weight = 1,
                          #popup = labels,
                          color="Red",
                          label=labels,
                          opacity = .5,
                          radius = ~(df_ult_casos_c()$confirmed*const),
                          group="Confirmados")%>%
      addCircles(data=df_ult_casos_c(),
                       lng =~df_ult_casos_c()$Long, 
                       lat = ~df_ult_casos_c()$Lat, 
                       weight = 1,
                       #popup = labels,
                       color="Grey",
                       label=labels,
                       opacity = .5,
                       radius = ~(df_ult_casos_c()$deaths*const),
                       group="Muertes")%>%
      addCircles(data=df_ult_casos_c(),
                       lng =~df_ult_casos_c()$Long, 
                       lat = ~df_ult_casos_c()$Lat, 
                       weight = 1,
                       #popup = labels,
                       color="Green",
                       label=labels,
                       opacity = .5,
                       radius = ~(df_ult_casos_c()$recovered*const),
                       group="Recuperados")%>%
      addCircles(data=df_ult_casos_c(),
                 lng =~df_ult_casos_c()$Long, 
                 lat = ~df_ult_casos_c()$Lat, 
                 weight = 1,
                 #popup = labels,
                 color="Orange",
                 label=labels,
                 opacity = .5,
                 radius = ~(df_ult_casos_c()$active*const),
                 group="Activos")%>%
      addLayersControl(
        baseGroups = c("Confirmados","Activos","Muertes","Recuperados"),
        options = layersControlOptions(collapsed = FALSE))
    
   
    
  })
  
 
}



shinyApp(ui, server)




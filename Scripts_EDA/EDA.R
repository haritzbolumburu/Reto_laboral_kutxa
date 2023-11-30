#EDA

#graficos para el analisis EDAD
# GRAFICOS 1 --------------------------------------------------------------
#grafico de relación de ipc y de GDP
# USA ---------------------------------------------------------------------
datos_usa <- read_csv("./Datos/Transformados/data_grafico_GDP_IPC_usa.csv")
#por trimestre
datos_usa$trimestre<-ifelse(month(datos_usa$Date)<=3, '1º trimestre',
                            ifelse(month(datos_usa$Date)<=6, '2º trimestre',
                                   ifelse(month(datos_usa$Date)<=9, '3º trimestre', '4º trimestre')))
datos_usa$trimestre<-as.factor(datos_usa$trimestre)
fig <- plot_ly(data = datos_usa, x = ~GDP, y = ~CPI, color = ~trimestre,
               colors = c("brown1", "blue1", "darkseagreen2", "darkorchid2"),
               marker = list(size = 2,
                             color = "azure3"))
fig <- fig %>% layout(title = 'Relacion entre el GDP y CPI de USA por trimestre',
                      yaxis = list(zeroline = FALSE,
                                   title = "CPI"),
                      
                      xaxis = list(zeroline = FALSE,
                                   title = "GDP"))

fig

#por decada
datos_usa$decada<-ifelse(year(datos_usa$Date)<=1980, "decada de los 70",
                         ifelse(year(datos_usa$Date)<=1990, "decada de los 80",
                                ifelse(year(datos_usa$Date)<=2000, "decada de los 90",
                                       ifelse(year(datos_usa$Date)<=2010, "decada de los 2000", "de 2010 a 2022"))))
datos_usa$decada<-as.factor(datos_usa$decada)
fig <- plot_ly(data = datos_usa, x = ~GDP, y = ~CPI, color = ~decada,
               colors = c("brown1", "blue1", "darkseagreen2", "darkorchid2", "gold"),
               marker = list(size = 2,
                             color = "azure3"))
fig <- fig %>% layout(title = 'Relacion entre el GDP y CPI de USA por decada',
                      yaxis = list(zeroline = FALSE,
                                   title = "CPI"),
                      
                      xaxis = list(zeroline = FALSE,
                                   title = "GDP"))

fig

saveWidget(fig, file="./Graficos/GDP_IPC_USA_d.html", selfcontained = T,libdir="lib")
# FRANCE ---------------------------------------------------------------------
datos_fra <- read_csv("./Datos/Transformados/data_grafico_GDP_IPC_fra.csv")

#por trimestre
datos_fra$trimestre<-ifelse(month(datos_fra$Date)<=3, '1º trimestre',
                            ifelse(month(datos_fra$Date)<=6, '2º trimestre',
                                   ifelse(month(datos_fra$Date)<=9, '3º trimestre', '4º trimestre')))
datos_fra$trimestre<-as.factor(datos_fra$trimestre)
fig <- plot_ly(data = datos_fra, x = ~GDP, y = ~CPI, color = ~trimestre,
               colors = c("brown1", "blue1", "darkseagreen2", "darkorchid2"),
               marker = list(size = 2,
                             color = "azure3"))
fig <- fig %>% layout(title = 'Relacion entre el GDP y CPI de FRANCIA por trimestre',
                      yaxis = list(zeroline = FALSE,
                                   title = "CPI"),
                      
                      xaxis = list(zeroline = FALSE,
                                   title = "GDP"))

fig

#por decada
datos_fra$decada<-ifelse(year(datos_fra$Date)<=1980, "decada de los 70",
                         ifelse(year(datos_fra$Date)<=1990, "decada de los 80",
                                ifelse(year(datos_fra$Date)<=2000, "decada de los 90",
                                       ifelse(year(datos_fra$Date)<=2010, "decada de los 2000", "de 2010 a 2022"))))
datos_fra$decada<-as.factor(datos_fra$decada)
fig <- plot_ly(data = datos_fra, x = ~GDP, y = ~CPI, color = ~decada,
               colors = c("brown1", "blue4", "darkseagreen4", "darkorchid2", "gold"),
               marker = list(size = 2,
                             color = "azure3"))
fig <- fig %>% layout(title = 'Relacion entre el GDP y CPI de Francia por decada',
                      yaxis = list(zeroline = FALSE,
                                   title = "CPI"),
                      
                      xaxis = list(zeroline = FALSE,
                                   title = "GDP"))

fig




# GRAFICOS 2 --------------------------------------------------------------
#grafico de evolución con Range Slider y Botones
# USA ---------------------------------------------------------------------
datos_usa_GDP<- read_csv("./Datos/Transformados/data_pib_usa.csv")
str(datos_usa_GDP)
fig <- plot_ly(datos_usa_GDP, type = 'scatter', mode = 'lines',
               fill = 'tozeroy')%>%
  add_trace(x = ~Fecha, y = ~PIB)%>%
  layout(showlegend = F, title='Evolución Temporal del PIB en USA',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig
datos_usa_CPI<- read_csv("./Datos/Transformados/data_ipc_usa.csv")
str(datos_usa_CPI)
fig <- plot_ly(datos_usa_CPI, type = 'scatter', mode = 'lines',
               fill = 'tozeroy')%>%
  add_trace(x = ~Fecha, y = ~CPI)%>%
  layout(showlegend = F, title='Evolución Temporal del CPI en USA',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig
# Francia  ---------------------------------------------------------------------
datos_fra_GDP<- read_csv("./Datos/Transformados/data_pib_francia.csv")
nrow(datos_fra_GDP)
fig <- plot_ly(datos_usa_GDP, type = 'scatter', mode = 'lines',
               fill = 'tozeroy')%>%
  add_trace(x = ~Fecha, y = ~PIB)%>%
  layout(showlegend = F, title='Evolución Temporal del GDP en Francia',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig
datos_fra_CPI<- read_csv("./Datos/Transformados/data_ipc_francia.csv")
str(datos_fra_CPI)
fig <- plot_ly(datos_fra_CPI, type = 'scatter', mode = 'lines',
               fill = 'tozeroy')%>%
  add_trace(x = ~Fecha, y = ~CPI)%>%
  layout(showlegend = F, title='Evolución Temporal del CPI en Francia',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig


# GRAFICOS 3 --------------------------------------------------------------
#grafico de histograma por año, y con puntos mensuales
# USA ---------------------------------------------------------------------
str(datos_usa_GDP)
fig <- plot_ly(datos_usa_GDP, x = ~PIB,y = ~Fecha, type = 'scatter', mode = 'markers', name = 'monthly', marker = list(color = '#008B00')) %>%
  add_trace(data = datos_usa_GDP, x = ~PIB, type = 'histogram', histfunc = 'avg', xbins = list(size = "Y1"),
            name = 'year average', marker = list(color = '#8EE5EE',
                                                 line = list(color = '#53868B',
                                                             width = 1.5)))

fig <- fig %>%
  layout(xaxis = list(title = 'avg of GDP',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6',
         title = 'Histograma de GDP de USA',
         bargap = 0.1)

fig <- fig %>%
  layout(hovermode="x unified",
         xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6', width = 900)

fig

# Francia ---------------------------------------------------------------------
str(datos_fra_GDP)
fig <- plot_ly(datos_fra_GDP, x = ~PIB,y = ~Fecha, type = 'scatter', mode = 'markers', name = 'monthly', marker = list(color = '#008B00')) %>%
  add_trace(data = datos_fra_GDP, x = ~PIB, type = 'histogram', histfunc = 'avg', xbins = list(size = "Y1"),
            name = 'year average', marker = list(color = '#8EE5EE',
                                                 line = list(color = '#53868B',
                                                             width = 1.5)))

fig <- fig %>%
  layout(xaxis = list(title = 'avg of GDP',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6',
         title = 'Histograma de GDP de Francia',
         bargap = 0.1)

fig <- fig %>%
  layout(hovermode="x unified",
         xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6', width = 900)

fig



# GRAFICOS 4 --------------------------------------------------------------
#mapa de calor según variables
# GDP --------------------------------------------------------------
datos_GDP_year <- read_csv("./Datos/Transformados/data_grafico_GDP_year.csv")

y_1996<-datos_GDP_year[datos_GDP_year$Year == 1996,]
fig_96 <- plot_ly(y_1996, type='choropleth', locations=y_1996$Code,
                  z=y_1996$GDP, text=y_1996$Country, color = ~GDP, colors = 'Purples')

y_2000<-datos_GDP_year[datos_GDP_year$Year == 2000,]
fig_00 <- plot_ly(y_2000, type='choropleth', locations=y_2000$Code,
                  z=y_2000$GDP, text=y_2000$Country, color = ~GDP, colors = 'Purples')

y_2005<-datos_GDP_year[datos_GDP_year$Year == 2005,]
fig_05 <- plot_ly(y_2005, type='choropleth', locations=y_2005$Code,
                  z=y_2005$GDP, text=y_2005$Country, color = ~GDP, colors = 'Purples')

y_2010<-datos_GDP_year[datos_GDP_year$Year == 2010,]
fig_10 <- plot_ly(y_2010, type='choropleth', locations=y_2010$Code,
                  z=y_2010$GDP, text=y_2010$Country, color = ~GDP, colors = 'Purples')

y_2015<-datos_GDP_year[datos_GDP_year$Year == 2015,]
fig_15 <- plot_ly(y_2015, type='choropleth', locations=y_2015$Code,
                  z=y_2015$GDP, text=y_2015$Country, color = ~GDP, colors = 'Purples')

y_2020<-datos_GDP_year[datos_GDP_year$Year == 2020,]
fig_20 <- plot_ly(y_2020, type='choropleth', locations=y_2020$Code,
                  z=y_2020$GDP, text=y_2020$Country, color = ~GDP, colors = 'Purples')

fig_00


# GRAFICOS 5 --------------------------------------------------------------
#radar chart

# USA ---------------------------------------------------------------------
datos_usa <- read_csv("./Datos/Transformados/data_grafico_usa.csv")

#vamos a hacer del 1 año y del ultimo, si ns interesan otros los podemos crear tambien
usa_70<-datos_usa[year(datos_usa$Date) == 1970,]

usa_70<-usa_70%>%
  group_by(variable, Country)%>%
  summarise(variable, mean(valor))
usa_70<-unique(usa_70)
fig <- plot_ly(
  type = 'scatterpolar',
  r = usa_70$`mean(valor)`,
  theta = usa_70$variable,
  fill = 'toself'
)

fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,650)
      )
    ),
    showlegend = F
  )

fig #las variables tienen valor muy diferentes habria que intentar normalizar o algo




# GRAFICOS 6 --------------------------------------------------------------
#Grafico de las crisis en EEUU
#en el GDP
str(datos_usa_GDP)
crisis1<-datos_usa_GDP[(year(datos_usa_GDP$Fecha) == 1975)|(year(datos_usa_GDP$Fecha) == 1974),]
crisis2<-datos_usa_GDP[(year(datos_usa_GDP$Fecha) >= 1979)&(year(datos_usa_GDP$Fecha) <= 1982),]
crisis3<-datos_usa_GDP[(year(datos_usa_GDP$Fecha) == 1987),]
crisis4<-datos_usa_GDP[(year(datos_usa_GDP$Fecha) == 1991),]
crisis5<-datos_usa_GDP[(year(datos_usa_GDP$Fecha) == 2007),]


crisis<-rbind(crisis1, crisis2)
crisis<-rbind(crisis, crisis3)
crisis<-rbind(crisis, crisis4)
crisis<-rbind(crisis, crisis5)

str(crisis)
crisis<-crisis%>%
  group_by(year(Fecha))%>%
  summarise(Fecha, PIB = mean(PIB))
crisis<-crisis[c(5, 9, 24, 28, 32, 36),]
crisis$anotacion <-c("1ª crisis",
                     "inicio 2ª crisis", "fin de 2ª crisis",
                     "3ª crisis",
                     "4ª crisis",
                     "5ª crisis")
library(plotly)
fig <- plot_ly(datos_usa_GDP, type = 'scatter', mode = 'lines', line = list(color = '#53868B'), fill = "tonexty", fillcolor='cadetblue2')%>%
  add_trace(x = ~Fecha, y = ~PIB)%>%
  layout(showlegend = F, title='Evolución Temporal del GDP en USA',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig <- fig %>%
  add_annotations(
    x = crisis$Fecha,
    y = crisis$PIB,
    text = crisis$anotacion,
    xref = "x",
    yref = "y",
    showarrow = TRUE
  )
fig

saveWidget(fig, file="./Graficos/crisis_USA_GDP.html", selfcontained = T,libdir="lib")


#en el CPI
str(datos_usa_CPI)
crisis1<-datos_usa_CPI[(year(datos_usa_CPI$Fecha) == 1975)|(year(datos_usa_CPI$Fecha) == 1974),]
crisis2<-datos_usa_CPI[(year(datos_usa_CPI$Fecha) >= 1979)&(year(datos_usa_CPI$Fecha) <= 1982),]
crisis3<-datos_usa_CPI[(year(datos_usa_CPI$Fecha) == 1987),]
crisis4<-datos_usa_CPI[(year(datos_usa_CPI$Fecha) == 1991),]
crisis5<-datos_usa_CPI[(year(datos_usa_CPI$Fecha) == 2008),]


crisis<-rbind(crisis1, crisis2)
crisis<-rbind(crisis, crisis3)
crisis<-rbind(crisis, crisis4)
crisis<-rbind(crisis, crisis5)

str(crisis)
crisis<-crisis%>%
  group_by(year(Fecha))%>%
  summarise(Fecha, CPI = mean(CPI))

crisis<-crisis[c(6, 18, 30, 42, 54, 66, 78, 90, 108),]
crisis$anotacion <-c("1ª crisis", "fin de 1ª crisis",
                     "inicio 2ª crisis", "", "", "fin de 2ª crisis",
                     "3ª crisis",
                     "4ª crisis",
                     "5ª crisis")
crisis<-crisis[c(1,3,6,7,8,9),]
fig <- plot_ly(datos_usa_CPI, type = 'scatter', mode = 'lines',
               line = list(color = '#53868B'), fill = "tonexty", fillcolor='cadetblue2')%>%
  add_trace(x = ~Fecha, y = ~CPI)%>%
  layout(showlegend = F, title='Evolución Temporal del CPI en USA',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig <- fig %>%
  add_annotations(
    x = crisis$Fecha,
    y = crisis$CPI,
    text = crisis$anotacion,
    xref = "x",
    yref = "y",
    showarrow = TRUE
  )
fig

saveWidget(fig, file="./Graficos/crisis_USA_CPI.html", selfcontained = T,libdir="lib")


#grafico evolución de francia UR
datos_fra_UR<- read_csv("./Datos/Transformados/data_unemployment_francia.csv")
datos_fra_UR
colnames(datos_fra_UR)<-c(" ","Date", "Unemployment_Rate", "Code")

crisis2008<-datos_fra_UR[(year(datos_fra_UR$Date) == 2008),]
crisis2008<-crisis2008[4,]
crisis2008$anotacion <- "crisis de 2008"
crisis2020<-datos_fra_UR[(year(datos_fra_UR$Date) == 2020),]
crisis2020<-crisis2020[3,]
crisis2020$anotacion <- "crisis del COVID"
crisis<-rbind(crisis2008, crisis2020)
fig <- plot_ly(datos_fra_UR, type = 'scatter', mode = 'lines',
               line = list(color = '#53868B'), fill = "tonexty", fillcolor='cadetblue2')%>%
  add_trace(x = ~Date, y = ~Unemployment_Rate)%>%
  layout(showlegend = F, title='Evolución Temporal del Unemployment Rate en Francia',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig <- fig %>%
  add_annotations(
    x = crisis$Date,
    y = crisis$Unemployment_Rate,
    text = crisis$anotacion,
    xref = "x",
    yref = "y",
    showarrow = TRUE
  )
fig

saveWidget(fig, file="./Graficos/crisis_Fra_UR.html", selfcontained = T,libdir="lib")

#grafico de PIB en Francia
datos_fra_GDP<- read_csv("./Datos/Transformados/data_pib_francia.csv")
colnames(datos_fra_GDP)<-c(" ","Date", "GDP", "Code")

crisis2008<-datos_fra_GDP[(year(datos_fra_GDP$Date) == 2008),]
crisis2008<-crisis2008[4,]
crisis2008$anotacion <- "crisis de 2008"
crisis2018<-datos_fra_GDP[(year(datos_fra_GDP$Date) == 2018),]
crisis2018<-crisis2018[4,]
crisis2018$anotacion <- "movimiento chalecos amarillos"
crisis2020<-datos_fra_GDP[(year(datos_fra_GDP$Date) == 2020),]
crisis2020<-crisis2020[2,]
crisis2020$anotacion <- "crisis del COVID"
crisis<-rbind(crisis2008, crisis2018)
crisis<-rbind(crisis, crisis2020)
fig <- plot_ly(datos_fra_GDP, type = 'scatter', mode = 'lines',
               line = list(color = '#53868B'), fill = "tonexty", fillcolor='cadetblue2')%>%
  add_trace(x = ~Date, y = ~GDP)%>%
  layout(showlegend = F, title='Evolución Temporal del PIB en Francia',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=5, label="5y", step="year", stepmode="backward"),
                          list(count=10, label="10y", step="year", stepmode="backward"),
                          list(count=25, label="25y", step="year", stepmode="todate"),
                          list(count=50, label="50y", step="year", stepmode="backward"),
                          list(step="all")))))

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)

fig <- fig %>%
  add_annotations(
    x = crisis$Date,
    y = crisis$GDP,
    text = crisis$anotacion,
    xref = "x",
    yref = "y",
    showarrow = TRUE
  )
fig

saveWidget(fig, file="./Graficos/crisis_Fra_GDP.html", selfcontained = T,libdir="lib")

#GRAFICOS DE EVOLUCION SENCILLOS PARA EL ANALISIS DEL MODELO
#desempleo francia 
desem_fra<- read_csv("./Datos/Transformados/data_unemployment_francia.csv")
colnames(desem_fra)
ggplot(desem_fra,aes(Fecha,Unemployment))+
  geom_line()+
  labs(title = 'Evolucion del Unemployment Rate de Francia')+
  theme_classic()
boxplot(desem_fra$Unemployment)

#CPI FRANCIA
cpi_fra<- read_csv("./Datos/Transformados/data_ipc_francia.csv")
colnames(cpi_fra)
ggplot(cpi_fra,aes(Fecha,CPI))+
  geom_line()+
  labs(title = 'Evolucion del IPC de Francia')+
  theme_classic()
boxplot(cpi_fra$CPI)

#GDP FRANCIA
pib_fra<- read_csv("./Datos/Transformados/data_pib_francia.csv")
ggplot(pib_fra,aes(Fecha,PIB))+
  geom_line()+
  labs(title = 'Evolucion del PIB de Francia')+
  theme_classic()
boxplot(pib_fra$PIB)

#MONEY_SUPPLY
MS_fra<- read_csv("./Datos/Transformados/data_money_supply_francia.csv")
ggplot(MS_fra,aes(Fecha,Money_supply_billion_currency_units))+
  geom_line()+
  labs(title = 'Evolucion del Money Supply de Francia')+
  theme_classic()
boxplot(MS_fra$Money_supply_billion_currency_units)

#Stock_market_index FRANCIA
SM_fra<- read_csv("./Datos/Transformados/data_stock_market_index_francia.csv")
ggplot(SM_fra,aes(Fecha,Stock_market_index))+
  geom_line()+
  labs(title = 'Evolucion del Stock Market Index de Francia')+
  theme_classic()
boxplot(SM_fra$Stock_market_index)


#desempleo usa 
desem_usa<- read_csv("./Datos/Transformados/data_unemployment_usa.csv")
colnames(desem_usa)
ggplot(desem_usa,aes(Fecha,Unemployment))+
  geom_line()+
  labs(title = 'Evolucion del Unemployment Rate de Usa')+
  theme_classic()
boxplot(desem_usa$Unemployment)

#CPI FRANCIA
cpi_usa<- read_csv("./Datos/Transformados/data_ipc_usa.csv")
colnames(cpi_usa)
ggplot(cpi_usa,aes(Fecha,CPI))+
  geom_line()+
  labs(title = 'Evolucion del IPC de USA')+
  theme_classic()
boxplot(cpi_usa$CPI)

#GDP FRANCIA
pib_usa<- read_csv("./Datos/Transformados/data_pib_usa.csv")
ggplot(pib_usa,aes(Fecha,PIB))+
  geom_line()+
  labs(title = 'Evolucion del PIB de USA')+
  theme_classic()
boxplot(pib_usa$PIB)

#MONEY_SUPPLY
MS_usa<- read_csv("./Datos/Transformados/data_money_supply_usa.csv")
ggplot(MS_usa,aes(Fecha,Money_supply_billion_currency_units))+
  geom_line()+
  labs(title = 'Evolucion del Money Supply de USA')+
  theme_classic()
boxplot(MS_usa$Money_supply_billion_currency_units)

#Stock_market_index FRANCIA
SM_usa<- read_csv("./Datos/Transformados/data_stock_market_index_usa.csv")
ggplot(SM_usa,aes(Fecha,Stock_market_index))+
  geom_line()+
  labs(title = 'Evolucion del Stock Market Index de USA')+
  theme_classic()
boxplot(SM_usa$Stock_market_index)


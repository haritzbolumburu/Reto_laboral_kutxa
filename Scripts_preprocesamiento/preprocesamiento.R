
# Preprocesamiento (Antes de subirlo a influx) ----------------------------

# Para la correcta lectura en InfluxDB, se va a modificar todas las fechas al 
# formato correcto (YYYY-MM-DDTHH:MM:SS)

# Fijamos directorio
path_datos_originales <- "Datos/Originales"

# Primer dataframe - UNEMPLOYMENT_FRANCE.CSV
desempleo_francia_original <- "unemployment_france.csv"
desempleo_francia <- read.csv(file.path(path_datos_originales,desempleo_francia_original), sep = ";")
str(desempleo_francia)

colnames(desempleo_francia)<-c("Period", "Unemployment_Rate")
str(desempleo_francia)
vis_miss(desempleo_francia)

#Los missings es lo que se pretende predecir por lo que lo eliminaremos
desempleo_francia<-na.omit(desempleo_francia)
desempleo_francia$trimestre<-substr(desempleo_francia$Period, start = 6, stop = 8 )
desempleo_francia$year<-substr(desempleo_francia$Period, start = 1, stop = 4) 
desempleo_francia$mes<-ifelse(desempleo_francia$trimestre=="T1", "03", 
                              ifelse(desempleo_francia$trimestre=="T2", "06",
                                     ifelse (desempleo_francia$trimestre=="T3", "09", "12")))

desempleo_francia$date<-paste(desempleo_francia$year,"-",desempleo_francia$mes,"-01")
desempleo_francia$date<-gsub(" ", "", desempleo_francia$date )
desempleo_francia$date<-strptime(desempleo_francia$date,format = "%Y-%m-%d",tz="GMT")
desempleo_francia$date<-as.POSIXct(desempleo_francia$date)
str(desempleo_francia)
desempleo_francia<-desempleo_francia%>%
  select(date, Unemployment_Rate)
desempleo_francia$Country <- "France"

# Segundo dataframe - EXOGENAS_USA
exogenas_usa_original <- "pib_exogenas_usa.csv"
exogenas_usa <- read.csv(file.path(path_datos_originales,exogenas_usa_original), sep = ";")
str(exogenas_usa)
tail(exogenas_usa)
vis_miss(exogenas_usa)
names(exogenas_usa)
exogenas_usa$fecha<-str_c(exogenas_usa$Year,"-",exogenas_usa$Month,"-01")
exogenas_usa$Year<-NULL
exogenas_usa$Month<-NULL
exogenas_usa$fecha <- ymd(exogenas_usa$fecha)
names(exogenas_usa)
colnames(exogenas_usa)<-c("Country" ,"Code","ContinentCode","GDP","Money_supply_billion_currency_units",
                          "Unemployment_rate_percent","Stock_market_index","fecha" )
names(exogenas_usa)

# Eliminamos la columna que esta completamente vacia
exogenas_usa$ContinentCode<-"NA"

# Los ultimos missings que son de los años que pretendemos predecir los eliminamos
exogenas_usa<-exogenas_usa[!((year(exogenas_usa$`fecha`)==2022) & (quarter(exogenas_usa$`fecha`)>=3)),]

#ademas de una de las variables no se disponen datos de 2022, pero como no no interesa eliminar al completo ni las observaciones ni la columna
#le daremos los valores de el ultimo año
exogenas_usa[is.na(exogenas_usa$Stock_market_index),]$Stock_market_index<-158.04333
vis_miss(exogenas_usa)
exogenas_usa<-as.data.frame(exogenas_usa)
colnames(exogenas_usa)
exogenas_usa$fecha<-strptime(exogenas_usa$fecha,format = "%Y-%m-%d",tz="GMT")
exogenas_usa$fecha<-as.POSIXct(exogenas_usa$fecha)
str(exogenas_usa)
exogenas_usa<-exogenas_usa%>%
  select(date = fecha, Country, Code, ContinentCode,GDP, Money_supply_billion_currency_units, Unemployment_rate_percent, Stock_market_index)

pib_usa<-exogenas_usa%>%
  select(date,Country,Code, ContinentCode,GDP)
pib_usa<-na.omit(pib_usa)

exogenas_usa<-exogenas_usa%>%
  select(date ,Country, Code, ContinentCode, Money_supply_billion_currency_units, Unemployment_rate_percent, Stock_market_index)



# Tercer dataframe - IPC_USA
ipc_usa_original <- "ipc_usa.csv"
ipc_usa <- read.csv(file.path(path_datos_originales,ipc_usa_original))
str(ipc_usa)
ipc_usa$date<-strptime(ipc_usa$DATE,format = "%Y-%m-%d",tz="GMT")
ipc_usa$date<-as.POSIXct(ipc_usa$date)
str(ipc_usa)
ipc_usa<-ipc_usa%>%
  select(date, CPI)
ipc_usa$Country<-"USA"
ipc_usa$Code<-"USA"
ipc_usa$ContinentCode<-"NA"

# Cuarto dataframe - PIB-IPC_PAISES
pib_ipc_paises_original <- "pib_ipc_paises_punto2.csv"
pib_ipc_paises  <- read.csv(file.path(path_datos_originales,pib_ipc_paises_original))
str(pib_ipc_paises)
vis_miss(pib_ipc_paises)
colnames(pib_ipc_paises)
pib_ipc_paises$fecha <- str_c(pib_ipc_paises$Year,"-",pib_ipc_paises$Month,"-01")
pib_ipc_paises$Year <- NULL
pib_ipc_paises$Month <- NULL
pib_ipc_paises$fecha <- ymd(pib_ipc_paises$fecha)

# Missings en el continent code - nos damos cuenta de los missings son en canada por lo que crearemos el code AM, para America
pib_ipc_paises$ContinentCode<-as.character(pib_ipc_paises$ContinentCode)
pib_ipc_paises[is.na(pib_ipc_paises$ContinentCode),]$ContinentCode<-"NA"

# Otros missings
vis_miss(pib_ipc_paises)
colnames(pib_ipc_paises)
pib_ipc_paises$date<-strptime(pib_ipc_paises$fecha,format = "%Y-%m-%d",tz="GMT")
pib_ipc_paises$date<-as.POSIXct(pib_ipc_paises$date)


colnames(pib_ipc_paises)<-c("Country","Code", "ContinentCode","GDP","Consumer_Price_Index_CPI","fecha","date")

pib_ipc_paises<-pib_ipc_paises%>%
  select(date, Country, Code, ContinentCode, GDP, Consumer_Price_Index_CPI)


pib_paises<<-pib_ipc_paises%>%
  select(date,Country,Code,ContinentCode,GDP)
pib_paises<-na.omit(pib_paises)

pib_paises$Country<-as.character(pib_paises$Country)

ipc_paises<<-pib_ipc_paises%>%
  select(date,Country,Code,ContinentCode,Consumer_Price_Index_CPI)

colnames(ipc_paises)<-c("date","Country","Code", "ContinentCode","CPI")
ipc_paises<-na.omit(ipc_paises)
ipc_paises$Country<-as.character(ipc_paises$Country)

# Quinto dataframe - EXOGENAS_PAISES
exogenas_paises_original<- "exogenas_paises_punto2.xlsx"
exogenas_paises <- readxl::read_xlsx(file.path(path_datos_originales,exogenas_paises_original))
str(exogenas_paises)
exogenas_paises$`Money supply billion currency units`<-as.numeric(exogenas_paises$`Money supply billion currency units`)
exogenas_paises$`Unemployment rate percent`<-as.numeric(exogenas_paises$`Unemployment rate percent`)
exogenas_paises$`Stock market index`<-as.numeric(exogenas_paises$`Stock market index`)
vis_miss(exogenas_paises)

exogenas_paises$fecha <- str_c(exogenas_paises$Year,"-",exogenas_paises$Month,"-01")
exogenas_paises$Year <- NULL
exogenas_paises$Month <- NULL
exogenas_paises$fecha <- ymd(exogenas_paises$fecha)
colnames(exogenas_paises)<-c("Country", "Code", "ContinentCode" ,"Money_supply_billion_currency_units", "Unemployment_rate_percent", 
                             "Stock_market_index", "fecha")
exogenas_paises$Money_supply_billion_currency_units[is.na(exogenas_paises$Money_supply_billion_currency_units)] <- 0
exogenas_paises$Stock_market_index[is.na(exogenas_paises$Stock_market_index)] <- 0
exogenas_paises$Unemployment_rate_percent<-NULL

vis_miss(exogenas_paises)
exogenas_paises$date<-strptime(exogenas_paises$fecha,format = "%Y-%m-%d",tz="GMT")
exogenas_paises$date<-as.POSIXct(exogenas_paises$date)

colnames(exogenas_paises)
exogenas_paises<-as.data.frame(exogenas_paises)
exogenas_paises<-exogenas_paises%>%
  select(date, Country, Code, ContinentCode, Money_supply_billion_currency_units, Stock_market_index)

# Vamos a eliminar las instancias que corresponde a las predicciones que se van a hacer
exogenas_paises<-exogenas_paises[!((year(exogenas_paises$date)==2022) & (quarter(exogenas_paises$date)>=3)),]


# Revisamos los formatos de todos los dataframes --------------------------

summary(desempleo_francia)
summary(exogenas_usa) 
summary(ipc_usa)
summary(pib_ipc_paises)
summary(exogenas_paises)

# Con todo en el formato que queremos y preprocesado, pasaremos a la subida de datos a InfluxDb
# mediante el script de adaptacion_Influx.R 
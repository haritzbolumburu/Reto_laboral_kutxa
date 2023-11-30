library(influxdbclient)
client<-InfluxDBClient$new(url="http://localhost:8086",
                           token="RWm3_kSr3x08cSDNABt0ZGeH1Hi5l-__8ql2nLgZwSm-neHXvEUx8VGhX-8ps3mi8Mdh-0aK8yXrHgtt4CIUrA==",
                           org="mondragon")
client$health()


#VARIABLES FRANCIA
#PIB
my_query_pib_francia<-'from(bucket: "reto05_morado") 
  |> range(start: 1960-01-01T00:00:00.000Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "GDP")'

my_query_pib_francia<-gsub("[\r\n]", "", my_query_pib_francia) # Esto nos permite ignorar los saltos de linea para realizar el query

data_pib_francia <-client$query(my_query_pib_francia)
data_pib_francia <- do.call(rbind.data.frame, data_pib_francia)
data_pib_francia<-data_pib_francia[,3:5]
names(data_pib_francia)
colnames(data_pib_francia) <- c("Fecha","PIB","Code")

write.csv(data_pib_francia, "./Datos/Transformados/data_pib_francia.csv")
# IPC Francia

my_query_francia_cpi <- 'from(bucket: "reto05_morado")
  |> range(start: 1965-10-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "CPI")'

data_ipc_francia<-gsub("[\r\n]", "", my_query_francia_cpi) # Esto nos permite ignorar los saltos de linea para realizar el query

data_ipc_francia <- client$query(data_ipc_francia)
data_ipc_francia<-do.call(rbind.data.frame, data_ipc_francia)

data_ipc_francia<-data_ipc_francia[,3:5]
names(data_ipc_francia)
colnames(data_ipc_francia) <- c("Fecha","CPI","Code")

write.csv(data_ipc_francia, "./Datos/Transformados/data_ipc_francia.csv")


#Money_supply_billion_currency_units Francia

my_query_francia_money_supply <- 'from(bucket: "reto05_morado")
  |> range(start: 1965-10-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "Money_supply_billion_currency_units")'

data_my_query_francia_money_supply<-gsub("[\r\n]", "", my_query_francia_money_supply) # Esto nos permite ignorar los saltos de linea para realizar el query

data_my_query_francia_money_supply <- client$query(data_my_query_francia_money_supply)
data_my_query_francia_money_supply<-do.call(rbind.data.frame, data_my_query_francia_money_supply)

data_my_query_francia_money_supply<-data_my_query_francia_money_supply[,3:5]
names(data_my_query_francia_money_supply)
colnames(data_my_query_francia_money_supply) <- c("Fecha","Money_supply_billion_currency_units","Code")

write.csv(data_my_query_francia_money_supply, "./Datos/Transformados/data_money_supply_francia.csv")


#Money_supply_billion_currency_units Estados Unidos

my_query_eeuu_money_supply <- 'from(bucket: "reto05_morado")
  |> range(start: 1965-10-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "Money_supply_billion_currency_units")'

data_query_eeuu_money_supply<-gsub("[\r\n]", "", my_query_eeuu_money_supply) # Esto nos permite ignorar los saltos de linea para realizar el query

data_query_eeuu_money_supply <- client$query(data_query_eeuu_money_supply)
data_query_eeuu_money_supply<-do.call(rbind.data.frame, data_query_eeuu_money_supply)

data_query_eeuu_money_supply<-data_query_eeuu_money_supply[,3:5]
names(data_query_eeuu_money_supply)
colnames(data_query_eeuu_money_supply) <- c("Fecha","Money_supply_billion_currency_units","Code")

write.csv(data_query_eeuu_money_supply, "./Datos/Transformados/data_money_supply_usa.csv")

#Stock_market_index Estados Unidos

my_query_eeuu_stockmarket<- 'from(bucket: "reto05_morado")
  |> range(start: 1965-10-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "Stock_market_index")'

data_query_eeuu_stockmarket<-gsub("[\r\n]", "", my_query_eeuu_stockmarket) # Esto nos permite ignorar los saltos de linea para realizar el query

data_query_eeuu_stockmarket <- client$query(data_query_eeuu_stockmarket)
data_query_eeuu_stockmarket<-do.call(rbind.data.frame, data_query_eeuu_stockmarket)

data_query_eeuu_stockmarket<-data_query_eeuu_stockmarket[,3:5]
names(data_query_eeuu_stockmarket)
colnames(data_query_eeuu_stockmarket) <- c("Fecha","Stock_market_index","Code")

write.csv(data_query_eeuu_stockmarket, "./Datos/Transformados/data_stock_market_index_usa.csv")


#Stock_market_index Francia

my_query_francia_stockmarket<- 'from(bucket: "reto05_morado")
  |> range(start: 1965-10-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "Stock_market_index")'

data_query_francia_stockmarket<-gsub("[\r\n]", "", my_query_francia_stockmarket) # Esto nos permite ignorar los saltos de linea para realizar el query

data_query_francia_stockmarket <- client$query(data_query_francia_stockmarket)
data_query_francia_stockmarket<-do.call(rbind.data.frame, data_query_francia_stockmarket)

data_query_francia_stockmarket<-data_query_francia_stockmarket[,3:5]
names(data_query_francia_stockmarket)
colnames(data_query_francia_stockmarket) <- c("Fecha","Stock_market_index","Code")

write.csv(data_query_francia_stockmarket, "./Datos/Transformados/data_stock_market_index_francia.csv")



# PIB Estados Unidos

my_query_pib_usa <- 'from(bucket: "reto05_morado")
  |> range(start: 1960-09-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "GDP")'

my_query_pib_usa<-gsub("[\r\n]", "", my_query_pib_usa) # Esto nos permite ignorar los saltos de linea para realizar el query

data_pib_usa<-client$query(my_query_pib_usa)
data_pib_usa<-do.call(rbind.data.frame, data_pib_usa)

data_pib_usa<-data_pib_usa[,3:5]
names(data_pib_usa)
colnames(data_pib_usa) <- c("Fecha","PIB","Code")

write.csv(data_pib_usa, "./Datos/Transformados/data_pib_usa.csv")


#  IPC Estados unidos

my_query_usa_cpi <- 'from(bucket: "reto05_morado")
  |> range(start: 1960-09-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "CPI")'

data_ipc_usa<-gsub("[\r\n]", "", my_query_usa_cpi) # Esto nos permite ignorar los saltos de linea para realizar el query

data_ipc_usa<-client$query(data_ipc_usa)
data_ipc_usa<-do.call(rbind.data.frame, data_ipc_usa)

data_ipc_usa<-data_ipc_usa[,3:5]
names(data_ipc_usa)
colnames(data_ipc_usa) <- c("Fecha","CPI","Code")

write.csv(data_ipc_usa, "./Datos/Transformados/data_ipc_usa.csv")


# Unemployment USA

my_query_usa_unemployment <- 'from(bucket: "reto05_morado")
  |> range(start: 1960-09-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "Unemployment_rate_percent")'

data_unemployment_usa<-gsub("[\r\n]", "", my_query_usa_unemployment) # Esto nos permite ignorar los saltos de linea para realizar el query

data_unemployment_usa<-client$query(data_unemployment_usa)
data_unemployment_usa<-do.call(rbind.data.frame, data_unemployment_usa)

data_unemployment_usa<-data_unemployment_usa[,3:5]
names(data_unemployment_usa)
colnames(data_unemployment_usa) <- c("Fecha","Unemployment","Code")

write.csv(data_unemployment_usa, "./Datos/Transformados/data_unemployment_usa.csv")


# Unemployment USA

my_query_francia_unemployment <- 'from(bucket: "reto05_morado")
  |> range(start: 1960-09-10T17:10:41.00Z, stop: 2022-10-10T16:10:41.814Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "Unemployment_rate_percent")'

data_unemployment_francia<-gsub("[\r\n]", "", my_query_francia_unemployment) # Esto nos permite ignorar los saltos de linea para realizar el query

data_unemployment_francia<-client$query(data_unemployment_francia)
data_unemployment_francia<-do.call(rbind.data.frame, data_unemployment_francia)

data_unemployment_francia<-data_unemployment_francia[,3:5]
names(data_unemployment_francia)
colnames(data_unemployment_francia) <- c("Fecha","Unemployment","Code")

write.csv(data_unemployment_francia, "./Datos/Transformados/data_unemployment_francia.csv")


#QUERYS PARA GRAFICOS
#grafico relación PIB y CPI USA
my_query<-'from(bucket: "reto05_morado")
  |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "GDP")'



my_query<-gsub("[\r\n]", "", my_query)

data2<-client$query(my_query)
datos_usa_GDP<-do.call(rbind.data.frame, data2)
datos_usa_GDP<-datos_usa_GDP%>%
  select(time, "_measurement", "_value")
colnames(datos_usa_GDP)<-c("Date", "Country", "GDP")

my_query<-'from(bucket: "reto05_morado")
  |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")
  |> filter(fn: (r) => r["Code"] == "USA")
  |> filter(fn: (r) => r["ContinentCode"] == "NA")
  |> filter(fn: (r) => r["_field"] == "CPI")'

my_query<-gsub("[\r\n]", "", my_query)

data2<-client$query(my_query)
datos_usa_CPI<-do.call(rbind.data.frame, data2)
datos_usa_CPI<-datos_usa_CPI%>%
  select(time, "_measurement", "_value")
colnames(datos_usa_CPI)<-c("Date", "Country", "CPI")
quarter(datos_usa_CPI$Date)
#unimos las dos tablas GDP y CPI
nrow(datos_usa_CPI)
nrow(datos_usa_GDP)

datos_usa_CPI_tri<-datos_usa_CPI[month(datos_usa_CPI$Date)==3 | month(datos_usa_CPI$Date)==6 | month(datos_usa_CPI$Date)==9 | month(datos_usa_CPI$Date)==12,]

datos_usa<-cbind(datos_usa_GDP, datos_usa_CPI_tri$CPI)
colnames(datos_usa)<-c("Date", "Country", "GDP", "CPI")

write.csv(datos_usa, "./Datos/Transformados/data_grafico_GDP_IPC_usa.csv")

#grafico relación PIB y CPI USA
my_query<-'from(bucket: "reto05_morado")
  |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "GDP")'

my_query<-gsub("[\r\n]", "", my_query)

data2<-client$query(my_query)
datos_fra_GDP<-do.call(rbind.data.frame, data2)
datos_fra_GDP<-datos_fra_GDP%>%
  select(time, "_measurement", "_value")
colnames(datos_fra_GDP)<-c("Date", "Country", "GDP")

my_query<-'from(bucket: "reto05_morado")
  |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["Code"] == "FRA")
  |> filter(fn: (r) => r["ContinentCode"] == "EU")
  |> filter(fn: (r) => r["_field"] == "CPI")'

my_query<-gsub("[\r\n]", "", my_query)

data2<-client$query(my_query)
datos_fra_CPI<-do.call(rbind.data.frame, data2)
datos_fra_CPI<-datos_fra_CPI%>%
  select(time, "_measurement", "_value")
colnames(datos_fra_CPI)<-c("Date", "Country", "CPI")

#unimos las dos tablas GDP y CPI
datos_fra_CPI_tri<-datos_fra_CPI[month(datos_fra_CPI$Date)==3 | month(datos_fra_CPI$Date)==6 | month(datos_fra_CPI$Date)==9 | month(datos_fra_CPI$Date)==12,]
datos_fra_GDP<-datos_fra_GDP[1:105,]
datos_fra<-cbind(datos_fra_GDP, datos_fra_CPI_tri$CPI)
colnames(datos_fra)<-c("Date", "Country", "GDP", "CPI")
str(datos_fra)
write.csv(datos_fra, "./Datos/Transformados/data_grafico_GDP_IPC_fra.csv")


#grafico mapa de calor GDP por año
my_query<-'from(bucket: "reto05_morado")
  |> range(start: 1996-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_field"] == "GDP")'

my_query<-gsub("[\r\n]", "", my_query)

data2<-client$query(my_query)
datos_GDP<-do.call(rbind.data.frame, data2)
datos_GDP<-datos_GDP%>%
  select(time, "_measurement", Code ,"_value")
colnames(datos_GDP)<-c("Date", "Country", "Code","GDP")
str(datos_GDP)
datos_GDP$year<-year(datos_GDP$Date)
datos_GDP_year<-datos_GDP%>%
  group_by(Country, Code,year)%>%
  summarise(Country, year, Code, mean(GDP))
datos_GDP_year<-unique(datos_GDP_year)
colnames(datos_GDP_year)<-c("Country", "Code", "Year", "GDP")
datos_GDP_year
write.csv(datos_GDP_year, "./Datos/Transformados/data_grafico_GDP_year.csv")

#graficos radar chart USA
my_query<-'from(bucket: "reto05_morado")
  |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "USA")'

my_query<-gsub("[\r\n]", "", my_query)

data2<-client$query(my_query)
datos_usa<-do.call(rbind.data.frame, data2)
datos_usa<-datos_usa%>%
  select(time, "_measurement", "_field" ,"_value")
colnames(datos_usa)<-c("Date", "Country",  "variable", "valor")
write.csv(datos_usa, "./Datos/Transformados/data_grafico_usa.csv")

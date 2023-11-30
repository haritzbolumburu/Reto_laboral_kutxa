client<-InfluxDBClient$new(url="http://localhost:8086",
                           token="kC4qah78kZy_XjfcBejEMzGu8-bdpboj03uQQB1lTD8aBF57lqVrFoLskSVSoFYylzcOZijcPWOj-f-tgHERMA==",
                           org="mondragon")
client$health()
#desempleo francia 
desem_fra<-'from(bucket: "prueba_reto") |> range(start: 1970-01-01T00:00:00.000000Z, stop: 2022-10-17T00:00:00.000000Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["_field"] == "Unemployment_rate_percent")'

desem_fra<-gsub("[\r\n]", "", desem_fra) # Esto nos permite ignorar los saltos de linea para realizar el query

desem_fra<-client$query(desem_fra)
df_desem_fra<-do.call(rbind.data.frame, desem_fra)

summary(df_desem_fra)
df_desem_fra<-df_desem_fra%>%
  select('time','_value','_measurement')
colnames(df_desem_fra)<-c('Date', 'Unemployment_rate_percent','counrty')
ggplot(df_desem_fra,aes(Date,Unemployment_rate_percent))+
  geom_line()+
 # title(main = 'PORCENTAJE DE DESEMPLEO EN FRANCIA')+
  theme_classic()
boxplot(df_desem_fra$Unemployment_rate_percent)

#CPI FRANCIA
CPI_fra<-'from(bucket: "prueba_reto") |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["_field"] == "CPI")'
  

CPI_fra<-gsub("[\r\n]", "", CPI_fra) # Esto nos permite ignorar los saltos de linea para realizar el query

CPI_fra<-client$query(query_CPI_fra)
df_CPI_fra<-do.call(rbind.data.frame, CPI_fra)

summary(df_CPI_fra)
df_CPI_fra<-df_CPI_fra%>%
  select("time", "_measurement", "Code", "ContinentCode", "_value")
colnames(df_CPI_fra)<-c("Date", "Country", "Code", "ContinentCode", "CPI")
ggplot(df_CPI_fra,aes(Date,CPI))+
  geom_line()+
  # title(main = 'PORCENTAJE DE DESEMPLEO EN FRANCIA')+
  theme_classic()
boxplot(df_CPI_fra$CPI)

#GDP FRANCIA
GDP_FRA<-'from(bucket: "prueba_reto") |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
  |> filter(fn: (r) => r["_measurement"] == "France")
  |> filter(fn: (r) => r["_field"] == "GDP")'
GDP_fra<-gsub("[\r\n]", "", GDP_FRA) # Esto nos permite ignorar los saltos de linea para realizar el query

GDP_fra<-client$query(GDP_fra)
df_GDP_fra<-do.call(rbind.data.frame, GDP_fra)

summary(df_GDP_fra)
df_GDP_fra<-df_GDP_fra%>%
  select("time", "_measurement", "Code", "ContinentCode", "_value")
colnames(df_GDP_fra)<-c("Date", "Country", "Code", "ContinentCode", "GDP")
ggplot(df_GDP_fra,aes(Date,GDP))+
  geom_line()+
  # title(main = 'PORCENTAJE DE DESEMPLEO EN FRANCIA')+
  theme_classic()
boxplot(df_GDP_fra$GDP)

#MONEY_SUPPLY
MONEY_FRA<-'from(bucket: "prueba_reto") |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
   |> filter(fn: (r) => r["_measurement"] == "France")
   |> filter(fn: (r) => r["_field"] == "Money_supply_billion_currency_units")'
MONEY_FRA<-gsub("[\r\n]", "", MONEY_FRA) # Esto nos permite ignorar los saltos de linea para realizar el query

MONEY_fra<-client$query(MONEY_FRA)
df_MONEY_fra<-do.call(rbind.data.frame, MONEY_fra)

summary(df_MONEY_fra)
df_MONEY_fra<-df_MONEY_fra%>%
  select("time", "_measurement", "Code", "ContinentCode", "_value")
colnames(df_MONEY_fra)<-c("Date", "Country", "Code", "ContinentCode", "Money_supply_billion_currency_units")
str(df_MONEY_fra)
df_MONEY_fra$Money_supply_billion_currency_units<-as.integer(df_MONEY_fra$Money_supply_billion_currency_units)
ggplot(df_MONEY_fra,aes(Date,Money_supply_billion_currency_units))+
  geom_line()+
  # title(main = 'PORCENTAJE DE DESEMPLEO EN FRANCIA')+
  theme_classic()
boxplot(df_MONEY_fra$Money_supply_billion_currency_units)

#Stock_market_index FRANCIA
Stock_market_index_FRA<-'from(bucket: "prueba_reto") |> range(start: 1970-01-01T00:00:00.001Z, stop: 2022-06-01T00:00:00.001Z)
   |> filter(fn: (r) => r["_measurement"] == "France")
   |> filter(fn: (r) => r["_field"] == "Stock_market_index")'
Stock_market_index_FRA<-gsub("[\r\n]", "", Stock_market_index_FRA) # Esto nos permite ignorar los saltos de linea para realizar el query

Stock_market_index_FRA<-client$query(Stock_market_index_FRA)
Stock_market_index_FRA<-do.call(rbind.data.frame, Stock_market_index_FRA)

summary(Stock_market_index_FRA)
Stock_market_index_FRA<-Stock_market_index_FRA%>%
  select("time", "_measurement", "Code", "ContinentCode", "_value")
colnames(Stock_market_index_FRA)<-c("Date", "Country", "Code", "ContinentCode", "Stock_market_index")
str(Stock_market_index_FRA)
Stock_market_index_FRA$Stock_market_index<-as.integer(Stock_market_index_FRA$Stock_market_index)
ggplot(Stock_market_index_FRA,aes(Date,Stock_market_index))+
  geom_line()+
  # title(main = 'PORCENTAJE DE DESEMPLEO EN FRANCIA')+
  theme_classic()
boxplot(Stock_market_index_FRA$Stock_market_index)

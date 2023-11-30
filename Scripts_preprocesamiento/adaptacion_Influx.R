
# Creacion del bucket donde se subiran los datos --------------------------

# Introduce tu token
INFLUX_TOKEN="zVhe8A56Rk8AvWvMmjRRHB_XqLpAPPqH3-Xxjoc84K9DhKanqlc6iGf9Zm77yEBanmDSCEoLjw1T4OHOEgzJDg=="

Influx_Authorization_Header <- paste ("Token", INFLUX_TOKEN)


INFLUX_URL="http://localhost:8086/api/v2/buckets"

# Introduce tu ID de organizacion 
INFLUX_ORG_ID='{
                "name": "reto05_morado",
                "orgID": "c51cf7ea5f263c12"
                }'

response <- POST (
  INFLUX_URL,
  body = INFLUX_ORG_ID,
  accept("application/csv"),
  content_type("application/json"),
  add_headers(Authorization = Influx_Authorization_Header)
)

# Verifica que todo ha ido correctamente
response$status_code


# Creacion del cliente ----------------------------------------------------

client<-InfluxDBClient$new(url="http://localhost:8086",
                           token=INFLUX_TOKEN,
                           org="mondragon")
client$health()


# Subida de datos a influx ------------------------------------------------

# Primer DF (Tasa de desempleo - Francia)

desempleo_francia$Code="FRA"
desempleo_francia$ContinentCode="EU"
length(desempleo_francia)
colnames(desempleo_francia)<-c("date", "Unemployment_rate_percent", "Country","Code","ContinentCode")
response <- client$write(desempleo_francia, bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("Unemployment_rate_percent"),
                         timeCol = "date")

# Segundo DF (Variables exogenas - USA)

response <- client$write(exogenas_usa[1:300,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c( "Unemployment_rate_percent", "Money_supply_billion_currency_units",
                                       "Stock_market_index"),
                         timeCol = "date")


response <- client$write(exogenas_usa[300:630,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c( "Unemployment_rate_percent", "Money_supply_billion_currency_units",
                                       "Stock_market_index"),
                         timeCol = "date")

response <- client$write(pib_usa, bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("GDP"),
                         timeCol = "date")

# Tercer DF (IPC - USA)

nrow(ipc_usa)
colnames(ipc_usa)
response <- client$write(ipc_usa[1:500,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("CPI"),
                         timeCol = "date")
response <- client$write(ipc_usa[501:631,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("CPI"),
                         timeCol = "date")

# Cuarto DF (PIB - Paises)

response <- client$write(pib_paises[1:500,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         fieldCols = c("GDP"),
                         tagCols = c("Code", "ContinentCode"),
                         timeCol = "date")
response <- client$write(pib_paises[501:741,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         fieldCols = c("GDP"),
                         tagCols = c("Code", "ContinentCode"),
                         timeCol = "date")
response <- client$write(ipc_paises[1:500,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         fieldCols = c( "CPI"),
                         tagCols = c("Code", "ContinentCode"),
                         timeCol = "date")
response <- client$write(ipc_paises[501:741,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         fieldCols = c( "CPI"),
                         tagCols = c("Code", "ContinentCode"),
                         timeCol = "date")

# Quinto DF (Variables exogenas - Paises)
nrow(exogenas_paises)
str(exogenas_paises)
exogenas_paises<-as.data.frame(exogenas_paises)
exogenas_paises$Country<-as.character(exogenas_paises$Country)
response <- client$write(exogenas_paises[1:500,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("Money_supply_billion_currency_units", "Stock_market_index"),
                         timeCol = "date")
response <- client$write(exogenas_paises[501:1000,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("Money_supply_billion_currency_units", "Stock_market_index"),
                         timeCol = "date")
response <- client$write(exogenas_paises[1001:1500,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("Money_supply_billion_currency_units", "Stock_market_index"),
                         timeCol = "date")
response <- client$write(exogenas_paises[1501:2000,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("Money_supply_billion_currency_units", "Stock_market_index"),
                         timeCol = "date")
response <- client$write(exogenas_paises[2001:2226,], bucket = "reto05_morado", precision="s",
                         measurementCol = "Country",
                         tagCols = c("Code", "ContinentCode"),
                         fieldCols = c("Money_supply_billion_currency_units", "Stock_market_index"),
                         timeCol = "date")


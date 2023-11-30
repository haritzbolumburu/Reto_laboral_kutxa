#Todas las variables
library(readr)
library(visdat)
library(fpp2)
library(forecast)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(lmtest)
library(stringr)
#Carga de todas las variables
data_ipc_usa<-read.csv("./Datos/Transformados/data_ipc_usa.csv")
data_ipc_francia<-read.csv("./Datos/Transformados/data_ipc_francia.csv")
data_money_supply_francia<-read.csv("./Datos/Transformados/data_money_supply_francia.csv")
data_money_supply_usa<-read.csv("./Datos/Transformados/data_money_supply_usa.csv")
data_pib_francia<-read.csv("./Datos/Transformados/data_pib_francia.csv")
data_pib_usa<-read.csv("./Datos/Transformados/data_pib_usa.csv")
data_stock_market_index_francia<-read.csv("./Datos/Transformados/data_stock_market_index_francia.csv")
data_stock_market_index_usa<-read.csv("./Datos/Transformados/data_stock_market_index_usa.csv")
data_unemployment_francia<-read.csv("./Datos/Transformados/data_unemployment_francia.csv")
data_unemployment_usa<-read.csv("./Datos/Transformados/data_unemployment_usa.csv")


#CPI USA~AGRUPACION
head(data_ipc_usa$Fecha)
str(data_ipc_usa)


data_ipc_usa<-data_ipc_usa[,2:3]
head(data_ipc_usa)
data_ipc_usa$Fecha <- as.yearqtr(data_ipc_usa$Fecha,           # Convert dates to quarterly
                                 format = "%Y-%m-%d")
head(data_ipc_usa)
data_ipc_usa<-data_ipc_usa %>%
  group_by(year(data_ipc_usa$Fecha),quarter(data_ipc_usa$Fecha)) %>%
  summarise(CPI=sum(CPI)/3)
head(data_ipc_usa)
colnames(data_ipc_usa)<-c("Year",'Quarter','CPI')
data_ipc_usa$Fecha<-str_c(data_ipc_usa$Year,'-',data_ipc_usa$Quarter,'-01')
data_ipc_usa<-data_ipc_usa[,3:4]
head(data_ipc_usa)
tail(data_ipc_usa)
data_ipc_usa$Fecha<-as.Date(data_ipc_usa$Fecha)
str(data_ipc_usa)
data_ipc_usa <-data_ipc_usa %>%
  filter(Fecha>='1996-01-01')
head(data_ipc_usa)


#CPI FRANCIA
head(data_ipc_francia$Fecha)
str(data_ipc_francia)
data_ipc_francia<-data_ipc_francia[,2:3]
data_ipc_francia$Fecha <- as.yearqtr(data_ipc_francia$Fecha,           # Convert dates to quarterly
                                     format = "%Y-%m-%d")
head(data_ipc_francia)
data_ipc_francia<-data_ipc_francia %>%
  group_by(year(data_ipc_francia$Fecha),quarter(data_ipc_francia$Fecha)) %>%
  summarise(CPI=sum(CPI)/3)
head(data_ipc_francia)
colnames(data_ipc_francia)<-c("Year",'Quarter','CPI')
data_ipc_francia$Fecha<-str_c(data_ipc_francia$Year,'-',data_ipc_francia$Quarter,'-01')
data_ipc_francia<-data_ipc_francia[,3:4]
head(data_ipc_francia)
data_ipc_francia$Fecha<-as.Date(data_ipc_francia$Fecha)
str(data_ipc_francia)
data_ipc_francia <-data_ipc_francia %>%
  filter(Fecha>='1996-01-01')
head(data_ipc_francia)

#money_supply_francia
head(data_money_supply_francia$Fecha)
names(data_money_supply_francia)
data_money_supply_francia<-data_money_supply_francia[,2:3]
data_money_supply_francia$Fecha <- as.yearqtr(data_money_supply_francia$Fecha,           # Convert dates to quarterly
                                              format = "%Y-%m-%d")
head(data_money_supply_francia)
data_money_supply_francia<-data_money_supply_francia %>%
  group_by(year(data_money_supply_francia$Fecha),quarter(data_money_supply_francia$Fecha)) %>%
  summarise(Money_supply_billion_currency_units=sum(Money_supply_billion_currency_units)/3)
head(data_money_supply_francia)
colnames(data_money_supply_francia)<-c("Year",'Quarter','Money_supply_billion_currency_units')
data_money_supply_francia$Fecha<-str_c(data_money_supply_francia$Year,'-',data_money_supply_francia$Quarter,'-01')
data_money_supply_francia<-data_money_supply_francia[,3:4]
head(data_money_supply_francia)
data_money_supply_francia$Fecha<-as.Date(data_money_supply_francia$Fecha)
str(data_money_supply_francia)
data_money_supply_francia <-data_money_supply_francia %>%
  filter(Fecha>='1996-01-01')
head(data_money_supply_francia)
#data_money_supply_usa
head(data_money_supply_usa$Fecha)
names(data_money_supply_usa)
data_money_supply_usa<-data_money_supply_usa[,2:3]
data_money_supply_usa$Fecha <- as.yearqtr(data_money_supply_usa$Fecha,           # Convert dates to quarterly
                                          format = "%Y-%m-%d")
head(data_money_supply_usa)
data_money_supply_usa<-data_money_supply_usa %>%
  group_by(year(data_money_supply_usa$Fecha),quarter(data_money_supply_usa$Fecha)) %>%
  summarise(Money_supply_billion_currency_units=sum(Money_supply_billion_currency_units)/3)
head(data_money_supply_usa)
colnames(data_money_supply_usa)<-c("Year",'Quarter','Money_supply_billion_currency_units')
data_money_supply_usa$Fecha<-str_c(data_money_supply_usa$Year,'-',data_money_supply_usa$Quarter,'-01')
data_money_supply_usa<-data_money_supply_usa[,3:4]
head(data_money_supply_usa)
data_money_supply_usa$Fecha<-as.Date(data_money_supply_usa$Fecha)
str(data_money_supply_usa)
data_money_supply_usa <-data_money_supply_usa %>%
  filter(Fecha>='1996-01-01')
head(data_money_supply_usa)

#data_pib_francia
head(data_pib_francia$Fecha)
str(data_pib_francia)
data_pib_francia<-data_pib_francia[,2:3]
head(data_pib_francia)



#data_pib_usa
head(data_pib_usa)
str(data_pib_usa)
data_pib_usa<-data_pib_usa[,2:3]
head(data_pib_usa)
data_pib_usa$Fecha<-as.Date(data_pib_usa$Fecha)
str(data_pib_usa)
data_pib_usa <-data_pib_usa %>%
  filter(Fecha>='1996-01-01')
head(data_pib_usa)

#data_stock_market_index_francia
head(data_stock_market_index_francia$Fecha)
names(data_stock_market_index_francia)
data_stock_market_index_francia<-data_stock_market_index_francia[,2:3]
names(data_stock_market_index_francia)
data_stock_market_index_francia$Fecha <- as.yearqtr(data_stock_market_index_francia$Fecha,           # Convert dates to quarterly
                                                    format = "%Y-%m-%d")
head(data_stock_market_index_francia)
data_stock_market_index_francia<-data_stock_market_index_francia %>%
  group_by(year(data_stock_market_index_francia$Fecha),quarter(data_stock_market_index_francia$Fecha)) %>%
  summarise(Stock_market_index=sum(Stock_market_index)/3)
head(data_stock_market_index_francia)
colnames(data_stock_market_index_francia)<-c("Year",'Quarter','Money_supply_billion_currency_units')
head(data_stock_market_index_francia)
data_stock_market_index_francia$Fecha<-str_c(data_stock_market_index_francia$Year,'-',data_stock_market_index_francia$Quarter,'-01')
data_stock_market_index_francia<-data_stock_market_index_francia[,3:4]
head(data_stock_market_index_francia)
data_stock_market_index_francia$Fecha<-as.Date(data_stock_market_index_francia$Fecha)
str(data_stock_market_index_francia)
data_stock_market_index_francia <-data_stock_market_index_francia %>%
  filter(Fecha>='1996-01-01')
head(data_stock_market_index_francia)


#data_data_stock_market_index_usa_usa
head(data_stock_market_index_usa$Fecha)
names(data_stock_market_index_usa)
data_stock_market_index_usa<-data_stock_market_index_usa[,2:3]
names(data_stock_market_index_usa)
data_stock_market_index_usa$Fecha <- as.yearqtr(data_stock_market_index_usa$Fecha,           # Convert dates to quarterly
                                                format = "%Y-%m-%d")
head(data_stock_market_index_usa)
data_stock_market_index_usa<-data_stock_market_index_usa %>%
  group_by(year(data_stock_market_index_usa$Fecha),quarter(data_stock_market_index_usa$Fecha)) %>%
  summarise(Stock_market_index=sum(Stock_market_index)/3)
head(data_stock_market_index_usa)
colnames(data_stock_market_index_usa)<-c("Year",'Quarter','Money_supply_billion_currency_units')
head(data_stock_market_index_usa)
data_stock_market_index_usa$Fecha<-str_c(data_stock_market_index_usa$Year,'-',data_stock_market_index_usa$Quarter,'-01')
data_stock_market_index_usa<-data_stock_market_index_usa[,3:4]
head(data_stock_market_index_usa)
data_stock_market_index_usa$Fecha<-as.Date(data_stock_market_index_usa$Fecha)
str(data_stock_market_index_usa)
data_stock_market_index_usa <-data_stock_market_index_usa %>%
  filter(Fecha>='1996-01-01')
head(data_stock_market_index_francia)






#data_unemployment_francia
head(data_unemployment_francia)
str(data_unemployment_francia)
data_unemployment_francia<-data_unemployment_francia[,2:3]
head(data_unemployment_francia)
data_unemployment_francia$Fecha<-as.Date(data_unemployment_francia$Fecha)
str(data_unemployment_francia)
data_unemployment_francia <-data_unemployment_francia %>%
  filter(Fecha>='1996-01-01')
head(data_stock_market_index_francia)



#data_unemployment_usa
head(data_unemployment_usa$Fecha)
names(data_unemployment_usa)
data_unemployment_usa<-data_unemployment_usa[,2:3]
names(data_unemployment_usa)
data_unemployment_usa$Fecha <- as.yearqtr(data_unemployment_usa$Fecha,           # Convert dates to quarterly
                                          format = "%Y-%m-%d")
head(data_unemployment_usa)
data_unemployment_usa<-data_unemployment_usa %>%
  group_by(year(data_unemployment_usa$Fecha),quarter(data_unemployment_usa$Fecha)) %>%
  summarise(Unemployment=sum(Unemployment)/3)
head(data_unemployment_usa)
colnames(data_unemployment_usa)<-c("Year",'Quarter','Unemployment')
head(data_unemployment_usa)
data_unemployment_usa$Fecha<-str_c(data_unemployment_usa$Year,'-',data_unemployment_usa$Quarter,'-01')
data_unemployment_usa<-data_unemployment_usa[,3:4]
head(data_unemployment_usa)

data_unemployment_usa$Fecha<-as.Date(data_unemployment_usa$Fecha)
str(data_unemployment_usa)
data_unemployment_usa <-data_unemployment_usa %>%
  filter(Fecha>='1996-01-01')
head(data_stock_market_index_francia)


#Creacion de series temporales
##creacion de series temporales
#IPC USA
ts_cpi_usa <-
  ts(data_ipc_usa$CPI,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))
autoplot(ts_cpi_usa)
#IPC FRANCIA

ts_ipc_francia <-
  ts(data_ipc_francia$CPI,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))

autoplot(ts_ipc_francia)

#MONEY SUPPLY FRANCIA
ts_money_supply_francia <-
  ts(data_money_supply_francia$Money_supply_billion_currency_units,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))

autoplot(ts_money_supply_francia)

#MONEY SUPPLY USA
ts_money_supply_usa <-
  ts(data_money_supply_usa$Money_supply_billion_currency_units,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))
autoplot(ts_money_supply_usa)

#PIB FRANCIA
ts_pib_francia <-
  ts(data_pib_francia$PIB,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))  #Fin de la serie
autoplot(ts_pib_francia)


#PIB USA

ts_pib_usa<-
  ts(data_pib_usa$PIB,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))  #Fin de la serie
tail(ts_pib_usa)
autoplot(ts_pib_usa)

#STOCK MARKET INDEX FRANCIA
ts_stock_market_index_francia<-
  ts(data_stock_market_index_francia$Money_supply_billion_currency_units,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))
autoplot(ts_stock_market_index_francia)
#STOCK MARKET INDEX USA
ts_stock_market_index_usa <-
  ts(data_stock_market_index_usa$Money_supply_billion_currency_units,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))
autoplot(ts_stock_market_index_usa)


#unemployment_francia
ts_unemployment_francia<-
  ts(data_unemployment_francia$Unemployment,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))  #Fin de la serie
tail(ts_unemployment_francia)
autoplot(ts_unemployment_francia)

#data_unemployment_usa
ts_unemployment_usa <-
  ts(data_unemployment_usa$Unemployment,        
     frequency = 4,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2022))
autoplot(ts_unemployment_usa)

#
autoplot(ts_cpi_usa)

#Granger_Test~Coonocer que variables afectan a cuales

#pib usa
library(vars)
grangertest(ts_money_supply_usa~ts_pib_usa,order=3) #5.508e-06 ***
grangertest(ts_cpi_usa~ts_pib_usa,order=3) #7.124e-11 ***
grangertest(ts_stock_market_index_usa~ts_pib_usa,order=3) #7.124e-11 ***
grangertest(ts_unemployment_usa~ts_pib_usa,order=9) #7.124e-11 ***
grangertest(ts_pib_francia~ts_pib_usa,order=9) #7.124e-11 ***
#ipc usa
grangertest(ts_money_supply_usa~ts_cpi_usa,order=3) #5.508e-06 ***
grangertest(ts_pib_usa~ts_cpi_usa,order=4) #7.124e-11 ***
grangertest(ts_stock_market_index_usa~ts_cpi_usa,order=8) #7.124e-11 ***
grangertest(ts_unemployment_usa~ts_cpi_usa,order=9) #7.124e-11 ***
grangertest(ts_pib_francia~ts_cpi_usa,order=9) #7.124e-11 ***

#Modelo usa pib
#Modelo usa pib
modelo_usa_pib<-cbind(ts_pib_usa,ts_cpi_usa,ts_stock_market_index_usa,ts_unemployment_usa)
#numero de diff
ndiffs(modelo_usa_pib)
library(vars)
df_train_usa_pib<- window(modelo_usa_pib, start=c(1970,1), end=c(2018,4))
df_test_usa_pib<-window(modelo_usa_pib, start=c(2019,1), end=c(2020,1))
#lag selected
varorder <- vars::VARselect(y = df_train_usa_pib, lag.max = 7, type = "none")
print(varorder$selection)
#creacion del modelo
var_modelo_usa_pib<-VAR(y=df_train_usa_pib,p=4,type="const")#p es el lag seleccionado
summary(var_modelo_usa_pib)#prediccion
predicciones_usa<-  forecast(var_modelo_usa_pib,h=4)
#accuracy
predicciones_ts_cpi_usa<-diffinv(predicciones_usa$forecast$ts_cpi_usa$mean,differences = 1,lag = 4)
predicciones_ts_pib_usa<-diffinv(predicciones_usa$forecast$ts_pib_usa$mean,differences = 1,lag = 4)
predicciones_ts_unemployment__usa<-diffinv(predicciones_usa$forecast$ts_unemployment_usa$mean,differences = 1,lag = 4)
accuracy(df_test_usa_pib[,1],predicciones_ts_pib_usa)
accuracy(df_test_usa_pib[,2],predicciones_ts_cpi_usa)
accuracy(df_test_usa_pib[,4],predicciones_ts_unemployment__usa)


if (Box.test(predicciones_usa$forecast$ts_pib_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


if (Box.test(predicciones_usa$forecast$ts_cpi_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

#prediccion pib 2022
modelo_final_pib<-VAR(y=modelo_usa_pib,p=4,type="const")#p es el lag seleccionado
summary(modelo_final_pib)#prediccion
predicciones_usa<-  forecast(modelo_final_pib,h=4)
predicciones_usa$forecast$ts_pib_usa$mean
predicciones_graph_usa<-  forecast(modelo_final_pib,h=4) %>% autoplot()
predicciones_graph_usa

if (Box.test(predicciones_usa$forecast$ts_pib_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

#Modelo usa cpi
modelo_usa_cpi<-cbind(ts_cpi_usa,ts_pib_usa,ts_money_supply_usa)
#numero de diff
df_train_usa_cpi<- window(modelo_usa_cpi, start=c(1970,1), end=c(2018,4))
df_test_usa_cpi<-window(modelo_usa_cpi, start=c(2019,1), end=c(2020,1))
modelo_usa_cpi <- diff(as.matrix(modelo_usa_cpi))

#lag selected
varorder <- vars::VARselect(y = df_train_usa_cpi, lag.max = 7, type = "none")
print(varorder$selection)
#creacion del modelo
var_modelo_usa_cpi<-VAR(y=df_train_usa_cpi,p=7,type="const")#p es el lag seleccionado
summary(var_modelo_usa_cpi)#prediccion
predicciones_usa_cpi<-  forecast(var_modelo_usa_cpi,h=4)
#accuracy
predicciones_ts_cpi_usa<-diffinv(predicciones_usa_cpi$forecast$ts_cpi_usa$mean,differences = 1,lag = 4)
predicciones_ts_pib_usa<-diffinv(predicciones_usa_cpi$forecast$ts_pib_usa$mean,differences = 1,lag = 4)
accuracy(df_test_usa_cpi[,2],predicciones_ts_pib_usa)
accuracy(df_test_usa_cpi[,1],predicciones_ts_cpi_usa)

if (Box.test(predicciones_usa_cpi$forecast$ts_cpi_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

if (Box.test(predicciones_usa_cpi$forecast$ts_pib_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

#prediccion pib 2022
modelo_final_pib<-VAR(y=modelo_usa_pib,p=4,type="const")#p es el lag seleccionado
summary(modelo_final_pib)#prediccion
predicciones_usa<-  forecast(modelo_final_pib,h=4)
predicciones_usa$forecast$ts_cpi_usa$mean
predicciones_graph_usa<-  forecast(modelo_final_pib,h=4) %>% autoplot()
predicciones_graph_usa



if (Box.test(predicciones_usa$forecast$ts_cpi_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


#francia modelado
#grangertest
#pib francia
grangertest(ts_unemployment_francia~ts_pib_francia,order=3) #5.508e-06 ***
grangertest(ts_ipc_francia~ts_pib_francia,order=3) #7.124e-11 ***
grangertest(ts_money_supply_francia~ts_pib_francia,order=3) #7.124e-11 ***
grangertest(ts_stock_market_index_francia~ts_pib_francia,order=9) #7.124e-11 ***
grangertest(ts_pib_usa~ts_pib_francia,order=1) #0.03504 *
#ipc francia
grangertest(ts_unemployment_francia~ ts_ipc_francia,order=9) #0.007963 **
grangertest(ts_pib_francia~ts_ipc_francia,order=3) #2.032e-07 ***
grangertest(ts_money_supply_francia~ts_ipc_francia,order=7) #0.0668 .
grangertest(ts_stock_market_index_francia~ts_ipc_francia,order=4) #0.004217 **
grangertest(ts_pib_usa~ts_ipc_francia,order=1) #0.003765 **

#variables seleccionadas
modelo_francia<-cbind(ts_pib_francia,ts_ipc_francia,ts_unemployment_francia,ts_money_supply_usa)
#Modelo francia cpi
modelo_francia_cpi<-cbind(ts_ipc_francia,ts_pib_francia,ts_unemployment_francia,ts_money_supply_usa,
                          ts_stock_market_index_francia)
#numero de diff
ndiffs(modelo_francia_cpi)
library(vars)
df_train_francia_cpi<- window(modelo_francia_cpi, start=c(1970,1), end=c(2018,4))
df_test_francia_cpi<-window(modelo_francia_cpi, start=c(2019,1), end=c(2020,1))
#lag selected
varorder <- vars::VARselect(y = df_train_francia_cpi, lag.max = 7, type = "none")
print(varorder$selection)
#creacion del modelo
var_modelo_francia_cpi<-VAR(y=df_train_francia_cpi,p=5,type="const")#p es el lag seleccionado
summary(var_modelo_francia_cpi)#prediccion
predicciones_francia_cpi<-  forecast(var_modelo_francia_cpi,h=4)
#accuracy
predicciones_ts_cpi_francia<-diffinv(predicciones_francia_cpi$forecast$ts_ipc_francia$mean,differences = 1,lag = 5)
predicciones_ts_pib_francia<-diffinv(predicciones_francia_cpi$forecast$ts_pib_francia$mean,differences = 1,lag=5)
accuracy(df_test_francia_cpi[,1],predicciones_ts_cpi_francia)
accuracy(df_test_francia_cpi[,4],predicciones_ts_pib_francia)


if (Box.test(predicciones_francia_cpi$forecast$ts_ipc_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


if (Box.test(predicciones_francia_cpi$forecast$ts_pib_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

#prediccion cpi 2022
modelo_francia_cpi<-cbind(ts_ipc_francia,ts_pib_francia,ts_unemployment_francia,ts_money_supply_usa,
                          ts_stock_market_index_francia)
modelo_final_cpi_francia<-VAR(y=modelo_francia_cpi,p=4,type="const")#p es el lag seleccionado
summary(modelo_francia_cpi)#prediccion
predicciones_francia_cpi<-  forecast(modelo_final_cpi_francia,h=4)
predicciones_francia_cpi$forecast$ts_ipc_francia$mean
predicciones_francia_cpi_graph<-  forecast(modelo_final_cpi_francia,h=4) %>% autoplot()
predicciones_francia_cpi_graph

if (Box.test(predicciones_francia_cpi$forecast$ts_ipc_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

#Modelo francia pib
modelo_francia_pib<-cbind(ts_pib_francia,ts_unemployment_francia,ts_pib_usa,ts_money_supply_usa,ts_stock_market_index_francia)
#numero de diff
ndiffs(modelo_francia_pib)
library(vars)
df_train_francia_pib<- window(modelo_francia_pib, start=c(1970,1), end=c(2018,4))
df_test_francia_pib<-window(modelo_francia_pib, start=c(2019,1), end=c(2020,1))
#lag selected
varorder <- vars::VARselect(y = df_train_francia_pib, lag.max = 7, type = "none")
print(varorder$selection)
#creacion del modelo
var_modelo_francia_pib<-VAR(y=df_train_francia_pib,p=7,type="const")#p es el lag seleccionado
summary(var_modelo_francia_pib)#prediccion
predicciones_francia_pib<-  forecast(var_modelo_francia_pib,h=4)
#accuracy
predicciones_ts_pib_francia<-diffinv(predicciones_francia_pib$forecast$ts_pib_francia$mean,differences = 1,lag=5)
accuracy(df_test_francia_pib[,1],predicciones_ts_pib_francia)

if (Box.test(predicciones_francia_pib$forecast$ts_pib_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


#prediccion pib 2022
modelo_francia_pib<-cbind(ts_pib_francia,ts_unemployment_francia,ts_pib_usa,ts_money_supply_usa,ts_stock_market_index_francia)
modelo_final_pib_fran<-VAR(y=modelo_francia_pib,p=4,type="const")#p es el lag seleccionado
summary(modelo_final_pib_fran)#prediccion
predicciones_francia_pib<-  forecast(modelo_final_pib_fran,h=4)
predicciones_francia_pib$forecast$ts_pib_francia$mean
predicciones_francia_pib_graph<-  forecast(modelo_final_pib_fran,h=4) %>% autoplot()
predicciones_francia_pib_graph


if (Box.test(predicciones_francia_pib$forecast$ts_pib_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

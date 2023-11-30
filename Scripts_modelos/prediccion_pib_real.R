
path_datos_originales <- "Datos/Originales"
pib_real<- "pib_real.xls"
pib_real <- readxl::read_xls(file.path(path_datos_originales,pib_real))


# ESTADOS UNIDOS 

pib_real_usa<-pib_real%>%filter(`Country Code`=="USA")

pib_real_usa<-data.frame(pib_real_usa[,-c(1,3,4)])

colnames(pib_real_usa)<-c("country",1:61)

pib_real_usa <- data.frame(t(pib_real_usa[-1]))

colnames(pib_real_usa) <- "pib"

fecha=1960:2021

pib_real_usa<-cbind(pib_real_usa, fecha)

str(pib_real_usa)

pib_real_usa$fecha<-as.character(pib_real_usa$fecha)

pib_real_usa$fecha<-strptime(pib_real_usa$fecha, format = "%Y")

data_pib_real_usa<-ts(pib_real_usa$pib, start = 1960, end = 2021, frequency = 1)


train_pib_real <- window(data_pib_real_usa, start=1960, end=2018)

train_original_pib_real<-train_pib_real

test_pib_real<-window(data_pib_real_usa, start=2019, end=2021)

# GRAFICAR LOS DATOS

autoplot(train_pib_real)+labs(title = "PIB real de USA (1960-2021)",
                              y="PIB real", x="Fecha")


gglagplot(train_pib_real)

ggtsdisplay(train_pib_real)

# test_pib_usa DE ESTACIONARIEDAD

adftest_pib_usa <- adf.test(train_pib_real)
if (adftest_pib_usa$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(train_pib_real)%>%round(3))
lambda<-BoxCox.lambda(train_pib_real)%>%round(3)

autoplot(BoxCox(train_pib_real, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda=0,554")

# el mas cercano es 0,5 -> raiz cuadrada

train_pib_real<-sqrt(train_pib_real)


# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA

ndiffs(train_pib_real)

train_pib_real2<-diff(train_pib_real,lag=1, differences = 1)

ggtsdisplay(train_pib_real2)




# MODELO AUTOARIMA

modeloAUTOARIMA<-auto.arima(train_pib_real2, seasonal = F)

auto.arima(train_pib_real2, seasonal = F) # arima(0,0,1)

checkresiduals(modeloAUTOARIMA)


# test_pib_usa DE RESIDUOS

if (Box.test(modeloAUTOARIMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloAUTOARIMA,h=3)$mean %>% diffinv(lag=1,xi=tail(train_pib_real,1))

predicciones<-predicciones^2

autoplot(ts.union(train_original_pib_real,  ts(predicciones,start=2018,frequency=1)))

acuracy_AUTOARIMA<-accuracy(test_pib_real,ts(predicciones[2:4],start=2019,frequency=1, end=2021))
modeloAUTOARIMA$aicc



#FRANCIA


pib_real_francia<-pib_real%>%filter(`Country Code`=="FRA")

pib_real_francia<-data.frame(pib_real_francia[,-c(1,3,4)])

colnames(pib_real_francia)<-c("country",1:61)

pib_real_francia <- data.frame(t(pib_real_francia[-1]))

colnames(pib_real_francia) <- "pib"

fecha=1960:2021

pib_real_francia<-cbind(pib_real_francia, fecha)

str(pib_real_francia)

pib_real_francia$fecha<-as.character(pib_real_francia$fecha)

pib_real_francia$fecha<-strptime(pib_real_francia$fecha, format = "%Y")

data_pib_real_francia<-ts(pib_real_francia$pib, start = 1960, end = 2021, frequency = 1)


train_pib_real <- window(data_pib_real_francia, start=1960, end=2018)

train_original_pib_real<-train_pib_real

test_pib_real<-window(data_pib_real_francia, start=2019, end=2021)

# GRAFICAR LOS DATOS

autoplot(train_pib_real)+labs(title = "PIB real de Francia (1960,2021",
                              y="PIB real", x="Fecha")

gglagplot(train_pib_real)

ggtsdisplay(train_pib_real)

# test_pib_usa DE ESTACIONARIEDAD

adftest_pib_usa <- adf.test(train_pib_real)
if (adftest_pib_usa$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(train_pib_real)%>%round(3))
lambda<-BoxCox.lambda(train_pib_real)%>%round(3)

autoplot(BoxCox(train_pib_real, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda=0,828")

# el mas cercano es 0,5 -> raiz cuadrada

train_pib_real<-sqrt(train_pib_real)


# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA

ndiffs(train_pib_real)

train_pib_real2<-diff(train_pib_real,lag=1, differences = 1)
train_pib_real2<-diff(train_pib_real2,lag=1, differences = 1)
ggtsdisplay(train_pib_real2)




# MODELO AUTOARIMA

modeloAUTOARIMA<-auto.arima(train_pib_real2, seasonal = F)

auto.arima(train_pib_real2, seasonal = F) # arima(0,0,2)

checkresiduals(modeloAUTOARIMA)


# test_pib_usa DE RESIDUOS

if (Box.test(modeloAUTOARIMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloAUTOARIMA,h=3)$mean %>% diffinv(lag=1,differences = 2,xi=tail(train_pib_real,2))

predicciones<-predicciones^2

autoplot(ts.union(train_original_pib_real,  ts(predicciones[2:5],start=2018,frequency=1)))

acuracy_AUTOARIMA<-accuracy(test_pib_real,ts(predicciones[3:5],start=2019,frequency=1, end=2021))
modeloAUTOARIMA$aicc



### MODELO VAR

#Carga de todas las variables
data_ipc_usa<-read.csv("./Datos/Transformados/data_ipc_usa.csv")
data_ipc_francia<-read.csv("./Datos/Transformados/data_ipc_francia.csv")
data_money_supply_francia<-read.csv("./Datos/Transformados/data_money_supply_francia.csv")
data_money_supply_usa<-read.csv("./Datos/Transformados/data_money_supply_usa.csv")
data_stock_market_index_francia<-read.csv("./Datos/Transformados/data_stock_market_index_francia.csv")
data_stock_market_index_usa<-read.csv("./Datos/Transformados/data_stock_market_index_usa.csv")
data_unemployment_francia<-read.csv("./Datos/Transformados/data_unemployment_francia.csv")
data_unemployment_usa<-read.csv("./Datos/Transformados/data_unemployment_usa.csv")

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


#CPI USA~AGRUPACION
head(data_ipc_usa$Fecha)
str(data_ipc_usa)


data_ipc_usa<-data_ipc_usa[,2:3]
head(data_ipc_usa)
data_ipc_usa$Fecha <- as.yearqtr(data_ipc_usa$Fecha,           # Convert dates to quarterly
                                 format = "%Y-%m-%d")
head(data_ipc_usa)
data_ipc_usa<-data_ipc_usa %>%
  group_by(year(data_ipc_usa$Fecha)) %>%
  summarise(CPI=mean(CPI))
head(data_ipc_usa)
colnames(data_ipc_usa)<-c("Year",'CPI')
data_ipc_usa$Fecha<-str_c(data_ipc_usa$Year,'-01-01')
data_ipc_usa<-data_ipc_usa[,2:3]
head(data_ipc_usa)
tail(data_ipc_usa)
data_ipc_usa$Fecha<-as.Date(data_ipc_usa$Fecha)
str(data_ipc_usa)
data_ipc_usa <-data_ipc_usa %>%
  filter(Fecha>='1996-01-01')
data_ipc_usa <-data_ipc_usa %>%
  filter(Fecha<='2021-01-01')
head(data_ipc_usa)




#CPI FRANCIA
head(data_ipc_francia$Fecha)
str(data_ipc_francia)
data_ipc_francia<-data_ipc_francia[,2:3]
data_ipc_francia$Fecha <- as.yearqtr(data_ipc_francia$Fecha,           # Convert dates to quarterly
                                     format = "%Y-%m-%d")
head(data_ipc_francia)
data_ipc_francia<-data_ipc_francia %>%
  group_by(year(data_ipc_francia$Fecha)) %>%
  summarise(CPI=mean(CPI))
head(data_ipc_francia)
colnames(data_ipc_francia)<-c("Year",'CPI')
data_ipc_francia$Fecha<-str_c(data_ipc_francia$Year,'-01-01')
data_ipc_francia<-data_ipc_francia[,2:3]
head(data_ipc_francia)
data_ipc_francia$Fecha<-as.Date(data_ipc_francia$Fecha)
str(data_ipc_francia)
data_ipc_francia <-data_ipc_francia %>%
  filter(Fecha>='1996-01-01')
data_ipc_francia <-data_ipc_francia %>%
  filter(Fecha<='2021-01-01')
head(data_ipc_francia)

#money_supply_francia
head(data_money_supply_francia$Fecha)
names(data_money_supply_francia)
data_money_supply_francia<-data_money_supply_francia[,2:3]
data_money_supply_francia$Fecha <- as.yearqtr(data_money_supply_francia$Fecha,           # Convert dates to quarterly
                                              format = "%Y-%m-%d")
head(data_money_supply_francia)
data_money_supply_francia<-data_money_supply_francia %>%
  group_by(year(data_money_supply_francia$Fecha)) %>%
  summarise(Money_supply_billion_currency_units=mean(Money_supply_billion_currency_units))
head(data_money_supply_francia)
colnames(data_money_supply_francia)<-c("Year",'Money_supply_billion_currency_units')
data_money_supply_francia$Fecha<-str_c(data_money_supply_francia$Year,'-01-01')
data_money_supply_francia<-data_money_supply_francia[,2:3]
head(data_money_supply_francia)
data_money_supply_francia$Fecha<-as.Date(data_money_supply_francia$Fecha)
str(data_money_supply_francia)
data_money_supply_francia <-data_money_supply_francia %>%
  filter(Fecha>='1996-01-01')
data_money_supply_francia <-data_money_supply_francia %>%
  filter(Fecha<='2021-01-01')
head(data_money_supply_francia)
#data_money_supply_usa
head(data_money_supply_usa$Fecha)
names(data_money_supply_usa)
data_money_supply_usa<-data_money_supply_usa[,2:3]
data_money_supply_usa$Fecha <- as.yearqtr(data_money_supply_usa$Fecha,           # Convert dates to quarterly
                                          format = "%Y-%m-%d")
head(data_money_supply_usa)
data_money_supply_usa<-data_money_supply_usa %>%
  group_by(year(data_money_supply_usa$Fecha)) %>%
  summarise(Money_supply_billion_currency_units=mean(Money_supply_billion_currency_units))
head(data_money_supply_usa)
colnames(data_money_supply_usa)<-c("Year",'Money_supply_billion_currency_units')
data_money_supply_usa$Fecha<-str_c(data_money_supply_usa$Year,'-01-01')
data_money_supply_usa<-data_money_supply_usa[,2:3]
head(data_money_supply_usa)
data_money_supply_usa$Fecha<-as.Date(data_money_supply_usa$Fecha)
str(data_money_supply_usa)
data_money_supply_usa <-data_money_supply_usa %>%
  filter(Fecha>='1996-01-01')
data_money_supply_usa <-data_money_supply_usa %>%
  filter(Fecha<='2021-01-01')
head(data_money_supply_usa)


#data_stock_market_index_francia
head(data_stock_market_index_francia$Fecha)
names(data_stock_market_index_francia)
data_stock_market_index_francia<-data_stock_market_index_francia[,2:3]
names(data_stock_market_index_francia)
data_stock_market_index_francia$Fecha <- as.yearqtr(data_stock_market_index_francia$Fecha,           # Convert dates to quarterly
                                                    format = "%Y-%m-%d")
head(data_stock_market_index_francia)
data_stock_market_index_francia<-data_stock_market_index_francia %>%
  group_by(year(data_stock_market_index_francia$Fecha)) %>%
  summarise(Stock_market_index=mean(Stock_market_index))
head(data_stock_market_index_francia)
colnames(data_stock_market_index_francia)<-c("Year",'stock_market_index')
head(data_stock_market_index_francia)
data_stock_market_index_francia$Fecha<-str_c(data_stock_market_index_francia$Year,'-01-01')
data_stock_market_index_francia<-data_stock_market_index_francia[,2:3]
head(data_stock_market_index_francia)
data_stock_market_index_francia$Fecha<-as.Date(data_stock_market_index_francia$Fecha)
str(data_stock_market_index_francia)
data_stock_market_index_francia <-data_stock_market_index_francia %>%
  filter(Fecha>='1996-01-01')
data_stock_market_index_francia <-data_stock_market_index_francia %>%
  filter(Fecha<='2021-01-01')
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
  group_by(year(data_stock_market_index_usa$Fecha)) %>%
  summarise(Stock_market_index=mean(Stock_market_index))
head(data_stock_market_index_usa)
colnames(data_stock_market_index_usa)<-c("Year",'stock_market_index')
head(data_stock_market_index_usa)
data_stock_market_index_usa$Fecha<-str_c(data_stock_market_index_usa$Year,'-01-01')
data_stock_market_index_usa<-data_stock_market_index_usa[,2:3]
head(data_stock_market_index_usa)
data_stock_market_index_usa$Fecha<-as.Date(data_stock_market_index_usa$Fecha)
str(data_stock_market_index_usa)
data_stock_market_index_usa <-data_stock_market_index_usa %>%
  filter(Fecha>='1996-01-01')
data_stock_market_index_usa <-data_stock_market_index_usa %>%
  filter(Fecha<='2021-01-01')
head(data_stock_market_index_francia)






#data_unemployment_francia
head(data_unemployment_francia)
str(data_unemployment_francia)
data_unemployment_francia<-data_unemployment_francia[,2:3]
head(data_unemployment_francia)
data_unemployment_francia$Fecha<-as.Date(data_unemployment_francia$Fecha)
head(data_unemployment_francia)
data_unemployment_francia<-data_unemployment_francia %>%
  group_by(year(data_unemployment_francia$Fecha)) %>%
  summarise(Unemployment=mean(Unemployment))
colnames(data_unemployment_francia)<-c("Year", "Unemployment")
data_unemployment_francia$Fecha<-str_c(data_unemployment_francia$Year,'-01-01')
str(data_unemployment_francia)
data_unemployment_francia<-data_unemployment_francia[,2:3]
data_unemployment_francia <-data_unemployment_francia %>%
  filter(Fecha>='1996-01-01')
data_unemployment_francia <-data_unemployment_francia %>%
  filter(Fecha<='2021-01-01')
head(data_unemployment_francia)


#data_unemployment_usa
head(data_unemployment_usa$Fecha)
names(data_unemployment_usa)
data_unemployment_usa<-data_unemployment_usa[,2:3]
names(data_unemployment_usa)
data_unemployment_usa$Fecha <- as.yearqtr(data_unemployment_usa$Fecha,           # Convert dates to quarterly
                                          format = "%Y-%m-%d")
head(data_unemployment_usa)
data_unemployment_usa<-data_unemployment_usa %>%
  group_by(year(data_unemployment_usa$Fecha)) %>%
  summarise(Unemployment=mean(Unemployment))
head(data_unemployment_usa)
colnames(data_unemployment_usa)<-c("Year",'Unemployment')
head(data_unemployment_usa)
data_unemployment_usa$Fecha<-str_c(data_unemployment_usa$Year,'-01-01')
data_unemployment_usa<-data_unemployment_usa[,2:3]
head(data_unemployment_usa)

data_unemployment_usa$Fecha<-as.Date(data_unemployment_usa$Fecha)
str(data_unemployment_usa)
data_unemployment_usa <-data_unemployment_usa %>%
  filter(Fecha>='1996-01-01')
data_unemployment_usa <-data_unemployment_usa %>%
  filter(Fecha<='2021-01-01')
head(data_unemployment_usa)


dim(data_unemployment_usa)
head(data_unemployment_usa)
dim(data_unemployment_francia)
head(data_unemployment_francia)
dim(data_stock_market_index_usa)
head(data_stock_market_index_usa)
dim(data_stock_market_index_francia)
head(data_stock_market_index_usa)
dim(data_ipc_usa)
head(data_ipc_usa)
dim(data_ipc_francia)
head(data_ipc_francia)
dim(data_money_supply_usa)
head(data_money_supply_usa)
dim(data_money_supply_francia)
head(data_money_supply_francia)
length(data_pib_real_usa)
length(data_pib_real_francia)

##creacion de series temporales
#IPC USA
ts_ipc_usa <-
  ts(data_ipc_usa$CPI,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))
autoplot(ts_ipc_usa)
#IPC FRANCIA

ts_ipc_francia <-
  ts(data_ipc_francia$CPI,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))

autoplot(ts_ipc_francia)

#MONEY SUPPLY FRANCIA
ts_money_supply_francia <-
  ts(data_money_supply_francia$Money_supply_billion_currency_units,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))

autoplot(ts_money_supply_francia)

#MONEY SUPPLY USA
ts_money_supply_usa <-
  ts(data_money_supply_usa$Money_supply_billion_currency_units,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))
autoplot(ts_money_supply_usa)

#PIB FRANCIA
ts_pib_francia <-
  window(data_pib_real_francia,        
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))  #Fin de la serie
autoplot(ts_pib_francia)


#PIB USA

ts_pib_usa<-
  window(data_pib_real_usa,        
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))  #Fin de la serie
autoplot(ts_pib_usa)

#STOCK MARKET INDEX FRANCIA
ts_stock_market_index_francia<-
  ts(data_stock_market_index_francia$stock_market_index,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))
autoplot(ts_stock_market_index_francia)
#STOCK MARKET INDEX USA
ts_stock_market_index_usa <-
  ts(data_stock_market_index_usa$stock_market_index,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))
autoplot(ts_stock_market_index_usa)


#unemployment_francia
ts_unemployment_francia<-
  ts(data_unemployment_francia$Unemployment,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))  #Fin de la serie
autoplot(ts_unemployment_francia)

#data_unemployment_usa
ts_unemployment_usa <-
  ts(data_unemployment_usa$Unemployment,        
     frequency = 1,    
     start=c(1996), #Inicio de la serie como vector c(Fecha, month)
     end=c(2021))
autoplot(ts_unemployment_usa)


#Granger_Test~Coonocer que variables afectan a cuales



length(ts_unemployment_usa)
length(ts_unemployment_francia)
length(ts_stock_market_index_usa)
length(ts_stock_market_index_francia)
length(ts_ipc_usa)
length(ts_ipc_francia)
length(ts_money_supply_usa)
length(ts_money_supply_francia)
length(ts_pib_usa)
length(ts_pib_francia)

#PIB USA
library(vars)
grangertest(ts_money_supply_usa~ts_pib_usa) 
grangertest(ts_ipc_usa~ts_pib_usa) #
grangertest(ts_stock_market_index_usa~ts_pib_usa) #
grangertest(ts_unemployment_usa~ts_pib_usa)

grangertest(ts_money_supply_francia~ts_pib_usa) 
grangertest(ts_ipc_francia~ts_pib_usa) #
grangertest(ts_stock_market_index_francia~ts_pib_usa) #
grangertest(ts_unemployment_francia~ts_pib_usa)
#IPC USA




#MODELO USA PIB

modelo_usa_pib<-cbind(ts_pib_usa,ts_ipc_usa)
#numero de diff
ndiffs(modelo_usa_pib)
library(vars)
df_train_usa_pib<- window(modelo_usa_pib, start=c(1996), end=c(2018))
df_test_usa_pib<-window(modelo_usa_pib, start=c(2019), end=c(2021))
#lag selected
varorder <- vars::VARselect(y = df_train_usa_pib, lag.max = 7, type = "none")
print(varorder$selection)
#creacion del modelo
var_modelo_usa_pib<-VAR(y=df_train_usa_pib,p=4,type="const")#p es el lag seleccionado
predicciones_usa<-  forecast(var_modelo_usa_pib,h=3)
#accuracy
predicciones_ts_cpi_usa<-predicciones_usa$forecast$ts_ipc_usa$mean
predicciones_ts_pib_usa<-predicciones_usa$forecast$ts_pib_usa$mean
accuracy(df_test_usa_pib[,1],predicciones_ts_pib_usa)


if (Box.test(predicciones_usa$forecast$ts_pib_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

# PREDICCIONES PIB USA
modelo_usa_pib_final<-VAR(y=modelo_usa_pib,p=4,type="const")#p es el lag seleccionado
predicciones_usa<-  forecast(modelo_usa_pib_final,h=3)
predicciones_usa$forecast$ts_pib_usa$mean
predicciones_graph_usa<-  forecast(modelo_usa_pib_final,h=3) %>% autoplot()
predicciones_graph_usa

if (Box.test(predicciones_usa$forecast$ts_pib_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

a<-c(2.033858e+13, 1.995338e+13, 2.008342e+13, 2.034828e+13 )

ts.plot(ts.union(ts_pib_usa,ts(a, start = 2021, frequency = 1)), lty = c(1,1),  col=c("red",91),gpars=list(xlab="Fecha", ylab="PIB"))
legend("topleft", bty="y", lty=c(1,1), col=c("red",91),
       legend=c("Realidad","Prediccion"))
title('Proyeccion PIB real USA ')



#PIB FRANCIA
library(vars)
grangertest(ts_money_supply_francia~ts_pib_francia) #
grangertest(ts_ipc_francia~ts_pib_francia)  #
grangertest(ts_stock_market_index_francia~ts_pib_francia) 
grangertest(ts_unemployment_francia~ts_pib_francia)

grangertest(ts_money_supply_usa~ts_pib_francia) #
grangertest(ts_ipc_usa~ts_pib_francia) 
grangertest(ts_stock_market_index_usa~ts_pib_francia) 
grangertest(ts_unemployment_usa~ts_pib_francia) #



#MODELO francia PIB

modelo_francia_pib<-cbind(ts_pib_francia,ts_ipc_francia)
#numero de diff
ndiffs(modelo_francia_pib)
library(vars)
df_train_francia_pib<- window(modelo_francia_pib, start=c(1996), end=c(2018))
df_test_francia_pib<-window(modelo_francia_pib, start=c(2019), end=c(2021))
#lag selected
varorder <- vars::VARselect(y = df_train_francia_pib, lag.max = 7, type = "none")
print(varorder$selection)
#creacion del modelo
var_modelo_francia_pib<-VAR(y=df_train_francia_pib,p=6,type="const")#p es el lag seleccionado
predicciones_francia<-  forecast(var_modelo_francia_pib,h=3)
#accuracy
predicciones_ts_cpi_francia<-predicciones_francia$forecast$ts_ipc_francia$mean
predicciones_ts_pib_francia<-predicciones_francia$forecast$ts_pib_francia$mean
accuracy(df_test_francia_pib[,1],predicciones_ts_pib_francia)

if (Box.test(predicciones_francia$forecast$ts_pib_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

# PREDICCIONES PIB Y IPC FRANCIA
modelo_francia_pib_final<-VAR(y=modelo_francia_pib,p=6,type="const")#p es el lag seleccionado
predicciones_francia<-  forecast(modelo_francia_pib_final,h=3)
predicciones_francia$forecast$ts_pib_francia$mean
predicciones_graph_francia<-  forecast(modelo_francia_pib_final,h=3) %>% autoplot()
predicciones_graph_francia


if (Box.test(predicciones_francia$forecast$ts_pib_francia$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


a<-c(2.579165e+12, 2.328800e+12, 2.817216e+12, 2.280712e+12 )

ts.plot(ts.union(ts_pib_francia,ts(a, start = 2021, frequency = 1)), lty = c(1,1),  col=c("blue",91),gpars=list(xlab="Fecha", ylab="PIB"))
legend("topleft", bty="y", lty=c(1,1), col=c("blue",91),
       legend=c("Realidad","Prediccion"))
title('Proyeccion PIB real Francia ')




# PREDICCION data_pib_usa USA AR-MA-ARIMA ------------------------------------------

data_pib_usa <- read_csv("./Datos/Transformados/data_pib_usa.csv")

# Separar en train y test_pib_usa

ts_pib_usa <- data_pib_usa[,2:3]

ts_pib_usa <- ts(ts_pib_usa[,2],frequency= 4, start = c(1970,1), end = c(2022,2))
graf<-autoplot(ts_pib_usa)+ labs(title = "PIB anual de USA (1970-2022)",x='Fecha', y='PIB')
ggsave("ev_PIB_usa.png", graf, path = "./Graficos")

train_pib_usa <- window(ts_pib_usa, start=c(1970,1), end=c(2018,4))

train_original_pib_usa<-train_pib_usa

test_pib_usa<-window(ts_pib_usa, start=c(2019,1), end=c(2019,4))

# GRAFICAR LOS DATOS

autoplot(train_pib_usa)

gglagplot(train_pib_usa)

ggtsdisplay(train_pib_usa)

# test_pib_usa DE ESTACIONARIEDAD

adftest_pib_usa <- adf.test(train_pib_usa)
if (adftest_pib_usa$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(train_pib_usa)%>%round(3))
lambda<-BoxCox.lambda(train_pib_usa)%>%round(3)

autoplot(BoxCox(train_pib_usa, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda=0,365")

# el mas cercano es 0,5 -> raiz cuadrada

train_pib_usa<-sqrt(train_pib_usa)

# QUITAMOS LA COMPONENTE ESTACIONAL

train_pib_usa_decom<-decompose(train_pib_usa)

plot(train_pib_usa_decom)

train_pib_usa<-train_pib_usa-train_pib_usa_decom$seasonal

ggtsdisplay(train_pib_usa)

# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA

ndiffs(train_pib_usa)

train_pib_usa2<-diff(train_pib_usa,lag=4, differences = 1)

ggtsdisplay(train_pib_usa2)

autoplot(train_pib_usa2)+ labs(title = "Serie trasnformada del PIB en Usa",x='Fecha', y='PIB')


# MODELO AUTOARIMA

modeloAUTOARIMA<-auto.arima(train_pib_usa2, seasonal = F)

auto.arima(train_pib_usa2, seasonal = F) # arima(1,0,1)

checkresiduals(modeloAUTOARIMA)


# test_pib_usa DE RESIDUOS

if (Box.test(modeloAUTOARIMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloAUTOARIMA,h=4)$mean %>% diffinv(lag=4,xi=tail(train_pib_usa,4))

predicciones<-(predicciones+train_pib_usa_decom$seasonal[1:8])^2

autoplot(ts.union(train_original_pib_usa,  ts(predicciones[4:8],start=c(2018,4),frequency=4)))

acuracy_AUTOARIMA<-accuracy(test_pib_usa,ts(predicciones[5:8],start=c(2019,1),frequency=4, end=c(2019,4)))


# MODELO AR


fit100<-Arima(train_pib_usa2, order = c(1,0,0))
fit200<-Arima(train_pib_usa2, order = c(2,0,0))
fit300<-Arima(train_pib_usa2, order = c(3,0,0))
fit110<-Arima(train_pib_usa2, order = c(1,1,0))
fit210<-Arima(train_pib_usa2, order = c(2,1,0))
fit310<-Arima(train_pib_usa2, order = c(3,1,0))
fit120<-Arima(train_pib_usa2, order = c(1,2,0))
fit220<-Arima(train_pib_usa2, order = c(2,2,0))
fit320<-Arima(train_pib_usa2, order = c(3,2,0))

bestAR<-which.min(c(
  fit100$aicc,fit200$aicc, fit300$aicc, fit110$aicc, fit210$aicc,fit310$aicc,
  fit120$aicc,fit220$aicc, fit320$aicc
))

bestAR # 3-> (3,0,0) -> AR(3)

modeloAR<-Arima(train_pib_usa2, order=c(3,0,0))

checkresiduals(modeloAR)

# test_pib_usa DE RESIDUOS

if (Box.test(modeloAR$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


predicciones<-  forecast(modeloAR,h=4)$mean %>% diffinv(lag=4,xi=tail(train_pib_usa,4))

predicciones<-(predicciones+train_pib_usa_decom$seasonal[1:8])^2

autoplot(ts.union(train_original_pib_usa,  ts(predicciones[4:8],start=c(2018,4),frequency=4)))

acuracy_AR<-accuracy(test_pib_usa,ts(predicciones[5:8],start=c(2019,1),frequency=4, end=c(2019,4)))

# MODELO MA


fit001<-Arima(train_pib_usa2, order = c(0,0,1))
fit002<-Arima(train_pib_usa2, order = c(0,0,2))
fit003<-Arima(train_pib_usa2, order = c(0,0,3))
fit011<-Arima(train_pib_usa2, order = c(0,1,1))
fit012<-Arima(train_pib_usa2, order = c(0,1,2))
fit013<-Arima(train_pib_usa2, order = c(0,1,3))
fit021<-Arima(train_pib_usa2, order = c(0,2,1))
fit022<-Arima(train_pib_usa2, order = c(0,2,2))
fit023<-Arima(train_pib_usa2, order = c(0,2,3))

bestMA<-which.min(c(
  fit001$aicc,fit002$aicc, fit003$aicc, fit011$aicc, fit012$aicc,fit013$aicc,
  fit021$aicc,fit022$aicc, fit023$aicc
))

bestMA # 3 -> (0,0,3) -> MA(3)

modeloMA<-Arima(train_pib_usa2, order=c(0,0,3))

checkresiduals(modeloMA)

# test_pib_usa DE RESIDUOS

if (Box.test(modeloMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloMA,h=4)$mean %>% diffinv(lag=4,xi=tail(train_pib_usa,4))

predicciones<-(predicciones+train_pib_usa_decom$seasonal[1:8])^2

autoplot(ts.union(train_original_pib_usa,  ts(predicciones[4:8],start=c(2018,4),frequency=4)))

acuracy_MA<-accuracy(test_pib_usa,ts(predicciones[5:8],start=c(2019,1),frequency=4, end=c(2019,4)))


# MODELOS ARMA

fit101<-Arima(train_pib_usa2, order = c(1,0,1))
fit102<-Arima(train_pib_usa2, order = c(1,0,2))
fit103<-Arima(train_pib_usa2, order = c(1,0,3))
fit201<-Arima(train_pib_usa2, order = c(2,0,1))
fit202<-Arima(train_pib_usa2, order = c(2,0,2))
fit203<-Arima(train_pib_usa2, order = c(2,0,3))
fit301<-Arima(train_pib_usa2, order = c(3,0,1))
fit302<-Arima(train_pib_usa2, order = c(3,0,2))
fit303<-Arima(train_pib_usa2, order = c(3,0,3))

bestARMA<-which.min(c(
  fit101$aicc,fit102$aicc, fit103$aicc, fit201$aicc, fit202$aicc,fit203$aicc,
  fit301$aicc,fit302$aicc, fit303$aicc
))

bestARMA # 6 -> (2,0,3) -> ARMA(2,3)

modeloARMA<-Arima(train_pib_usa2, order=c(2,0,3))

checkresiduals(modeloARMA)

# test_pib_usa DE RESIDUOS

if (Box.test(modeloARMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloARMA,h=4)$mean %>% diffinv(lag=4,xi=tail(train_pib_usa,4))

predicciones<-(predicciones+train_pib_usa_decom$seasonal[1:8])^2

autoplot(ts.union(train_original_pib_usa,  ts(predicciones[4:8],start=c(2018,4),frequency=4)))

acuracy_ARMA<-accuracy(test_pib_usa,ts(predicciones[5:8],start=c(2019,1),frequency=4, end=c(2019,4)))


#MODELO ARIMA A MANO

fit111<-Arima(train_pib_usa2, order = c(1,1,1))
fit112<-Arima(train_pib_usa2, order = c(1,1,2))
fit113<-Arima(train_pib_usa2, order = c(1,1,3))
fit211<-Arima(train_pib_usa2, order = c(2,1,1))
fit212<-Arima(train_pib_usa2, order = c(2,1,2))
fit213<-Arima(train_pib_usa2, order = c(2,1,3))
fit311<-Arima(train_pib_usa2, order = c(3,1,1))
fit312<-Arima(train_pib_usa2, order = c(3,1,2))
fit313<-Arima(train_pib_usa2, order = c(3,1,3))

fit121<-Arima(train_pib_usa2, order = c(1,2,1))
fit122<-Arima(train_pib_usa2, order = c(1,2,2))
fit123<-Arima(train_pib_usa2, order = c(1,2,3))
fit221<-Arima(train_pib_usa2, order = c(2,2,1))
fit222<-Arima(train_pib_usa2, order = c(2,2,2))
fit223<-Arima(train_pib_usa2, order = c(2,2,3))
fit321<-Arima(train_pib_usa2, order = c(3,2,1))
fit322<-Arima(train_pib_usa2, order = c(3,2,2))
fit323<-Arima(train_pib_usa2, order = c(3,2,3))

bestARIMA<-which.min(c(
  fit111$aicc,fit112$aicc, fit113$aicc, fit211$aicc, fit212$aicc,fit213$aicc,
  fit311$aicc,fit312$aicc, fit312$aicc,fit121$aicc,fit122$aicc,fit123$aicc,
  fit221$aicc,fit222$aicc,fit223$aicc,fit321$aicc,fit322$aicc,fit323$aicc
))

bestARIMA # 6 -> (2,1,3) -> ARIMA(2,1,3)

modeloARIMA_mano<-Arima(train_pib_usa2, order=c(2,1,3))

checkresiduals(modeloARIMA_mano)

# test_pib_usa DE RESIDUOS

if (Box.test(modeloARIMA_mano$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloARIMA_mano,h=4)$mean %>% diffinv(lag=4,xi=tail(train_pib_usa,4))

predicciones<-(predicciones+train_pib_usa_decom$seasonal[1:8])^2

autoplot(ts.union(train_original_pib_usa,  ts(predicciones[4:8],start=c(2018,4),frequency=4)))

acuracy_ARIMA_mano<-accuracy(test_pib_usa,ts(predicciones[5:8],start=c(2019,1),frequency=4, end=c(2019,4)))

#MODELO ARIMAX

data_pib_francia <- read_csv("./Datos/Transformados/data_pib_francia.csv")

# Separar en tran y test

ts_pib_francia<-data_pib_francia[,2:3]

ts_pib_francia<-ts(ts_pib_francia[,2],frequency= 4, start = c(1996,1), end = c(2022,2))

train_pib_francia<-window(ts_pib_francia, start=c(1996,1), end=c(2018,4))

train_original_pib_francia<-train_pib_francia

test<-window(ts_pib_francia, start=c(2019,1), end=c(2019,4))

# GRAFICAR LOS DATOS

autoplot(train_pib_francia)

gglagplot(train_pib_francia)

ggtsdisplay(train_pib_francia)

# TEST DE ESTACIONARIEDAD

adftest <- adf.test(train_pib_francia)
if (adftest$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(train_pib_francia)%>%round(3))
lambda<-BoxCox.lambda(train_pib_francia)%>%round(3)

autoplot(BoxCox(train_pib_francia, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda = 0.179")

# el mas cercano es 0 -> logaritmo

train_pib_francia <- log(train_pib_francia)

# QUITAMOS LA COMPONENTE ESTACIONAL

train_pib_francia_decom<-decompose(train_pib_francia)

plot(train_pib_francia_decom)

train_pib_francia<-train_pib_francia-train_pib_francia_decom$seasonal

ggtsdisplay(train_pib_francia)

# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA

ndiffs(train_pib_francia)

train_pib_francia2<-diff(train_pib_francia,lag=4, differences = 1)

ggtsdisplay(train_pib_francia2)

# TEST DE ESTACIONARIEDAD

adftest <- adf.test(train_pib_francia2)
if (adftest$p.value > 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# Guardar datos
length(train_pib_usa2)
length(train_pib_francia2)
df_ts_pib_usa <- data.frame(train_pib_usa2[105:192])
colnames(df_ts_pib_usa)<-"train_pib_usa"
colnames(df_ts_pib_usa)
df_ts_pib_francia<- data.frame(train_pib_francia2)
colnames(df_ts_pib_francia)

# Modelo Arimax
# predecir el PIB en USA mediante el PIB de Francia
modeloARIMAX <- auto.arima(df_ts_pib_usa$train_pib_usa, xreg = df_ts_pib_francia$train_pib_francia)
summary(modeloARIMAX) #modelo ARIMA(0,1,0)
checkresiduals(modeloARIMAX)
# TEST DE RESIDUOS
if (Box.test(modeloARIMAX$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloARIMAX, xreg = rep(mean(df_ts_pib_francia$train_pib_francia),4), h=4)$mean %>% diffinv(lag=4,xi=tail(train_pib_usa,4))
predicciones<-(predicciones+train_pib_usa_decom$seasonal[1:8])^2
acuracy_ARIMAX<-accuracy(test_pib_usa,ts(predicciones[5:8],start=c(2019,1),frequency=4, end=c(2019,4)))



# COMPARACION
modelos<-c("AUTOARIMA","ARIMA_MANO", "ARMA", "AR", "MA", "ARIMAX")



resultados<-data.frame(acuracy_AUTOARIMA)
resultados<-resultados%>%rbind(data.frame(acuracy_ARIMA_mano))
resultados<-resultados%>%rbind(data.frame(acuracy_ARMA))
resultados<-resultados%>%rbind(data.frame(acuracy_AR))
resultados<-resultados%>%rbind(data.frame(acuracy_MA))
resultados<-resultados%>%rbind(data.frame(acuracy_ARIMAX))

aicc<-data.frame(modeloAUTOARIMA$aicc)
aicc<-aicc%>%rbind(modeloARIMA_mano$aicc)
aicc<-aicc%>%rbind(modeloARMA$aicc)
aicc<-aicc%>%rbind(modeloAR$aicc)
aicc<-aicc%>%rbind(modeloMA$aicc)
aicc<-aicc%>%rbind(modeloARIMAX$aicc)

RESULTADOS_PIB_USA<-cbind(modelos,resultados,aicc)

RESULTADOS_PIB_USA

# MODELO SELECCIONADO -> ARIMA A MANO

RESULTADOS_PIB_USA[2,]

data_pib_usa <- read_csv("./Datos/Transformados/data_pib_usa.csv")

ts_pib_usa <- data_pib_usa[,2:3]

ts_pib_usa <- ts(ts_pib_usa[,2],frequency= 4, start = c(1970,1), end = c(2022,2))

ts_pib_usa_original<-ts_pib_usa

autoplot(ts_pib_usa)

gglagplot(ts_pib_usa)

ggtsdisplay(ts_pib_usa)

# test_pib_usa DE ESTACIONARIEDAD

adftest_pib_usa <- adf.test(ts_pib_usa)
if (adftest_pib_usa$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(ts_pib_usa)%>%round(3))
lambda<-BoxCox.lambda(ts_pib_usa)%>%round(3)

autoplot(BoxCox(ts_pib_usa, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda=0,143")

# el mas cercano es 0 -> logaritmo

ts_pib_usa<-log(ts_pib_usa)

# QUITAMOS LA COMPONENTE ESTACIONAL

ts_pib_usa_decom<-decompose(ts_pib_usa)

plot(ts_pib_usa_decom)

ts_pib_usa<-ts_pib_usa-ts_pib_usa_decom$seasonal

ggtsdisplay(ts_pib_usa)

# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA

ndiffs(ts_pib_usa)

ts_pib_usa2<-diff(ts_pib_usa,lag=4, differences = 2)

ggtsdisplay(ts_pib_usa2)

modelo_pib_usa<-Arima(ts_pib_usa2, order=c(2,1,3))

checkresiduals(modelo_pib_usa)

# test_pib_usa DE RESIDUOS

if (Box.test(modelo_pib_usa$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modelo_pib_usa,h=4)$mean %>% diffinv(lag=4, differences = 2,xi=tail(ts_pib_usa,8))

predicciones<-(predicciones+ts_pib_usa_decom$seasonal[3:14])%>%exp()

autoplot(ts.union(ts_pib_usa_original,  ts(predicciones[8:12],start=c(2022,2),frequency=4)))

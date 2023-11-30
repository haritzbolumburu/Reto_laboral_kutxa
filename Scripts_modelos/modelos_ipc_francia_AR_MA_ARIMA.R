
# PREDICCION data_ipc_francia AR-MA-ARIMA ------------------------------------------

data_ipc_francia<- read_csv("./Datos/Transformados/data_ipc_francia.csv")

# Separar en tran y test_ipc_francia

ts_ipc_francia<-data_ipc_francia[,2:3]

ts_ipc_francia<-ts(ts_ipc_francia[,2],frequency= 12, start = c(1996,1), end = c(2022,4))
graf<-autoplot(ts_ipc_francia)+ labs(title = "IPC anual de USA (1996-2022)",x='Fecha', y='IPC')
ggsave("ev_IPC_francia.png", graf, path = "./Graficos")

train_ipc_francia<-window(ts_ipc_francia, start=c(1996,1), end=c(2018,12))

train_ipc_francia_original<-train_ipc_francia

test_ipc_francia<-window(ts_ipc_francia, start=c(2019,1), end=c(2019,6))

# GRAFICAR LOS DATOS

autoplot(train_ipc_francia)

gglagplot(train_ipc_francia)

ggtsdisplay(train_ipc_francia)

# test_ipc_francia DE ESTACIONARIEDAD

adftest_ipc_francia <- adf.test(train_ipc_francia)
if (adftest_ipc_francia$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(train_ipc_francia)%>%round(3))
lambda<-BoxCox.lambda(train_ipc_francia)%>%round(3)

autoplot(BoxCox(train_ipc_francia, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda = -0.56")

train_ipc_francia<-log(train_ipc_francia)
# QUITAMOS LA COMPONENTE ESTACIONAL

train_ipc_francia_decom<-decompose(train_ipc_francia)

plot(train_ipc_francia_decom)
train_ipc_francia<-train_ipc_francia-train_ipc_francia_decom$seasonal

ggtsdisplay(train_ipc_francia)


# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA
ndiffs(train_ipc_francia)

train_ipc_francia2<-diff(train_ipc_francia,lag=1, differences = 1)

ggtsdisplay(train_ipc_francia2)

# test_ipc_francia DE ESTACIONARIEDAD

adftest_ipc_francia <- adf.test(train_ipc_francia2)
if (adftest_ipc_francia$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

autoplot(train_ipc_francia2)+ labs(title = "Serie trasnformada del IPC en Francia",x='Fecha', y='IPC')
# MODELO AUTOARIMA

modeloAUTOARIMA<-auto.arima(train_ipc_francia2, seasonal = F)

auto.arima(train_ipc_francia2, seasonal = F) # arima(1,0,1)

checkresiduals(modeloAUTOARIMA)


# test_ipc_francia DE RESIDUOS

if (Box.test(modeloAUTOARIMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-forecast(modeloAUTOARIMA, h=6)$mean %>% diffinv(lag=1,xi=tail(train_ipc_francia,1))

predicciones<-(predicciones+train_ipc_francia_decom$seasonal[12:18])%>%exp()

autoplot(ts.union(train_ipc_francia_original,  ts(predicciones,start=c(2018,12),frequency=12)))

acuracy_AUTOARIMA<-accuracy(test_ipc_francia,ts(predicciones[2:7],start=c(2019,1),frequency=12, end=c(2019,6)))


# MODELO AR


fit100<-Arima(train_ipc_francia2, order = c(1,0,0))
fit200<-Arima(train_ipc_francia2, order = c(2,0,0))
fit300<-Arima(train_ipc_francia2, order = c(3,0,0))
fit110<-Arima(train_ipc_francia2, order = c(1,1,0))
fit210<-Arima(train_ipc_francia2, order = c(2,1,0))
fit310<-Arima(train_ipc_francia2, order = c(3,1,0))
fit120<-Arima(train_ipc_francia2, order = c(1,2,0))
fit220<-Arima(train_ipc_francia2, order = c(2,2,0))
fit320<-Arima(train_ipc_francia2, order = c(3,2,0))

bestAR<-which.min(c(
  fit100$aicc,fit200$aicc, fit300$aicc, fit110$aicc, fit210$aicc,fit310$aicc,
  fit120$aicc,fit220$aicc, fit320$aicc
))

bestAR # 1-> (1,0,0) -> AR(1)

modeloAR<-Arima(train_ipc_francia2, order=c(1,0,0))

checkresiduals(modeloAR)

# test_ipc_francia DE RESIDUOS

if (Box.test(modeloAR$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}


predicciones<-  forecast(modeloAR,h=6)$mean %>% diffinv(lag=1,xi=tail(train_ipc_francia,1))

predicciones<-(predicciones+train_ipc_francia_decom$seasonal[12:18])%>%exp()

autoplot(ts.union(train_ipc_francia_original,  ts(predicciones,start=c(2018,12),frequency=12)))

acuracy_AR<-accuracy(test_ipc_francia,ts(predicciones[2:7],start=c(2019,1),frequency=12, end=c(2019,6)))

# MODELO MA


fit001<-Arima(train_ipc_francia2, order = c(0,0,1))
fit002<-Arima(train_ipc_francia2, order = c(0,0,2))
fit003<-Arima(train_ipc_francia2, order = c(0,0,3))
fit011<-Arima(train_ipc_francia2, order = c(0,1,1))
fit012<-Arima(train_ipc_francia2, order = c(0,1,2))
fit013<-Arima(train_ipc_francia2, order = c(0,1,3))
fit021<-Arima(train_ipc_francia2, order = c(0,2,1))
fit022<-Arima(train_ipc_francia2, order = c(0,2,2))
fit023<-Arima(train_ipc_francia2, order = c(0,2,3))

bestMA<-which.min(c(
  fit001$aicc,fit002$aicc, fit003$aicc, fit011$aicc, fit012$aicc,fit013$aicc,
  fit021$aicc,fit022$aicc, fit023$aicc
))

bestMA # 1 -> (0,0,1) -> MA(1)

modeloMA<-Arima(train_ipc_francia2, order=c(0,0,1))

checkresiduals(modeloMA)

# test_ipc_francia DE RESIDUOS

if (Box.test(modeloMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloMA,h=6)$mean %>% diffinv(lag=1,xi=tail(train_ipc_francia,1))

predicciones<-(predicciones+train_ipc_francia_decom$seasonal[12:18])%>%exp()

autoplot(ts.union(train_ipc_francia_original,  ts(predicciones,start=c(2018,12),frequency=12)))

acuracy_MA<-accuracy(test_ipc_francia,ts(predicciones[2:7],start=c(2019,1),frequency=12, end=c(2019,6)))

# MODELOS ARMA

fit101<-Arima(train_ipc_francia2, order = c(1,0,1))
fit102<-Arima(train_ipc_francia2, order = c(1,0,2))
fit103<-Arima(train_ipc_francia2, order = c(1,0,3))
fit201<-Arima(train_ipc_francia2, order = c(2,0,1))
fit202<-Arima(train_ipc_francia2, order = c(2,0,2))
fit203<-Arima(train_ipc_francia2, order = c(2,0,3))
fit301<-Arima(train_ipc_francia2, order = c(3,0,1))
fit302<-Arima(train_ipc_francia2, order = c(3,0,2))
fit303<-Arima(train_ipc_francia2, order = c(3,0,3))

bestARMA<-which.min(c(
  fit101$aicc,fit102$aicc, fit103$aicc, fit201$aicc, fit202$aicc,fit203$aicc,
  fit301$aicc,fit302$aicc, fit303$aicc
))

bestARMA # 1 -> (1,0,1) -> ARMA(1,1)

modeloARMA<-Arima(train_ipc_francia2, order=c(1,0,1))

checkresiduals(modeloARMA)

# test_ipc_francia DE RESIDUOS

if (Box.test(modeloARMA$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloARMA,h=6)$mean %>% diffinv(lag=1,xi=tail(train_ipc_francia,1))

predicciones<-(predicciones+train_ipc_francia_decom$seasonal[12:18])%>%exp()

autoplot(ts.union(train_ipc_francia_original,  ts(predicciones,start=c(2018,12),frequency=12)))

acuracy_ARMA<-accuracy(test_ipc_francia,ts(predicciones[2:7],start=c(2019,1),frequency=12, end=c(2019,6)))


#MODELO ARIMA A MANO

fit111<-Arima(train_ipc_francia2, order = c(1,1,1))
fit112<-Arima(train_ipc_francia2, order = c(1,1,2))
fit113<-Arima(train_ipc_francia2, order = c(1,1,3))
fit211<-Arima(train_ipc_francia2, order = c(2,1,1))
fit212<-Arima(train_ipc_francia2, order = c(2,1,2))
fit213<-Arima(train_ipc_francia2, order = c(2,1,3))
fit311<-Arima(train_ipc_francia2, order = c(3,1,1))
fit312<-Arima(train_ipc_francia2, order = c(3,1,2))
fit313<-Arima(train_ipc_francia2, order = c(3,1,3))

fit121<-Arima(train_ipc_francia2, order = c(1,2,1))
fit122<-Arima(train_ipc_francia2, order = c(1,2,2))
fit123<-Arima(train_ipc_francia2, order = c(1,2,3))
fit221<-Arima(train_ipc_francia2, order = c(2,2,1))
fit222<-Arima(train_ipc_francia2, order = c(2,2,2))
fit223<-Arima(train_ipc_francia2, order = c(2,2,3))
fit321<-Arima(train_ipc_francia2, order = c(3,2,1))
fit322<-Arima(train_ipc_francia2, order = c(3,2,2))
fit323<-Arima(train_ipc_francia2, order = c(3,2,3))

bestARIMA<-which.min(c(
  fit111$aicc,fit112$aicc, fit113$aicc, fit211$aicc, fit212$aicc,fit213$aicc,
  fit311$aicc,fit312$aicc, fit312$aicc,fit121$aicc,fit122$aicc,fit123$aicc,
  fit221$aicc,fit222$aicc,fit223$aicc,fit321$aicc,fit322$aicc,fit323$aicc
))

bestARIMA # 2 -> (1,1,2) -> ARIMA(1,1,2)

modeloARIMA_mano<-Arima(train_ipc_francia2, order=c(1,1,2))

checkresiduals(modeloARIMA_mano)

# test_ipc_francia DE RESIDUOS

if (Box.test(modeloARIMA_mano$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloARIMA_mano,h=6)$mean %>% diffinv(lag=1,xi=tail(train_ipc_francia,1))

predicciones<-(predicciones+train_ipc_francia_decom$seasonal[12:18])%>%exp()

autoplot(ts.union(train_ipc_francia_original,  ts(predicciones,start=c(2018,12),frequency=12)))

acuracy_ARIMA_mano<-accuracy(test_ipc_francia,ts(predicciones[2:7],start=c(2019,1),frequency=12, end=c(2019,6)))

#MODELO ARIMAX


data_ipc_usa<- read_csv("./Datos/Transformados/data_ipc_usa.csv")

# Separar en tran y test_ipc_usa

ts_ipc_usa<-data_ipc_usa[,2:3]

ts_ipc_usa<-ts(ts_ipc_usa[,2],frequency= 12,start = c(1970,1), end = c(2022,7))

train_ipc_usa<-window(ts_ipc_usa, start=c(1970,1), end=c(2018,12))

train_ipc_usa_original<-train_ipc_usa

test_ipc_usa<-window(ts_ipc_usa, start=c(2019,1), end=c(2019,6))

# GRAFICAR LOS DATOS

autoplot(train_ipc_usa)

gglagplot(train_ipc_usa)

ggtsdisplay(train_ipc_usa)

# test_ipc_usa DE ESTACIONARIEDAD

adftest_ipc_usa <- adf.test(train_ipc_usa)
if (adftest_ipc_usa$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}

# ESTABILIZAR LA VARIANZA MEDIANTE BOXCOX

#elegir el lambda optimo

paste("optimal lambda: ", BoxCox.lambda(train_ipc_usa)%>%round(3))
lambda<-BoxCox.lambda(train_ipc_usa)%>%round(3)

autoplot(BoxCox(train_ipc_usa, lambda = lambda))+xlab("Fecha")+ylab("")+
  ggtitle("Plot-lambda = 0.915")

# el mas cercano es 1 -> NADA

# QUITAMOS LA COMPONENTE ESTACIONAL

train_ipc_usa_decom<-decompose(train_ipc_usa)

plot(train_ipc_usa_decom)

train_ipc_usa<-train_ipc_usa-train_ipc_usa_decom$seasonal

ggtsdisplay(train_ipc_usa)


# DIFERENCIAMOS LA SERIE PARA QUITAR LA TENDENCIA

ndiffs(train_ipc_usa)

train_ipc_usa2<-diff(train_ipc_usa,lag=1, differences = 1)

ggtsdisplay(train_ipc_usa2)

# test_ipc_usa DE ESTACIONARIEDAD

adftest_ipc_usa <- adf.test(train_ipc_usa2)
if (adftest_ipc_usa$p.value < 0.05) {print("serie estacionaria")
}else {
  print("serie NO estacionaria")
}



# Guardar datos
length(train_ipc_usa2)
length(train_ipc_francia2)
df_ts_ipc_usa <- data.frame(train_ipc_usa2[313:587])
colnames(df_ts_ipc_usa)<-"train_ipc_usa"
colnames(df_ts_ipc_usa)
df_ts_ipc_francia<- data.frame(train_ipc_francia2)
colnames(df_ts_ipc_francia)

# Modelo Arimax
# predecir el PIB en USA mediante el PIB de Francia
modeloARIMAX <- auto.arima(df_ts_ipc_francia$train_ipc_francia, xreg = df_ts_ipc_usa$train_ipc_usa)
summary(modeloARIMAX) #modelo ARIMA(3,0,3)
checkresiduals(modeloARIMAX)
# TEST DE RESIDUOS
if (Box.test(modeloARIMAX$residuals)$p.value<0.05){
  print("Los residuos no son ruido blanco")
} else {
  print("Los residuos son ruido blanco")
}

predicciones<-  forecast(modeloARIMAX, xreg = rep(mean(df_ts_ipc_usa$train_ipc_usa),6), h=6)$mean %>% diffinv(lag=1,xi=tail(train_ipc_francia,1))
predicciones<-(predicciones+train_ipc_francia_decom$seasonal[12:18])%>%exp()
acuracy_ARIMAX<-accuracy(test_ipc_francia,ts(predicciones[2:7],start=c(2019,1),frequency=12, end=c(2019,6)))

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

RESULTADOS_ipc_francia<-cbind(modelos,resultados,aicc)

RESULTADOS_ipc_francia

# MODELO SELECCIONADO -> ARMA


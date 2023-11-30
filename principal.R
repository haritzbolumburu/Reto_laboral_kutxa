# Reto 05 - equipo morado -------------------------------------------------
source("./carga_librerias.R")

#preprocesamiento
source("./Scripts_preprocesamiento/preprocesamiento.R")
source("./Scripts_preprocesamiento/adaptacion_Influx.R") #meter Token y org propia
rm(list=ls())
source("./Scripts_preprocesamiento/querys_dataframes.R") #meter Token
rm(list=ls())

#analisis
source("./Scripts_EDA/EDA.R")
rm(list=ls())

#modelado
source("./Scripts_modelos/modelos_pib_usa_AR_MA_ARIMA.R")
rm(list=ls())
source("./Scripts_modelos/modelos_pib_francia_AR_MA_ARIMA.R")
rm(list=ls())
source("./Scripts_modelos/modelos_ipc_usa_AR_MA_ARIMA.R")
rm(list=ls())
source("./Scripts_modelos/modelos_ipc_francia_AR_MA_ARIMA.R")
rm(list=ls())
source("./Scripts_modelos/modelos_VAR.R") #se carga la libreria vars que hace que la funcion select de dplyr no funciones
rm(list=ls())
source("./Scripts_modelos/prediccion_pib_real.R")
rm(list=ls())

# Ejemplo 4.1

library(foreach)  # Pronosticos y otros
library(TSA)      # Serie de tiempo
library(urca)     # Prueba de Raiz unitaria
library(tseries)  # Serie de tiempo
library(readxl)
casa <- read_excel("C:/UNAP/SERIES DE TIEMPOS/Ejm_4_1-Casas.xlsx")
View(casa)

# Importacion

# 1. Metodo grafico

# a) Grafica de serie original$
# Venta de casas
Yc <- ts(casa$ventas, start = c(2010, 1), frequency = 12)
plot(Yc, xlab = "Años", ylab = "Ventas")
abline(h = mean(Yc), col = "red")

# Poblacion
library(readxl)
poblacion <- read_excel("C:/UNAP/SERIES DE TIEMPOS/Ejm_4_1-Poblacion.xlsx")
View(poblacion)
Yp <- ts(poblacion$poblacion, start = c(1997, 1), frequency = 4)
plot(Yp, xlab = "Años", ylab = "Poblacion")
abline(h = mean(Yc), col = "red")

# b) Correlograma
par(mfrow = c(1,2))
Yc_acf <- acf(Yc, lag.max = 25, main = "")
Yc_pacf <- pacf(Yc, lag.max = 25, main = "")
Yc_pacf$acf

Yc_acf <- acf(Yp, lag.max = 9, main = "")
Yc_pacf <- pacf(Yp, lag.max = 9, main = "")
Yc_pacf$acf


# Serie estacional funcion de autocorrelacion simple de crecimiento exponencial o rapido.
# Funcion de autocorrelacion parcial el primer coeficiente de aurtocorrelacion parcial menor a ser 0.29
# No estacionario: El fast de crecimiento lento el 

# 2. Prueba de raiz unitaria

# a) Prueba de  Dickey - Fuller
Yc_df <- ur.df(Yc, type = "drift", lags = 0)
summary(Yc_df)
# Apuntes
# PRUEBA ESTADISTICA
# 1) Planteamiento de hipotesis 
# 2) Nivel de significancia
# 3) Estadistico de prueba
# 4) Decision
# 5) Conclusion
Yp_df <- ur.df(Yp, type = "trend", lags = 0)
summary(Yp_df)

# Tambien se utiliza la funcion adf.test()
adf.test(Yc, k = 0)
adf.test(Yp, k = 0)
# b) Prueba de Dickey - Fuller aumentada
Yc_df <- ur.df(Yc, type = "drift", lags = 1)
summary(Yc_df)
Yp_df <- ur.df(Yp, type = "trend", lags = 1)
summary(Yp_df)

# c) Prueba de Phillps - Perron (PP)
pp.test(Yc, lshort = TRUE)
pp.test(Yp, lshort = TRUE)

# d) Prueba de KPSS
kpss.test(Yc)
kpss.test(Yp)

# e) Prueba de ERS
Yc_ers <- ur.ers(Yc, type = "P-test", model = "constant", lag.max = 1)
summary(Yc_ers)

Yp_ers <- ur.ers(Yp, type = "P-test", model = "trend", lag.max = 1)
summary(Yp_ers)

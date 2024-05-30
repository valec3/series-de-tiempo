#EJEMPLO 8 -Modelos ARMA

# librerias
library(forecast) # Modelos ARIMA
library(tseries)   # Series de tiempo
library(TSA)	# Series de tiempo
library(urca)	# Raiz unitaria
library(ggplot2)  # Graficos
library(dplyr)	# Manipulacion de datos

# importacion de datos con el nombre "datos"
datos = read.csv("F:/777--Programacion repos/Una/r/data/Ejem_2_3.csv")
#Grafica de la serie

Y <- ts(datos$Yt, start = 1, frequency = 1)
plot(Y, xlab = "Tiempo", ylab = "Lecturas")

# ESTACIONALIDAD EN MEDIA

# Correlograma
FAS <- acf(Y, lag.max = 16, main = "FAS", level = 0.95)
FAS <- pacf(Y, lag.max = 16, main = "FAP", level = 0.95)

# Prueba de raiz unitaria
Y_ru <- ur.df(Y, type = "drift", lags = 1)
summary(Y_ru)



# ESTACIONARIEDAD EN VARIANZA
# Diagrama de cajas "boxplot"
grupo <- c(rep(1:5, rep(15, 5)))
boxplot(datos$Yt-grupo,xlab = "Grupo")

# Definir el modelo
# AR(1)
# MA(2)
# ARMA(1, 2)

# Modelo 1: AR(1)
modelo1 <- arma(Y, order = c(1, 0))
summary(modelo1)

# Modelo 2: MA(2)
modelo2 <- arma(Y, order = c(0, 2))
summary(modelo2)

# Modelo 3: ARMA(1, 2)
modelo3 <- arma(Y, order = c(1,2))
summary(modelo3)
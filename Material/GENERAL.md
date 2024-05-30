## LIBRERIAS MAS UTILIZADAS

```R
library(forecast) # Modelos ARIMA
library(tseries)   # Series de tiempo
library(TSA)	# Series de tiempo
library(urca)	# Raiz unitaria
library(ggplot2)  # Graficos
library(dplyr)	# Manipulacion de datos
```

## MEDIA MOVIL y MEDIA MOVIL DOBLE

```R
# Cargar datos:
# # file.choose() abre una ventana para seleccionar el archivo
datos = read_excel("F:/777--Programacion repos/Una/r/data/Ejem_2_3.xlsx")
View(datos)

#Convertir los datos a serie temporal
yts <- ts(datos$Y, start = c(2011,1), frequency = 12)
print(yts)

#Calculo de la media
k = 12
yts_ma <- SMA(yts,k)
print(yts_ma)
lines(yts_ma,type="l", col = "blue")
legend(x = "bottomright",legend = c("yts","yts_ma"), col = c('black','red'), lty = c(1,1))

#Calculo de la media movil
pred = predict(yts_ma,h=12)
summary(pred)# muestra los pronosticos y medidas de error


#Graficar la serie temporal y los pronosticos
pred
plot(pred,col="red")


#Calculo de la media movil doble
k = 12
yts_ma2 = SMA(yts_ma,k)
a = 2*yts_ma - yts_ma2
b = (2/(k-1))*(yts_ma - yts_ma2)

p = 1
yma2 = a + b*p

#Graficar la serie temporal original y la media movil doble
plot(yts,type = "l", xlab="Meses",ylab="Y")
lines(yma2,type="l", col = "blue")
legend(x = "bottomright",legend = c("yts","yma2"), col = c('black','blue'), lty = c(1,1))


##############################################
####   COMPARACION ENTRE MEDIA MOVIL Y MEDIA MOVIL DOBLE
##############################################

plot(Yts, type = "l", xlab="Meses", ylab=" ")
lines(Yts_ma, type = "l", col = "red")
lines(Yma2, type = "l", col = "blue")
legend(x = "bottomright", legend = c("Yts", "Yts_ma", "Yma2"), col = c('black', 'red',
'blue'), lty = c(1, 1))
```

##

## ESTACIONALIDAD

```R
# Grafico de lineas apiladas
ggsubseriesplot(yts, xlab = "Trimestres", ylab = "Horas")
#  Ssi el comportamiento es diferente, entonces existe estacionalidad.

# Grafico de lineas separadas
ggseasonplot(yts, xlab = "Trimestres", ylab = "Horas")
# Si las lineas tienen el mismo comportamiento, hay estacionalidad

# Grafico de correlograma
ggAcf(yts, xlab = "Trimestres", ylab = "Horas")
Si los picos se repiten en el mismo periodo, hay estacionalidad
```

## DESCOMPOSICION DE SERIES DE TIEMPO

```R
decomp = decompose(yts)
plot(decomp$x)
# TENDENCIA
plot(decomp$trend)
# ESTACIONALIDAD
plot(decomp$seasonal)
# RESIDUOS o ALEATORIEDAD
plot(decomp$random)

```

## MODELOS ARMA

```R
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
```

## MODELO AR(1), MA(2) Y ARMA(1,2)

```R
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
```

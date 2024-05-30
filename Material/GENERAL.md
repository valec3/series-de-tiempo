## LIBRERIAS MAS UTILIZADAS

```R
library(forecast) # Modelos ARIMA
library(tseries)   # Series de tiempo
library(TSA)	# Series de tiempo
library(urca)	# Raiz unitaria
library(ggplot2)  # Graficos
library(dplyr)	# Manipulacion de datos
library(fpp2)    # Series de tiempo
library(readxl)  # Leer archivos excel
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

## Suavizamiento Exponencial Simple, doble, holt y holt-winters

```R
datos <- read_excel("D:/ … /Ejem_2_3.xlsx")
# Convertir los datos a Serie Temporal
Yts<-ts(datos$Y, start = c(2013,1), frequency = 12)
# Grafico de la serie
plot(Yts, type = "l", xlab="Meses", ylab="Y")

# Suavizamiento Exponencial Simple

Yses <- ses(Yts, alpha = 0.5, h = 12)
plot(Yts, type = "l", xlab="Meses", ylab=" ")
lines(Yses$fitted, type = "l", col = "red")
legend(x = "bottomright", legend = c("Yts", "Yses"), col = c('black', 'red'), lty = c(1,
1))
# Grafico del pronostico para 12 meses
summary(Yses)
plot(Yses, col = "red")


################################
# Suavizamiento Exponencial Doble

datos <- read_excel("D:/ … /Ejem_2_4.xlsx")
View(datos)
VENts<-ts(datos$VEN, start = 1, frequency = 1)
print(VENts)
plot(VENts, type = "l", xlab="Meses", ylab="Ventas")

VENses <- ses(VENts, alpha = 0.136, h = 1) # Primero
VEN2 <- ses(VENses$fitted, alpha = 0.136, h = 1) # Segundo
a <- 2*VENses$fitted - VEN2$fitted
b <- (0.136/(1-0.136))*(VENses$fitted - VEN2$fitted)
p <- 1 # Periodo futuro a pronosticar
VENses2 <- a + b*p
plot(VENts, type = "l", xlab="Meses", ylab=" ") # Grafico de la serie temporal
lines(VENses2, type = "l", col = "red")
legend(x = "bottomright", legend = c("VENts", "VENses2"), col = c('black', 'red'), lty =
c(1, 1))

############################
# Método de Holt
VENholt <- holt(VENts, h = 1, alpha = 0.3, beta = 0.05)
plot(VENts, type = "l", xlab="Meses", ylab=" ")
lines(VENholt$fitted, type = "l", col = "red")
summary(VENholt)

# otro ejemplo de holt
datos <- read_excel("D:/ … /Ejem_2_5.xlsx")
Yts<-ts(datos$PBI_mm, start = c(1980,1), frequency = 1)
plot(Yts, type = "l", xlab="Años", ylab="PBI")
Yholt <- holt(Yts, h = 5, alpha = 0.4, beta = 0.07)
plot(Yts, type = "l", xlab="Años", ylab=" ")
lines(Yholt$fitted, type = "l", col = "red")

################################
# Método de Holt-Winters
VENts<-ts(datos$VEN, start = 1, frequency = 4) # frecuencia trimestral
plot(VENts, type = "l", xlab="Meses", ylab="Ventas")
VENhw_m <- HoltWinters(VENts, seasonal = "multiplicative")
plot(VENhw_m)
legend(x = "bottomright", legend = c("VENts", "VENhw_m"), col = c('blac
k', 'red'), lty = c(1, 1))

# personalizar los parametros
VENhw_m2 <- HoltWinters(VENts, seasonal = "multiplicative", optim.start = c(0.98,0.01,0))
plot(VENhw_m2)

# Pronosticos para 4 trimestres
PronHW<-forecast(VENhw_m2, h = 4,level = c(0.95)) # h = 4, pronostico para 4 trimestres
PronHW
# grafico usando ggplot2
PronHW %>% autoplot(main = "Pronostico de Holt Winters, Multiplicativo")

```

---

## ESTACIONARIEDAD

```R
# Prueba de raiz unitaria
Yc <- ts(casa$ventas, start = c(2010, 1), frequency = 12)
plot(Yc, xlab = "Años", ylab = "Ventas")
abline(h = mean(Yc), col = "red") # Linea de la media

# Poblacion
poblacion <- read_excel("C:/UNAP/SERIES DE TIEMPOS/Ejm_4_1-Poblacion.xlsx")
Yp <- ts(poblacion$poblacion, start = c(1997, 1), frequency = 4)
plot(Yp, xlab = "Años", ylab = "Poblacion")
abline(h = mean(Yc), col = "red")

# b) Correlograma
par(mfrow = c(1,2)) # Divide la ventana en 2
# ventas
Yc_acf <- acf(Yc, lag.max = 25, main = "") # Correlograma
Yc_pacf <- pacf(Yc, lag.max = 25, main = "") # Autocorrelograma parcial
Yc_pacf$acf # Muestra los valores del correlograma

# poblacion
Yc_acf <- acf(Yp, lag.max = 9, main = "") # Correlograma
Yc_pacf <- pacf(Yp, lag.max = 9, main = "") # Autocorrelograma parcial
Yc_pacf$acf # Muestra los valores del correlograma parcial

# CITA GRAN JULIO: # Serie estacional funcion de autocorrelacion simple de crecimiento exponencial o rapido.
# # Funcion de autocorrelacion parcial el primer coeficiente de aurtocorrelacion parcial menor a ser 0.29
# # No estacionario: El fast de crecimiento lento el

# 2. Prueba de raiz unitaria
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


```

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
# Si los picos se repiten en el mismo periodo, hay estacionalidad
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

## DESCOMPOSICIÓN Y ANÁLISIS DE COMPONENTES - Tendencia

```R

# Cargar datos desde Excel
serie <- read_excel("C:/Users/JHONYNANI/Downloads/Ejm_3_3.xlsx")
# Crear serie de tiempo
Yt <- ts(serie$Y, start = c(1984, 3), frequency = 4)
# Graficar la serie de tiempo
plot(Yt,xlab = "Años")

# Descomposicion MULTIPICATIVA
Yt_decomp <- decompose(Yt, type = "multiplicative")
plot(Yt_decomp)


serie <- read_excel("D:/.../Ejm_3_3.xlsx")
View(serie)
Yts <- ts(serie$Y, start = c(1984,3), frequency = 4)
print(Yts)
plot(Yts, type = "l", xlab="Años", ylab="Y")
# Ajuste a un modelo lineal
t <- time(Yts)
Ylm <- lm(Yts ~ t) # Modelo lineal simple Y = a + b*t
pron = ts(predict(Ylm, t), c(1984,3), frequency = 4)
plot(Yts, xlab="Años", ylab=" ")
lines(pron, type = "l", col = "red")
legend(x = "bottomright", legend = c("Yts", "Ylm"), col = c('black', 'red'), lty = c(1,1))

# Ajuste Cuadrático
Y2 <- lm(Yts ~ t + I(t^2))
pron2 = ts(predict(Y2, t), c(1984,3), frequency = 4)
plot(Yts, xlab="Años", ylab=" ")
lines(pron2, type = "l", col = "red")
legend(x = "bottomright", legend = c("Yts", "Y2"), col = c('black', 'red'), lty = c(1, 1))

# Ajuste a un Exponencial
nt <- 1:32 # Número de trimestres o datos de la serie
Ye <- lm(log(Yts) ~ nt)
Ye
Y3 <- Ye$coefficients[1] + Ye$coefficients[2]*nt
pron3 = exp(Y3)
plot(serie$Y, type = "l", xlab="Tiempo", ylab=" ")
lines(pron3, type = "l", col = "red")
legend(x = "bottomright", legend = c("Y", "Y3"), col = c('black', 'red'), lty = c(1, 1))
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

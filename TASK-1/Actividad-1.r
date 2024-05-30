#LIBRERIAS NECESARIA
library(ggplot2) #PARA HACER GRAFICOS
library(forecast) #PRONOSTICO
library(TTR) #metodo de media movil
library(readxl) #para leer archivos excel
library(fpp2) 

#EJERCICIO 1


x <- c(1,2,3,4,5,6,7,8,9,10,11,12)
y <- c(2400,6200,6000,6000,9600,12400,10400,11600,15200,18000,16000,19000)
datos <- data.frame(Años = x, Ventas = y)

View(datos)

Yts <- ts(datos$Ventas, start = c(1,1), frequency = 1)
print(Yts)

#Grafico de la serie ORIGINAL
plot(Yts, type="l", xlab = "Años", ylab="Ventas")

################################
#MEDIA MOVIL SIMPLE
################################

library(TTR) #metodo de media movil
k<-2 #Periodo
Yts_ma <- SMA(Yts, k)
print(Yts_ma)
#Grafico de la serie original y la media movil simple
plot(Yts, type="l", xlab = "Años", ylab="Ventas")
lines(Yts_ma, type="l", col="red")
legend(x = "bottomright", legend =  c("Yts", "Yts_ma"), col = c("black","red"), lty=c(1,1))

# Grafico de la predicción de la media movil simple
pred <- predict(Yts_ma, h=5)
summary(pred)
plot(pred, col="red")

###########################
#MEDIA MOVIL DOBLE
###########################

Yts_ma2 <- SMA(Yts_ma, k) # Media móvil de media móvil
a <- 2*Yts_ma - Yts_ma2
b <- (2/(k-1))*(Yts_ma - Yts_ma2)
p <- 5 # Periodo futuro a pronosticar
Yma2 <- a + b*p
print(Yma2)


# Graficar la serie original y la media móvil doble
plot(Yts, type = "l", xlab="Meses", ylab=" ") # Serie original
lines(Yma2, type = "l", col = "blue") # Media móvil doble
legend(x = "bottomright", legend = c("Yts", "Yma2"), col = c('black', 'blue'), lty = c(1,
1))

# Graficar la predicción de la media movil doble
pred <- predict(Yma2, h=5)
summary(pred)
plot(pred, col="blue")


#GRAFICA DE AMBOS METODOS con la serie original
plot(Yts, type='l', xlab="Años", ylab="Ventas")
lines(Yts_ma, type='l', col="red")
lines(Yma2, type='l', col="blue")
legend(x = "bottomright", legend = c("Yts_ma", "Yma2"), col = c('red', 'blue'), lty = c(1, 1))

# La media movil simple se ajusta mas a los datos originales que la media movil doble.

##################################################
#EJERCICIO 2


datos = read_excel("F:/777--Programacion repos/Una/r/data/02-t.xlsx")

Yts <- ts(datos$Ventas, start = c(2015,1), frequency = 12)
print(Yts)

#Graficar la serie temporal
plot(Yts,type = "l", xlab="Meses",ylab="Y")

# a) Construir un promedio móvil de orden 12
k = 12
Yts_ma <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Meses", ylab = " ") # Serie original
lines(Yts_ma, type = "l", col = "red")



# b) Construir un promedio móvil doble de orden 12
Yts_ma2 <- SMA(Yts_ma, k) # Media móvil de media móvil
a <- 2*Yts_ma - Yts_ma2
b <- (2/(k-1))*(Yts_ma - Yts_ma2)
p <- 5 # Periodo futuro a pronosticar
Yma2 <- a + b*p
print(Yma2)

# Graficar la serie original y la media móvil doble
plot(Yts, type = "l", xlab="Meses", ylab=" ") # Serie original
lines(Yma2, type = "l", col = "blue") # Media móvil doble


# c) Representar gráficamente los resultados obtenidos junto con los datos originales
# y comparar los resultados.

plot(Yts, type='l', xlab="Años", ylab="Ventas")
lines(Yts_ma, type='l', col="red")
lines(Yts_ma2, type='l', col="blue")

# Podemos ver que la media movil doble es mas suave que la media movil simple.
# Y que la media movil simple se ajusta mas a los datos originales.


##################################################
#EJERCICIO 3


datos = read_excel("F:/777--Programacion repos/Una/r/data/03-t.xlsx")

VENts <- ts(datos$Ventas, start = c(2010,1), frequency = 12)
print(VENts)

#Graficar la serie temporal
plot(VENts,type = "l", xlab="Meses",ylab="Ventas",col="black")

# HOLT WINTERS SIN PARAMETROS

VENhw_m <- HoltWinters(VENts,seasonal = "multiplicative")
plot(VENhw_m)
legend("topleft",legend = c("Original","Prediccion"),col = c("black","red"),lty = c(1,1))

VENhw_m #Muestra los valores de alpha, beta y gamma

# HOLT WINTERS CON PARAMETROS
VENhw_m2 <- HoltWinters(VENts,seasonal = "multiplicative",optim.start = c(0.4,0.6002,1))
plot(VENhw_m2)
legend("topleft",legend = c("Original","Prediccion"),col = c("black","red"),lty = c(1,1))


# Predicciones

autoplot(forecast(VENhw_m,h=5),xlab="Años",ylab="Ventas",col="red")
autoplot(forecast(VENhw_m2,h=5),xlab="Años",ylab="Ventas",col="red")

print(forecast(VENhw_m,h=5))
print(forecast(VENhw_m2,h=5))

# Podemos ver que ambos metodos de Holt Winters se ajustan a los datos originales.

##################################################
#EJERCICIO 4

datos = read_excel("F:/777--Programacion repos/Una/r/data/04-t.xlsx")

Yts <- ts(datos$Ventas, start = c(2016,1), frequency = 4)
print(Yts)

#Graficar la serie temporal
plot(Yts,type = "l", xlab="Trimestres",ylab="Ventas")


# (a) Realice el pronóstico utilizando métodos de promedios móviles y determine que método es
# mejor, evaluar gráficamente y utilizando los estadísticos del error.

# Promedio móvil simple
k = 3
Yts_ma <- SMA(Yts,k)

# Graficar la serie temporal original con la media movil
plot(Yts, type = "l", xlab = "Trimestres", ylab = " ") # Serie original
lines(Yts_ma, type = "l", col = "red")

# Estadísticos del error
print('Error promedio móvil simple')
error <- Yts - Yts_ma
error_cuadrado <- error^2
mse <- mean(error_cuadrado, na.rm = TRUE)
print(paste("MSE:", mse))
rmse <- sqrt(mse)
print(paste("RMSE:", rmse))


# Promedio móvil doble

Yts_ma2 <- SMA(Yts_ma, k) # Media móvil de media móvil
a <- 2*Yts_ma - Yts_ma2
b <- (2/(k-1))*(Yts_ma - Yts_ma2)
p <- 5 # Periodo futuro a pronosticar
Yma2 <- a + b*p
print(Yma2)

# Graficar la serie original y la media móvil doble
plot(Yts, type = "l", xlab="Trimestres", ylab=" ") # Serie original
lines(Yma2, type = "l", col = "blue") # Media móvil doble

# Estadísticos del error

print('Error promedio móvil doble')
error_2 <- Yts - Yts_ma2
error_2_cuadrado <- error_2^2
mse_2 <- mean(error_2_cuadrado, na.rm = TRUE)
print(paste("MSE:", mse_2))
rmse_2 <- sqrt(mse_2)
print(paste("RMSE:", rmse_2))


# Grafica comparativa entre los 2 metodos con la serie original
plot(Yts, type = "l", xlab="Trimestres", ylab="Ventas") # Serie original
lines(Yts_ma, type = "l", col = "red") # Media móvil simple
lines(Yma2, type = "l", col = "blue") # Media móvil doble

# Concluimos que el método de promedio móvil doble es mejor que el método de promedio móvil simple,
# ya que el error cuadrático medio es menor en el método de promedio móvil doble.



# (b) Si se utiliza un suavizamiento exponencial simple con una constante de suavizamiento de 0.4,
# ¿Cuál es el pronóstico para el IV trimestre del 2020?

Yses <- ses(Yts, alpha = 0.4, h = 4)
plot(Yts, type = "l", xlab="Trimestre", ylab=" ")
lines(Yses$fitted, type = "l", col = "red")
legend(x = "bottomright", legend = c("Yts", "Yses"), col = c('black', 'red'), lty = c(1,
1))

Yses$fitted

# El pronóstico para el IV trimestre del 2020 es 338.0426

# (c) Utilice el método de suavizamiento multiplicativo de Holt-Winters con las constantes de
# suavizamiento alpha = beta = gamma = 0.5.

Yhw_m <- HoltWinters(Yts, seasonal = "multiplicative",optim.start = c(0.5,0.5,0.5))
plot(Yhw_m)
legend("topleft",legend = c("Original","Prediccion"),col = c("black","red"),lty = c(1,1))

# El metodo de Holt Winters se ajusta muy bien a los datos originales.

##################################################

#EJERCICIO 5

# La Tabla 5 indica el número de terremotos severos anuales (aquellos con una magnitud en la escala
# de Richter de 7 grados o más) de 1900 a 1999.

datos = read_excel("F:/777--Programacion repos/Una/r/data/05-t.xlsx")

Yts <- ts(datos$Numero, start = c(1900,1), frequency = 1)
print(Yts)

#Graficar la serie temporal
plot(Yts,type = "l", xlab="Años",ylab="Numero de terremotos")

# (a) Utilice R-Studio para suavizar los datos de terremotos con promedios móviles de órdenes de k
# = 5, 10 y 15. Describa la naturaleza de la suavización conforme el orden del promedio móvil se
# incrementa. ¿Cree usted que podría haber un ciclo en estos datos? Si es así, dé un estimado de
# la duración (en años) del ciclo.

# Promedio móvil simple k = 5
k = 5
Yts_ma <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma, type = "l", col = "red")

# Promedio móvil simple k = 10
k = 10
Yts_ma2 <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma2, type = "l", col = "blue")

# Promedio móvil simple k = 15
k = 15
Yts_ma3 <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma3, type = "l", col = "green")

# Grafica comparativa entre los 3 promedios móviles
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma, type = "l", col = "red")
lines(Yts_ma2, type = "l", col = "blue")
lines(Yts_ma3, type = "l", col = "green")
legend("topleft",legend = c("Original","Promedio móvil k=5","Promedio móvil k=10","Promedio móvil k=15"),col = c("black","red","blue","green"),lty = c(1,1))


# Se observa que conforme el orden del promedio móvil se incrementa, la suavización de los datos es mayor.
# En este caso, el promedio móvil de orden 15 es el que suaviza más los datos. Se observa que hay un ciclo 
# en los datos, ya que se observan picos y valles en la serie temporal. La duración del ciclo es de 
# aproximadamente 50 años.


# (b) Use R-Studio para suavizar los datos de los terremotos usando la suavización exponencial
# simple. Almacene los residuos y genere un pronóstico para el número de terremotos severos en
# el año 2000. ¿La suavización exponencial simple ofrece un ajuste razonable de estos datos?
# Explique.

# Suavización exponencial simple

Yses <- ses(Yts,alpha = 0.5,h = 1)

plot(Yts,type="l", xlab="Años",ylab=" ")
lines(Yses$fitted, type ="l", col="red")
legend(x = "bottomright", legend = c("Yts","Yses"),col = c('black','red'),lty = c(1,1))

# Residuos
residuos <- Yts - Yses$fitted
print(residuos)

# Pronósticos
plot(Yses)


# El pronóstico para el año 2000 es 20.64654. 
# La suavización exponencial simple ofrece un ajuste razonable de los datos, ya que los pronósticos
# se ajustan a la serie temporal. Se observa que los pronósticos siguen la tendencia de los datos 
# originales.


# (c) ¿Existe un componente cíclico en los datos del terremoto? ¿Por qué?

ggAcf(Yts)

# Se observa que no hay un componente cíclico en los datos del terremoto, ya que no hay picos 
# significativos en la función de autocorrelación. Por lo tanto, no hay un patrón cíclico en 
# los datos del terremoto.

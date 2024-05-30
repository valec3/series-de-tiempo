#EJERCICIO 1

#LIBRERIAS NECESARIA
library(ggplot2) #PARA HACER GRAFICOS
library(forecast) #PRONOSTICO
library(TTR) #metodo de media movil
library(readxl) #para leer archivos excel

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

# Grafico de la predicción
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

# Graficar la predicción
pred <- predict(Yma2, h=5)
summary(pred)
plot(pred, col="blue")



# Graficar la serie original y la media móvil doble
plot(Yts, type = "l", xlab="Meses", ylab="Ventas") # Serie original
lines(Yma2, type = "l", col = "blue") # Media móvil doble
legend(x = "bottomright", legend = c("Yts", "Yma2"), col = c('black', 'blue'), lty = c(1,
1))


#GRAFICA DE AMBOS METODOS con la serie original
plot(Yts, type='l', xlab="Años", ylab="Ventas")
lines(Yts_ma, type='l', col="red")
lines(Yma2, type='l', col="blue")
legend(x = "bottomright", legend = c("Yts_ma", "Yma2"), col = c('red', 'blue'), lty = c(1, 1))

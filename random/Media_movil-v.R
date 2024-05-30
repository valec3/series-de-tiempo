# Librerias necesarias
library(ggplot2)
library(forecast)
library(TTR)
library(readxl)
library(fpp2)
###########################################################

# Cargar datos:
# # file.choose() abre una ventana para seleccionar el archivo
datos = read_excel("F:/777--Programacion repos/Una/r/data/Ejem_2_3.xlsx")
View(datos)

#Convertir los datos a serie temporal
yts <- ts(datos$Y, start = c(2011,1), frequency = 12)
print(yts)

#Graficar la serie temporal
plot(yts,type = "l", xlab="Meses",ylab="Y")


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




##############################################
####        MEDIA MOVIL DOBLE
##############################################

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
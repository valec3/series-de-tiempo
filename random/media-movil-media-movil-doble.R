library(readxl)
datos <- read_excel("~/Descargas/Ejem_2_3.xlsx")
View(Ejem_2_1)   

library(ggplot2)
library(forecast)

#concertir los datos a serie temporal

yts <- ts(datos$Y, start = c(2011,1), frequency = 12)
print(yts)

#grafico de la serie

plot(yts,type = "l", xlab="Meses",ylab="Y")

library(TTR) #metodo de media movil
k <- 12 # perdiodo

yts_ma <- SMA(yts,k)
print(yts_ma)
lines(yts_ma,type="l", col = "blue")
legend(x = "bottomright",legend = c("yts","yts_ma"), col = c('black','red'), lty = c(1,1))

#prediccion
pred = predict(yts_ma,h=12)
#summary(pred), # muestra los pronosticos y medidas de error

pred
plot(pred,col="red")


##############################################
#MEDIA MOVIL DOBLE
##############################################
#library(TTR) #metodo de media movil
#k <- 12 # perdiodo
#yts_ma <- SMA(yts,k)
#print(yts_ma)
yts_ma2 = SMA(yts_ma,k)
#print(yts_ma2)

a <- 2*yts_ma - yts_ma2
b <- (2/(k-1))*(yts_ma - yts_ma2)

p <- 1
yma2 <- a + b*p


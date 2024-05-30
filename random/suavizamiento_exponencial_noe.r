#SUAVIZAMIENTO EXPONENCIAL
require(readxl)
require(ggplot2) #PARA HACER GRAFICOS
require(forecast) #PRONOSTICO

datos <- read_excel("D:/SERIES DE TIEMPO/Ejem_2_3.xlsx")
View(datos)

#Convertir los datos a Serie Temporal
Yts <- ts(datos$Y, start = c(2013,1), frequency = 12)
print(Yts)

#Grafico de la serie
plot(Yts, type="l", xlab = "Meses", ylab="Y")

##################################################
# SUAVIZAMIENTO EXPONENCIAL SIMPLE
##################################################
library(fpp2)
Yses <- ses(Yts, alpha = 0.5, h=12)

plot(Yts, type="l", xlab="Meses", ylab=" ")
lines(Yses$fitted, type="l", col="red")
legend(x="bottomright", legend = c("Yts", "Yses"), col=c("black", "red"), lty = c(1,1) )


summary(Yses)
plot(Yses, col="red")

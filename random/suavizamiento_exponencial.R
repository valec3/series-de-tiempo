library(readxl)
library(ggplot2)
library(fpp2)
datos <- read_excel("F:/777--Programacion repos/Una/r/data/Ejem_2_3.xlsx")
View(datos)

#convertir los datos a serie temporal
yts = ts(datos$Y, start = c(2013,1), frequency = 12)
print(yts)

#grafico de seria
plot(yts,type = "l", xlab="Meses",ylab="Y")

###########################
# SUAVIZAMIENTO EXPONENCIAL SIMPLE
###########################

yses <- ses(yts,alpha = 0.9,h = 12)

plot(yts,type="l", xlab="meses",ylab=" ")
lines(yses$fitted, type ="l", col="red")
legend(x = "bottomright", legend = c("yts","yses"),col = c('black','red'),lty = c(1,1))


summary(yses)
plot(yses,col="red")
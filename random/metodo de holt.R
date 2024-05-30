library(readxl)
datos <- read_excel("~/Descargas/Ejem_2_6.xlsx")
View(datos)

library(ggplot2)
library(forecast)

#concertir los datos a serie temporal

yts <- ts(datos$PBI_mm, start = c(1980,1), frequency = 1)
print(yts)

#grafico de la serie

plot(yts,type = "l", xlab="Meses",ylab="Y")

###########################################
#     METODO DE HOLT
###########################################

yholt <- holt(yts,h=5,alpha=0.4,beta = 0.07)

plot(yts, type ="l", xlab = "años", ylab=" ")










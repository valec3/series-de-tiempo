#LIBRERIAS
library(ggplot2)
library(forecast)
library(readxl)
#library(ggfortify)
#concertir los datos a serie temporal
serie <- read_excel("Descargas/Ejm_3_2.xlsx")
View(serie)

yt <- ts(serie$Y, start = c(2013,1), frequency = 12)
print(yt)
#grafico de la serie
plot(yt,type = "l")


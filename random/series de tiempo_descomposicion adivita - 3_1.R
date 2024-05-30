#LIBRERIAS
library(ggplot2)
library(forecast)
library(readxl)

#concertir los datos a serie temporal
serie <- read_excel("Descargas/Ejm_3_1.xlsx")
View(serie)

yt <- ts(serie$Y, start = c(2013,1), frequency = 12)
print(yt)
#grafico de la serie
plot(yt,type = "l")

#descomposicion de la seria - Aditivo
yt_a <- decompose(yt, type = "additive")
plot(yt_a,type="l")

print(yt_a)


datos = read_excel("F:/777--Programacion repos/Una/r/data/02-t.xlsx")

Yts <- ts(datos$Ventas, start = c(2015,1), frequency = 12)
print(Yts)

#Graficar la serie temporal original
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



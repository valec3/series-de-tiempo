datos = read_excel("F:/777--Programacion repos/Una/r/data/04-t.xlsx")

Yts <- ts(datos$Ventas, start = c(2016,1), frequency = 4)
print(Yts)

#Graficar la serie temporal
plot(Yts,type = "l", xlab="Trimestres",ylab="Ventas")


# (a) Realice el pronóstico utilizando métodos de promedios móviles y determine que método es
# mejor, evaluar gráficamente y utilizando los estadísticos del error.

# Promedio móvil simple
k = 2
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

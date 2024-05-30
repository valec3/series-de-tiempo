
datos = read_excel("F:/777--Programacion repos/Una/r/data/03-t.xlsx")

VENts <- ts(datos$Ventas, start = c(2010,1), frequency = 12)
print(VENts)

#Graficar la serie temporal
plot(VENts,type = "l", xlab="Meses",ylab="Ventas",col="black")

# HOLT WINTERS SIN PARAMETROS

VENhw_m <- HoltWinters(VENts,seasonal = "multiplicative")
plot(VENhw_m)
legend("topleft",legend = c("Original","Prediccion"),col = c("black","red"),lty = c(1,1))

VENhw_m #Muestra los valores de alpha, beta y gamma

# HOLT WINTERS CON PARAMETROS
VENhw_m2 <- HoltWinters(VENts,seasonal = "multiplicative",optim.start = c(0.4,0.6002,1))
plot(VENhw_m2)
legend("topleft",legend = c("Original","Prediccion"),col = c("black","red"),lty = c(1,1))


# Predicciones

autoplot(forecast(VENhw_m,h=5),xlab="Años",ylab="Ventas",col="red")
autoplot(forecast(VENhw_m2,h=5),xlab="Años",ylab="Ventas",col="red")


print(forecast(VENhw_m,h=5))
print(forecast(VENhw_m2,h=5))

# La Tabla 5 indica el número de terremotos severos anuales (aquellos con una magnitud en la escala
# de Richter de 7 grados o más) de 1900 a 1999.

datos = read_excel("F:/777--Programacion repos/Una/r/data/05-t.xlsx")

Yts <- ts(datos$Numero, start = c(1900,1), frequency = 1)
print(Yts)

#Graficar la serie temporal
plot(Yts,type = "l", xlab="Años",ylab="Numero de terremotos")

# (a) Utilice R-Studio para suavizar los datos de terremotos con promedios móviles de órdenes de k
# = 5, 10 y 15. Describa la naturaleza de la suavización conforme el orden del promedio móvil se
# incrementa. ¿Cree usted que podría haber un ciclo en estos datos? Si es así, dé un estimado de
# la duración (en años) del ciclo.

# Promedio móvil simple k = 5
k = 5
Yts_ma <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma, type = "l", col = "red")

# Promedio móvil simple k = 10
k = 10
Yts_ma2 <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma2, type = "l", col = "blue")

# Promedio móvil simple k = 15
k = 15
Yts_ma3 <- SMA(Yts,k)

# Graficar la serie temporal con la media movil
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma3, type = "l", col = "green")

# Grafica comparativa entre los 3 promedios móviles
plot(Yts, type = "l", xlab = "Años", ylab = "Terremotos") # Serie original
lines(Yts_ma, type = "l", col = "red")
lines(Yts_ma2, type = "l", col = "blue")
lines(Yts_ma3, type = "l", col = "green")
legend("topleft",legend = c("Original","Promedio móvil k=5","Promedio móvil k=10","Promedio móvil k=15"),col = c("black","red","blue","green"),lty = c(1,1))


# Se observa que conforme el orden del promedio móvil se incrementa, la suavización de los datos es mayor.
# En este caso, el promedio móvil de orden 15 es el que suaviza más los datos. Se observa que hay un ciclo 
# en los datos, ya que se observan picos y valles en la serie temporal. La duración del ciclo es de 
# aproximadamente 50 años.


# (b) Use R-Studio para suavizar los datos de los terremotos usando la suavización exponencial
# simple. Almacene los residuos y genere un pronóstico para el número de terremotos severos en
# el año 2000. ¿La suavización exponencial simple ofrece un ajuste razonable de estos datos?
# Explique.

# Suavización exponencial simple

Yses <- ses(Yts,alpha = 0.5,h = 1)

plot(Yts,type="l", xlab="Años",ylab=" ")
lines(Yses$fitted, type ="l", col="red")
legend(x = "bottomright", legend = c("Yts","Yses"),col = c('black','red'),lty = c(1,1))

# Residuos
residuos <- Yts - Yses$fitted
print(residuos)

# Pronósticos
plot(Yses)


# El pronóstico para el año 2000 es 20.64654. 
# La suavización exponencial simple ofrece un ajuste razonable de los datos, ya que los pronósticos
# se ajustan a la serie temporal. Se observa que los pronósticos siguen la tendencia de los datos 
# originales.


# (c) ¿Existe un componente cíclico en los datos del terremoto? ¿Por qué?

ggAcf(Yts)

# Se observa que no hay un componente cíclico en los datos del terremoto, ya que no hay picos 
# significativos en la función de autocorrelación. Por lo tanto, no hay un patrón cíclico en 
# los datos del terremoto.

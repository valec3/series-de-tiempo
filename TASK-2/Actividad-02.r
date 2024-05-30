################
# ACTIVIDAD 2  #
################

# EJERCICIO 1
#LIBRERIAS
library(ggplot2)
library(forecast)
library(readxl)
library(seasonal)
library(fastDummies)#creacion de variables dummies
library(scales) # formtato de fechas en el eje x
library(dplyr) #generar tablas de resumen

# A.1) tendencia: T = 2*t + 1

t <- 1:150
tendencia <- 2*t + 1
plot(tendencia, type = "l", xlab="t")

# A.2) estacional: Et = 30*sin((2*pi/12)*(t+1))+100

estacional <- 30*sin((2*pi/12)*(t+1))+100
plot(estacional, type = "l", xlab="t")

# A.3) irregular 
irregular = rnorm(length(t), 0,5)
plot(irregular, type = "l", xlab="t")


# simular los modelos: 
#modelo aditivo : 
ya <- tendencia + estacional + irregular
plot(ya,type="l", xlab="t")

# modelo mixto:
    # calcular irregular 
irregular = rnorm(length(t),0,500)

ym <- tendencia*estacional + irregular
plot(ym,type="l", xlab="t")

# b) tendencia exponencial
# tendencia: Tt = e^2t

t <- seq(0,2, length= length(t))
tendencia_e <- exp(2*t)
plot(tendencia_e, type = "l", xlab="t")

# Ejericio 2

datos = read_excel("F:/777--Programacion repos/Una/r/TASK-2/data/data.xlsx")
View(datos)
yts <- ts(datos$temperatura, start = c(2010,1), frequency = 12)
plot(yts, type = "l", xlab = "Meses", ylab = "Temperatura")

# (a) Realice la descomposición de los componentes de la serie de temperaturas.

decomp = decompose(yts,type = "additive")
plot(decomp, xlab = "Meses", ylab = "Temperatura")

# (b) ¿Cree que podría haber un componente cíclico en las temperaturas? Explique su respuesta.

# generacion de t
datos$t = seq(1:NROW(datos))

# Construir series
cosP <- cos(2*pi/120*12*datos$t)
senP <- sin(2 * pi / 120 * 12 * datos$t)

# Ajuste del modelo
ciclo <- lm(yts ~ cosP + senP)
plot(ciclo$residuals, type = "l", xlab="t", ylab="Residuos", col = "red")

# Se observa que el grafico de los residuos tiene un parecido a la función seno, por lo que se
# puede decir que hay un componente cíclico en las temperaturas.

# (c) ¿Utilizaría los componentes de tendencia o estacionalidad, o ambos para realizar el
# pronóstico en la previsión de las temperaturas de la década siguiente?

plot(decomp, xlab = "Meses", ylab = "Temperatura")

# Analisis de la estacionalidad
ggsubseriesplot(yts, xlab = "Meses", ylab = "Temperatura")
ggseasonplot(yts, xlab = "Meses", ylab = "Temperatura")
ggAcf(yts, xlab = "Meses", ylab = "Temperatura")

# Analisis de la tendencia
t <- time(yts)
Ylm <- lm(yts ~ t) # Modelo lineal simple Y = a + b*t
pron = ts(predict(Ylm, t), c(2010,1), frequency = 12)
plot(yts, xlab="Años", ylab=" ")
lines(pron, type = "l", col = "red")
legend(x = "bottomright", legend = c("Yts", "Ylm"), col = c('black', 'red'), lty = c(1,1))


# No se considera la tendencia, ya que se observa una tendencia muy pequeña en los datos. 
# Practicamente es constante.

# En cambio se considera la estacionalidad porque podemos detectarla gracias a los graficos usados.


# EJERCICIO 3

datos = read_excel("F:/777--Programacion repos/Una/r/TASK-2/data/data.xlsx", sheet = 2)
View(datos)
Yts <- ts(datos$Ventas, start = c(2019, 1), frequency = 4)
plot(Yts, type = "l", xlab = "Trimestre", ylab = "Ventas")

# (a) Grafique las observaciones contra el tiempo, ¿qué clase de tendencia parece haber?

t <- time(Yts)
Ylm <- lm(Yts ~ t) # Modelo lineal simple Y = a + b*t
pron = ts(predict(Ylm, t), c(2019, 1), frequency = 4)
plot(Yts, xlab = "Años", ylab = " ")
lines(pron, type = "l", col = "red")
legend(x = "bottomright", legend = c("Yts", "Ylm"), col = c("black", "red"), lty = c(1, 1))

# Se observa una tendencia creciente en las ventas.

# b) Si se supone que una tendencia lineal VENt = 0 + 1t, describe las observaciones, determine
# las estimaciones puntuales de mínimos cuadrados de 0 y 1.

print(Ylm)

# Revisando el modelo lineal simple, se tiene que la ecuación de la recta es:
# Y = -45.14621 + 0.03519*t
# Donde:
# Y = Ventas
# t = Trimestre
# Beta0 = -45.14621
# Beta1 = 0.03519

# (d) Determine a través de los métodos gráficos y estadísticos la existencia de los componentes de
# estacionalidad y ciclicidad.

# Comprobar la estacionalidad
ggsubseriesplot(Yts, xlab = "Trimestre", ylab = "Ventas")
ggseasonplot(Yts, xlab = "Trimestre", ylab = "Ventas")
ggAcf(Yts, xlab = "Trimestre", ylab = "Ventas")

# Comprobar la ciclicidad
datos$t = seq(1:NROW(datos))
cosP <- cos(2 * pi / 4 * datos$t)
senP <- sin(2 * pi / 4 * datos$t)
ciclo <- lm(Yts ~ cosP + senP)
plot(ciclo$residuals, type = "l", xlab = "Trimestre", ylab = "Residuos", col = "red")

# Se observa que hay un componente estacional en las ventas, ya que se observan picos y valles en la serie temporal.

# No se observa un componente cíclico en las ventas, ya que los residuos no tienen un comportamiento cíclico.

##########################
# EJERCICIO 4

datos = read_excel("F:/777--Programacion repos/Una/r/TASK-2/data/data.xlsx", sheet = 3)
View(datos)
Yts <- ts(datos$ventas, start = c(2010, 1), frequency = 4)
plot(Yts, type = "l", xlab = "Trimestre", ylab = "Ventas")


# ¿Parece que hay algún efecto estacional significativo de estos niveles de venta?

# Comprobar la estacionalidad
ggsubseriesplot(Yts, xlab = "Trimestre", ylab = "Ventas")
ggseasonplot(Yts, xlab = "Trimestre", ylab = "Ventas")
ggAcf(Yts, xlab = "Trimestre", ylab = "Ventas")

# Se observa que los 3 gráficos muestran un comportamiento NO estacional en las ventas.

datos$ven_1 = lag(datos$ventas)

datos$dummy = datos$trimestre
tabla_dummy = dummy_columns(datos, select_columns = c("dummy"), remove_first_dummy = TRUE)
head(tabla_dummy)

tabla_reg = tabla_dummy[, c("ventas", "ven_1", "dummy_2", "dummy_3", "dummy_4")]
reg = lm(datos$ventas ~ ., data = tabla_reg)
summary(reg)

# 4 Indices estacionales
mean_ventas = mean(datos$ventas)
mean_trim1 = mean(datos$ventas[datos$trimestre == 1])
mean_trim2 = mean(datos$ventas[datos$trimestre == 2])
mean_trim3 = mean(datos$ventas[datos$trimestre == 3])
mean_trim4 = mean(datos$ventas[datos$trimestre == 4])

indices = c(mean_trim1, mean_trim2, mean_trim3, mean_trim4) / mean_ventas
indices

# Los indices son: 0.9531114 1.0209111 1.0123360 1.0136415

# La magnitud del efecto estacional es relativamente pequeña, ya que los valores de los índices estacionales
# están cerca de 1. Esto significa que aunque hay un efecto estacional en las ventas, no es muy pronunciado.
# La empresa experimenta variaciones estacionales en las ventas, pero estas no son significativamente grandes
# en comparación con el nivel medio de ventas.


datos$t <- seq(1:NROW(datos))
cosP <- cos(2 * pi / 15 * datos$t)
senP <- sin(2 * pi / 15 * datos$t)
ciclo <- lm(Yts ~ cosP + senP)
plot(ciclo$residuals, type = "l", xlab = "Trimestre", ylab = "Residuos", col = "red")


# No se logra apreciar un componente cíclico en las ventas, 
# ya que los residuos no tienen un comportamiento cíclico.
# En cambio se observa una tendencia creciente en las ventas.
###############
# TAREA 2     #
###############

# EJERCICIO 1
#LIBRERIAS
library(ggplot2)
library(forecast)
library(readxl)
library(seasonal)
library(fastDummies) # Creación de variables dummies
library(scales) # Formato de fechas en el eje x
library(dplyr) # Generar tablas de resumen

# A.1) Tendencia: T = 2*t + 1

t_vals <- 1:150
trend <- 2 * t_vals + 1
plot(trend, type = "l", xlab="t")

# A.2) Estacional: Et = 30*sin((2*pi/12)*(t_vals+1)) + 100

seasonal <- 30 * sin((2*pi/12) * (t_vals + 1)) + 100
plot(seasonal, type = "l", xlab="t")

# A.3) Irregular 
noise <- rnorm(length(t_vals), 0, 5)
plot(noise, type = "l", xlab="t")

# Simular los modelos: 
# Modelo aditivo: 
y_add <- trend + seasonal + noise
plot(y_add, type="l", xlab="t")

# Modelo mixto:
# Calcular irregular 
noise_mixed <- rnorm(length(t_vals), 0, 500)

y_mult <- trend * seasonal + noise_mixed
plot(y_mult, type="l", xlab="t")

# b) Tendencia exponencial
# Tendencia: Tt = e^2t

t_vals_exp <- seq(0, 2, length= length(t_vals))
trend_exp <- exp(2 * t_vals_exp)
plot(trend_exp, type = "l", xlab="t")

# EJERCICIO 2

data = read_excel("F:/777--Programacion repos/Una/r/TASK-2/data/data.xlsx")
View(data)
temps <- ts(data$temperatura, start = c(2010, 1), frequency = 12)
plot(temps, type = "l", xlab = "Meses", ylab = "Temperatura")

# (a) Realice la descomposición de los componentes de la serie de temperaturas.

decomp_temps = decompose(temps, type = "additive")
plot(decomp_temps, xlab = "Meses", ylab = "Temperatura")

# (b) ¿Cree que podría haber un componente cíclico en las temperaturas? Explique su respuesta.

# Generación de t
data$t <- seq(1:NROW(data))

# Construir series
cos_component <- cos(2 * pi / 120 * 12 * data$t)
sin_component <- sin(2 * pi / 120 * 12 * data$t)

# Ajuste del modelo
cycle_model <- lm(temps ~ cos_component + sin_component)
plot(cycle_model$residuals, type = "l", xlab="t", ylab="Residuos", col = "blue")

# Se observa que el gráfico de los residuos muestra un comportamiento ciclico, por lo que se puede decir que hay un componente ciclico en las temperaturas.

# (c) ¿Utilizaría los componentes de tendencia o estacionalidad, o ambos para realizar el
# pronóstico en la previsión de las temperaturas de la década siguiente?

plot(decomp_temps, xlab = "Meses", ylab = "Temperatura")

# Análisis de la estacionalidad
ggsubseriesplot(temps, xlab = "Meses", ylab = "Temperatura")
ggseasonplot(temps, xlab = "Meses", ylab = "Temperatura")
ggAcf(temps, xlab = "Meses", ylab = "Temperatura")

# Análisis de la tendencia
t_time <- time(temps)
linear_model <- lm(temps ~ t_time) # Modelo lineal simple Y = a + b*t
predicted <- ts(predict(linear_model, t_time), c(2010, 1), frequency = 12)
plot(temps, xlab="Años", ylab="Temperatura")
lines(predicted, type = "l", col = "green")
legend(x = "bottomright", legend = c("Temps", "Predicted"), col = c('black', 'green'), lty = c(1,1))

# No se considera la tendencia

# EJERCICIO 3

data2 = read_excel("F:/777--Programacion repos/Una/r/TASK-2/data/data.xlsx", sheet = 2)
View(data2)
sales <- ts(data2$Ventas, start = c(2019, 1), frequency = 4)
plot(sales, type = "l", xlab = "Trimestre", ylab = "Ventas")

# (a) Grafique las observaciones contra el tiempo, ¿qué clase de tendencia parece haber?

t_time2 <- time(sales)
linear_model2 <- lm(sales ~ t_time2) # Modelo lineal simple Y = a + b*t
predicted2 <- ts(predict(linear_model2, t_time2), c(2019, 1), frequency = 4)
plot(sales, xlab = "Años", ylab = "Ventas")
lines(predicted2, type = "l", col = "purple")
legend(x = "bottomright", legend = c("Sales", "Predicted"), col = c("black", "purple"), lty = c(1, 1))

# Se observa una tendencia creciente en las ventas.

# b) Si se supone que una tendencia lineal VENt = β0 + β1t, describe las observaciones, determine
# las estimaciones puntuales de mínimos cuadrados de β0 y β1.

print(linear_model2)

# Revisando el modelo lineal simple, se tiene que la ecuación de la recta es:

# Beta0 = -45.14621
# Beta1 = 0.03519

# (d) Determine a través de los métodos gráficos y estadísticos la existencia de los componentes de
# estacionalidad y ciclicidad.

ggsubseriesplot(sales, xlab = "Trimestre", ylab = "Ventas")
ggseasonplot(sales, xlab = "Trimestre", ylab = "Ventas")
ggAcf(sales, xlab = "Trimestre", ylab = "Ventas")

# Comprobar la ciclicidad
data2$t <- seq(1:NROW(data2))
cos_comp <- cos(2 * pi / 4 * data2$t)
sin_comp <- sin(2 * pi / 4 * data2$t)
cycle_model2 <- lm(sales ~ cos_comp + sin_comp)
plot(cycle_model2$residuals, type = "l", xlab = "Trimestre", ylab = "Residuos", col = "red")

# Hay un componente estacional en las ventas.
# No se observa un componente cíclico en las ventas.

# EJERCICIO 4

data3 = read_excel("F:/777--Programacion repos/Una/r/TASK-2/data/data.xlsx", sheet = 3)
View(data3)
sales_ts <- ts(data3$ventas, start = c(2010, 1), frequency = 4)
plot(sales_ts, type = "l", xlab = "Trimestre", ylab = "Ventas")


# ¿Parece que hay algún efecto estacional significativo de estos niveles de venta?

ggsubseriesplot(sales_ts, xlab = "Trimestre", ylab = "Ventas")
ggseasonplot(sales_ts, xlab = "Trimestre", ylab = "Ventas")
ggAcf(sales_ts, xlab = "Trimestre", ylab = "Ventas")

# No se observa estacionalidad en las ventas.

data3$sales_lag = lag(data3$ventas)

data3$seasonal_dummy = data3$trimestre
dummy_table = dummy_cols(data3, select_columns = c("seasonal_dummy"), remove_first_dummy = TRUE)
head(dummy_table)

regression_table = dummy_table[, c("ventas", "sales_lag", "seasonal_dummy_2", "seasonal_dummy_3", "seasonal_dummy_4")]
reg_model = lm(data3$ventas ~ ., data = regression_table)
summary(reg_model)

# 4 Índices estacionales
mean_sales = mean(data3$ventas)
mean_q1 = mean(data3$ventas[data3$trimestre == 1])
mean_q2 = mean(data3$ventas[data3$trimestre == 2])
mean_q3 = mean(data3$ventas[data3$trimestre == 3])
mean_q4 = mean(data3$ventas[data3$trimestre == 4])

seasonal_indices = c(mean_q1, mean_q2, mean_q3, mean_q4) / mean_sales
seasonal_indices

# Los índices son: 0.9531114 1.020

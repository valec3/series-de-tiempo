library(foreach)  # Pronosticos y otros
library(urca)     # Prueba de Raiz unitaria
library(tseries)  # Serie de tiempo
library(readxl)   # Leer archivos de excel

# Lecturas diarias de viscosidad del producto químico XB-77-5.
data = read_excel("F:\\777--Programacion repos\\Una\\r\\data\\tarea-arima.xlsx")

ts_data = ts(data$yt,start = c(1,1),frequency=1)
plot(ts_data, main = "Viscosidad del producto XB-77-5", ylab = "Viscosidad", xlab = "Días")
abline(h = mean(ts_data), col = "red")
# Gráficos de ACF y PACF
par(mfrow = c(1, 1))
acf(ts_data, lag.max = 16, main = "ACF")
pacf(ts_data, lag.max = 16, main = "PACF")

# Prueba de Dickey-Fuller
data_adf <- ur.df(ts_data, type="drift", lags = 0)
summary(data_adf)


# Probando modelos
modelo_ar2 = arma(ts_data, order = c(2,0))
summary(modelo_ar2)

modelo_ma1 = arma(ts_data, order = c(0,1))
summary(modelo_ma1)

# PROBAMOS EL MODELO ARMA(2,1)
modelo_fnl = arma(ts_data, order = c(2,1))
summary(modelo_fnl)

# No parece haber mejora
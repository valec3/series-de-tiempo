# Librerias necesaria
library(forecast) # Modelo ARIMA
library(tseries) # Para series de tiempo
library(TSA) # Para series de tiempo
library(urca) # Raiz Unitaria 
library(ggplot2) # Para hacer gráficos
library(gridExtra)
library(dplyr) # Para la manipulación de datos
library(lmtest) # Inferencia para coeficientes estimados
library(MASS) # Transformacion de Box-Cox
library(nortest) # Pruebas de normalidad
library(strucchange) # Cambio estructural - Test de Chow
library(mFilter)
library(readxl)
library(fitdistrplus)

data <- read_excel("F:\\777--Programacion repos\\Una\\r\\data\\actividad-04.xlsx",sheet = "datos2")
# Gráfica de la serie
data_ts <- ts(data$Produccion, start = c(1961,1), frequency = 1)
plot(data_ts, xlab="Años", ylab="Produccion")


#Análisis de tendencia
lambda_hp <- 1000
data_hp <- hpfilter(data_ts, type="lambda", freq=lambda_hp)
plot(data_hp)

#Estacionaridad en varianza
Grupo <- rep(1:9, each = 6)
Grupo <- c(Grupo, rep(x = 10,5))
boxplot(data$Produccion ~ Grupo, xlab = "Año", ylab = "Producción", main = "Distribución de Producción por Año")

b = BoxCox.ar(data_ts)
lambda <- b$mle
round(lambda,0)


T4.Yt <- log(data_ts)
qqnorm(T4.Yt, main = "Lambda = 0")
qqline(T4.Yt)

qqnorm(data_ts, main = "Lambda = 1") # Yt: Original
qqline(data_ts)



FAS <- acf(data_ts, lag.max = 15, main="FAS - Yt", level = 0.95)
FAP <- pacf(data_ts, lag.max = 15, main="FAP - Yt", level = 0.95)


# Verificación con la prueba de Raíz unitaria de Dickey-Fuller Aumentada.
data_adf <- ur.df(data_ts, type="trend", lags = 1)
summary(data_adf)

#diff

#PRIMERA DIFERENCIA
data_diff <- diff(data_ts)
plot(data_diff, xlab = "Años", ylab="Produccion")


#Correlograma de la 1ra diferencia
par(mfrow =c(1,2))
FAS <- acf(data_diff, lag.max=15, main="Fas", level = 0.95)
FAP <- pacf(data_diff, lag.max=15, main="Fap", level =0.95)


data_adf <- ur.df(data_diff, type="drift", lags = 1)
summary(data_adf)






par(mfrow =c(1,1))
plot(data_diff, xlab="Años", ylab="Produccion")
abline(h = mean(data_diff), col = "red")


#incluir el intercepto
Z <- mean(data_diff)
Co <- var(data_diff)
Tn <- length(data_ts)
Ta <- Tn - 1
Sigma <- Co/Ta
t <- Z/Sigma
tt <- qt(1-0.05/2,Ta-1)
pruebaT <- c(t, tt)
names(pruebaT) <- c("t-calculado","t-critico")
pruebaT

# No incluimos la constante


#ARIMA(1,1,0)
mod1 <- Arima(data_ts, order = c(1,1,0))
coeftest(mod1)


#ARIMA(0,1,3)
mod2 <- Arima(data_ts, order = c(0,1,2))
coeftest(mod2)


#ARIMA(1,1,1)
mod3 <- Arima(data_ts, order = c(1,1,2))
coeftest(mod3)



vcov(mod1)
vcov(mod2)
vcov(mod3)

mod1 = Arima(data_diff, order = c(1,1,0))
autoplot(mod1)
mod2 = Arima(data_diff, order = c(0,1,2))
autoplot(mod2)
mod3 = Arima(data_diff, order = c(1,1,2))
autoplot(mod3)


Chow_mod1 <- Fstats(mod1$fitted ~ 1, from = 0.40)
sctest(Chow_mod1)

Chow_mod2 <- Fstats(mod2$fitted ~ 1, from = 0.40)
sctest(Chow_mod2)

Chow_mod3 <- Fstats(mod3$fitted ~ 1, from = 0.40)
sctest(Chow_mod3)



plot(mod1$residuals)
abline(h = 0, col = "red")
t.test(mod1$residuals, mu = 0)

plot(mod2$residuals)
abline(h = 0, col = "red")
t.test(mod2$residuals, mu = 0)

plot(mod3$residuals)
abline(h = 0, col = "red")
t.test(mod3$residuals, mu = 0)


par(mfrow = c(3,1))
scatter.smooth(sqrt(abs(mod1$residuals)), lpars=list(col=2), main = "Modelo 1")
scatter.smooth(sqrt(abs(mod2$residuals)), lpars=list(col=2), main = "Modelo 2")
scatter.smooth(sqrt(abs(mod3$residuals)), lpars=list(col=2), main = "Modelo 3")


obs=get(mod1$series)
bptest(resid(mod1)~I(obs-resid(mod1)))

obs=get(mod2$series)
bptest(resid(mod2)~I(obs-resid(mod2)))

obs=get(mod3$series)
bptest(resid(mod3)~I(obs-resid(mod3)))





resid_m1 <- as.vector(mod1$residuals)
resid_m2 <- as.vector(mod2$residuals)
resid_m3 <- as.vector(mod3$residuals)
par(mfrow = c(1,3))
FAS_e.m1 <- acf(resid_m1, lag.max = 25,
                main="FAS Modelo 1", level = 0.95)
FAS_e.m2 <- acf(resid_m2, lag.max = 25,
                main="FAS Modelo 2", level = 0.95)
FAS_e.m3 <- acf(resid_m3, lag.max = 25,
                main="FAS Modelo 3", level = 0.95)


Box.test(resid_m1,type = "Ljung-Box")
Box.test(resid_m2,type = "Ljung-Box")
Box.test(resid_m3,type = "Ljung-Box")

ajuste_m1<-fitdist(data = resid_m1, distr="norm")
plot(ajuste_m1)
JB_m1 <- jarque.bera.test(resid_m1)
JB_m1


ajuste_m2<-fitdist(data = resid_m2, distr="norm")
plot(ajuste_m2)
JB_m2 <- jarque.bera.test(resid_m2)
JB_m2

ajuste_m3<-fitdist(data = resid_m3, distr="norm")
plot(ajuste_m3)
JB_m3 <- jarque.bera.test(resid_m3)
JB_m3

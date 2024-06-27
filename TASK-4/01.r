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

data <- read_excel("F:\\777--Programacion repos\\Una\\r\\data\\actividad-04.xlsx",sheet = "datos1")
View(data)
# Gráfica de la serie
data_ts <- ts(data$Ventas, start = c(1981,1), frequency = 52)
plot(data_ts, xlab="Semanas", ylab="Ventas")


#Descomposición de la serie
data_des <- decompose(data_ts, type = "additive")
plot(data_des,type="l")

#Graficos de la serie para identificar estacionalidad
plot1 <- ggsubseriesplot(data_ts, xlab = "Semanas", ylab = "Ventas")
plot2 <- ggseasonplot(data_ts, xlab = "Semanas", ylab = "Ventas")
plot3 <- ggAcf(data_ts, xlab = "Semanas", ylab = "Ventas")

grid.arrange(plot1, plot2, plot3, ncol = 1)



#Análisis de tendencia
lambda_hp <- 129600  
data_hp <- hpfilter(data_ts, type="lambda", freq=lambda_hp)
plot(data_hp)

#Estacionaridad en varianza
Grupo <- rep(1:13, each = 8)
boxplot(data$Ventas ~ Grupo, xlab = "Grupo", ylab="Yt")



lambda <- BoxCox.lambda(data_ts)
round(lambda,2)

qqnorm(data_ts, main = "Lambda = 1") # Yt: Original
qqline(data_ts)


FAS <- acf(data_ts, lag.max = 15, main="FAS - Yt", level = 0.95)
FAP <- pacf(data_ts, lag.max = 15, main="FAP - Yt", level = 0.95)


# Verificación con la prueba de Raíz unitaria de Dickey-Fuller Aumentada.
data_adf <- ur.df(data_ts, type="drift", lags = 1)
summary(data_adf)

#incluir el intercepto
Z <- mean(data_ts)
Co <- var(data_ts)
Tn <- length(data_ts)
Ta <- Tn - 1
Sigma <- Co/Ta
t <- Z/Sigma
tt <- qt(1-0.05/2,Ta-1)
pruebaT <- c(t, tt)
names(pruebaT) <- c("t-calculado","t-critico")
pruebaT

# No incluimos la constante


mod1 <- Arima(data_ts, order = c(1, 0, 0), include.constant = FALSE)
coeftest(mod1)

mod2 <- Arima(data_ts, order = c(0, 0, 2), include.constant = FALSE)
coeftest(mod2)

mod3 <- Arima(data_ts, order = c(1, 0, 2), include.constant = FALSE)
coeftest(mod3)


vcov(mod1)
vcov(mod2)
vcov(mod3)

autoplot(mod1)
autoplot(mod2)
autoplot(mod3)


Chow_mod1 <- Fstats(mod1$fitted ~ 1, from = 0.49)
sctest(Chow_mod1)

Chow_mod2 <- Fstats(mod2$fitted ~ 1, from = 0.49)
sctest(Chow_mod2)

Chow_mod3 <- Fstats(mod3$fitted ~ 1, from = 0.49)
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
par(mfrow = c(1,1))
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

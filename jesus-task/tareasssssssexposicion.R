# Librer?as
library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)
library(TSA)
library(urca)
library(lmtest)
library(MASS)
library(nortest)
library(mFilter)
library(strucchange)
library(fitdistrplus)
library(readxl)

SERIE <- read_excel("F:\\777--Programacion repos\\Una\\r\\jesus-task\\TAREA3__2.xlsx")
  

# IDENTIFICACION
# Grafica de la serie
Yt <- ts(SERIE$DATOS, start = 1999, frequency = 4)
plot(Yt, xlab = "A?o", ylab = "INTERESES PAGADOS")

# UTILIZAREMOS EL FILTRO DE HODRIK- PRESCOT
lambda_hp = 100 # para datos anuales
Yt_hp <- hpfilter(Yt, type = "lambda", freq = lambda_hp)
plot(Yt_hp)


##########################################################
# estacionariedad en varianza
###########################################################
Grupo <- rep(1:8, each = 7)
boxplot(SERIE$DATOS~Grupo, xlab=  "grupo", Ylab="Yt")

#CORRECCION DE LA ESTACIONARIEDAD EN VARIANZA

# 1) para lambda = 1 <- Yt
qqnorm(Yt, main ="lambda = 1") 
qqline(Yt)

# 2) para lambda = 2 <- Yt^2
T1.Yt <- Yt^2
qqnorm(T1.Yt, main ="lambda = 2") 
qqline(T1.Yt)

# 3) para lambda = 0.5 <- RAIZ 2
T3.Yt <- sqrt(Yt)
qqnorm(T3.Yt, main ="lambda = 0.5") 
qqline(T3.Yt)

# 4) para lambda = 0
T4.Yt <- log(Yt)
qqnorm(T4.Yt, main = "Lambda = 0")
qqline(T4.Yt)

# 5) Para lambda = -0.5
T5.Yt <- 1/sqrt(Yt)
qqnorm(T5.Yt, main = "Lambda = -0.5")
qqline(T5.Yt)

# 6)Para lambda = -1
T6.Yt <- 1/Yt
qqnorm(T6.Yt, main = "Lambda = -1")
qqline(T6.Yt)

# 7) Para lambda = -2
T7.Yt <- 1/(Yt^2)
qqnorm(T7.Yt, main = "Lambda = -2")
qqline(T7.Yt)

# determinacion de valor de lambda
b <- BoxCox.ar(Yt)

# lambda optimo
lambda <- b$mle
round(lambda,1)

# serie transfomormada T.Yt = 
T.Yt <- log(Yt)
plot(T.Yt, xlab = "A?OS", ylab="Log(Yt)") 

########################################
#ESTACIONARIEDAD EN MEDIA
#######################################
# CORREOLOGRAMA
FAS <- acf(T.Yt, lag.max = 15, main = "FAS - T.Yt", level = 0.05)
# MA()

FAP <- pacf(T.Yt, lag.max = 15, main = "FAP - T.Yt", level = 0.05) # tiene que ser menor a 9.0
#AR()

# PRUEBA DE RAIZ UNITARIA DIKEN FULER
TY_dfa <- ur.df(T.Yt, type = "trend", lags = 1)
summary(TY_dfa)


# SI ES NO ESTACIONARIA DIFERENCIAMOS LA SERIE
DT.Yt <- diff(T.Yt)
plot(DT.Yt, xlab = "TIEMPO")

# verificando la estacionalidad 
FAS <- acf(DT.Yt, lag.max = 15, main = "FAS D1Y", level = 0.95)
FAP <-pacf(DT.Yt, lag.max = 15, main = "FAP D1Y", level = 0.95)

# PRUEBA DE RAIZ UNITARIA 
Ydfa1 <- ur.df(DT.Yt,type = "drift", lags = 1)
summary(Ydfa1)

# ar(1) <- ARIMA(1,1,0)
# ma(1) <- ARIMA(0,1,1)

# Modelos ARIMA
modelo1 <- Arima(T.Yt, order = c(1, 1, 0), include.constant = TRUE)
coeftest(modelo1)
modelo2 <- Arima(T.Yt, order = c(0, 1, 1), include.constant = TRUE)
coeftest(modelo2)


# Validaci?n de modelos
vcov(modelo1)
vcov(modelo2)


mod1 <- Arima(T.Yt, order = c(1, 1, 0), include.constant = TRUE)
autoplot(mod1)
mod2 <- Arima(T.Yt, order = c(0, 1, 1), include.constant = TRUE)
autoplot(mod2)


# Test de Chow
Chow_mod1 <- Fstats(mod1$fitted ~ 1, from = 0.43)
sctest(Chow_mod1)

Chow_mod2 <- Fstats(mod2$fitted ~ 1, from = 0.43)
sctest(Chow_mod2)

# An?lisis de los residuos
plot(mod1$residuals, main = "MODELO 1")
abline(h = 0, col = "red")
t.test(mod1$residuals, mu = 0)

plot(mod2$residuals, main = "MODELO 2")
abline(h = 0, col = "blue")
t.test(mod2$residuals, mu = 0)



# Varianza constante de residuos
scatter.smooth(sqrt(abs(mod1$residuals)), tpars = list(col ~ 2), main = "MODELO 1")
scatter.smooth(sqrt(abs(mod2$residuals)), tpars = list(col ~ 2), main = "MODELO 2")

obs = get(mod1$series)
bptest(resid(mod1) ~ I(obs - resid(mod1)))

obs = get(mod2$series)
bptest(resid(mod2) ~ I(obs - resid(mod2)))


# Ausencia de correlaci?n serial
resid_m1 <- as.vector(mod1$residuals)
resid_m2 <- as.vector(mod2$residuals)


FAS_e.m1 <- acf(resid_m1, lag.max = 25, main = "FAS Modelo 1", level = 0.95)
FAS_e.m2 <- acf(resid_m2, lag.max = 25, main = "FAS Modelo 2", level = 0.95)

Box.test(resid_m1, lag = 3, type = "Ljung-Box")
Box.test(resid_m2, lag = 3, type = "Ljung-Box")

# Distribuci?n de residuos
library(fitdistrplus)
ajuste_m1<-fitdist(data = resid_m1, distr="norm")
plot(ajuste_m1)
JB_m1 <- jarque.bera.test(resid_m1)#Prueba de normalidad de Jarque-bera
JB_m1

ajuste_m2<-fitdist(data = resid_m2, distr="norm")
plot(ajuste_m2)
JB_m2 <- jarque.bera.test(resid_m2)
JB_m2


#########################3
# PRONOSTICO
#########################
Pron1 <- forecast(mod1, level = c(95), h=10)
plot(Pron1)
summary(Pron1)


Yt_arima1 <- exp(mod1$fitted)
grafico_comparativo <- cbind(Yt, Yt_arima1)
ts.plot(grafico_comparativo, col = c(1:2), lwd = 1)
legend("topleft", c("DT.Yt", "Yest"), lty = c(1,1), col = c(1:2), lwd = 2)

Pron1$mean <- exp(Pron1$mean)
Pron1$lower <- exp(Pron1$lower)

Pron1$upper <- exp(Pron1$upper)
Pron1$x <- exp(Pron1$x)
Pron1$fitted <- exp(Pron1$fitted)
Pron1$residuals <- exp(Pron1$residuals)
summary(Pron1)


plot(Pron1, shaded = FALSE, xlab = "A?os", ylab = "INTERESES PAGADOS", main = "ARIMA(1,1,0)")
lines(Pron1$fitted, col = "red")
legend("topleft", legend = c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),
       col = c("black", "blue", "black", "red"), lty = c(1, 1, 2, 1), lwd = 2, cex = 0.6)
abline(v = 1930, lwd = 1, col = "green")


#########################################################################################3
# Modelo 2
Pron2 <- forecast(mod2, level = c(95), h = 10)
summary(Pron2)

Yt_arima2 <- exp(mod2$fitted)
grafico_comparativo <- cbind(Yt, Yt_arima2)
ts.plot(grafico_comparativo, col = c(1:2), lwd = 1)
legend("topleft", c("Yt", "Yest"), lty = c(1, 1), col = c(1:2), lwd = 2)

Pron2$mean <- exp(Pron2$mean)
Pron2$lower <- exp(Pron2$lower)
Pron2$upper <- exp(Pron2$upper)
Pron2$x <- exp(Pron2$x)
Pron2$fitted <- exp(Pron2$fitted)
Pron2$residuals <- mod2$residuals  
summary(Pron2)

plot(Pron2, shaded = FALSE, xlab = "A?os", ylab = "N? DE PIELES", main = "ARIMA(0,0,2)")
lines(Pron2$fitted, col = "red")
legend("topleft", legend = c("SERIE", "PREDICCION", "INTERVALO DE CONFIANZA AL 95%", "AJUSTE"),
       col = c("black", "blue", "black", "red"), lty = c(1, 1, 2, 1), lwd = 2, cex = 0.6)
abline(v = 1930, lwd = 1, col = "green")


#Error medio absoluto escalado (MASE - Mean Absolute Scaled Error)
accuracy(Pron1)
accuracy(Pron2)


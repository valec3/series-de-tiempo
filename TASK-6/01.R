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

data <- read_excel("F:\\777--Programacion repos\\Una\\r\\data\\actividad-06.xlsx",sheet = "01")
View(data)
# Gráfica de la serie
data_ts <- ts(data$Yt, start = c(1872,1), frequency = 1)
plot(data_ts, xlab="Años", ylab="PRODUCCIÓN DE TABACO")

#Haciendo un analisis inicial podemos ver una clara tendencia creciente y a su vez la serie no aparenta ser estacionaria


#Descomposición de la serie
data_des <- decompose(data_ts, type = "additive")
plot(data_des,type="l")



#Graficos de la serie para identificar estacionalidad
plot1 <- ggsubseriesplot(data_ts, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO")
plot2 <- ggseasonplot(data_ts, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO")
plot3 <- ggAcf(data_ts, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO")
grid.arrange(plot1, plot2, plot3, ncol = 1)


###
#Dado que la serie es anual no podemos realizar un analisis de estacionalidad
##


#Análisis de tendencia
lambda_hp <- 1000  
data_hp <- hpfilter(data_ts, type="lambda", freq=lambda_hp)
plot(data_hp)

#Volvemos a reafirmar que la serie tiene una tendencia creciente y no se ve un patron ciclico claro



#ESTACIONARIEDAD
#Estacionaridad en varianza
Grupo <- rep(1:9, each = 6)
Grupo <- c(Grupo, rep(10,5))
boxplot(data$Producción ~ Grupo, xlab = "Grupo", ylab="Yt")

#Vemos claramente que la serie no es estacionaria en varianza


par(mfrow = c(1,1))
b <- BoxCox.ar(data_ts)
lambda <- b$mle
round(lambda,2)



##############################################
#######        TRANSFORMACION
##############################################
par(mfrow = c(2,3))
#lambda  = 1 <- yt
qqnorm(data_ts,main="lambda = 1")
qqline(data_ts)


#lambda =2 <- yt^2
t1.yt <- data_ts^2
qqnorm(t1.yt, main="lambda = 2")
qqline(t1.yt)

#lambda = 0.5 <- aiz(YT)
t3.yt <- sqrt(data_ts)
qqnorm(t3.yt, main="lambda = 0.5")
qqline(t3.yt)

#lambda  =  0 <- log(Yt)

t4.yt <- log(data_ts)
qqnorm(t4.yt, main="lambda = 0")
qqline(t4.yt)


t5.yt <- 1/sqrt(data_ts)
qqnorm(t5.yt, main="lambda = -0.5")
qqline(t5.yt)

#lambda  =  -1 <- 1/Yt

t6.yt <- 1/data_ts
qqnorm(t6.yt, main="lambda = -1")
qqline(t6.yt)


#lambda  = -2 <- 1/(Yt^2)
t7.yt <- 1/(data_ts^2)
qqnorm(t7.yt, main="lambda = -2")
qqline(t7.yt)
#################################################

data_tf <- log(data_ts)
plot(data_tf)


#Vemos que no hay una mejora significativa al transformar la serie, por lo que tomamos la desicion de NO transformar la serie











par(mfrow = c(1,2))
FAS <- acf(data_tf, lag.max = 15, main="FAS - Yt")
FAP <- pacf(data_tf, lag.max = 15, main="FAP - Yt")
FAP$acf[1]



# Verificación con la prueba de Raíz unitaria de Dickey-Fuller Aumentada.
data_adf <- ur.df(data_tf, type="trend", lags = 1)
summary(data_adf)




#DIFERENCIACION POR NO ESTACIONARIEDAD 
par(mfrow = c(1,1))

data_diff = diff(data_tf)
plot(data_diff, xlab="Años", ylab="PRODUCCIÓN DE TABACO")
abline(h = mean(data_diff), col = "red")

par(mfrow = c(1,2))
FAS <- acf(data_diff, lag.max = 20, main="FAS - diff Yt")
FAP <- pacf(data_diff, lag.max = 20, main="FAP - diff Yt")
FAP$acf[1]

data_adf <- ur.df(data_diff, type="drift", lags = 1)
summary(data_adf)



#incluir el intercepto
Z <- mean(data_diff)
Co <- var(data_diff)
Tn <- length(data_diff)
Ta <- Tn - 1
Sigma <- Co/Ta
t <- Z/Sigma
tt <- qt(1-0.05/2,Ta-1)
pruebaT <- c(t, tt)
names(pruebaT) <- c("t-calculado","t-critico")
pruebaT

#  Incluimos la constante

par(mfrow = c(1,2))
FAS <- acf(data_diff, lag.max = 15, main="FAS - data diferenciada")
FAP <- pacf(data_diff, lag.max = 15, main="FAP - data diferenciada")

#Segun el FAS plantearemos un MA(1) dado que solo hay coeficiente que es ligeramente significativo
#Y segun el FAP plantearemos un AR(1) ya que tiene solo un coeficiente que es ligeramente significativo




mod1 <- Arima(data_tf, order = c(2, 1, 0), include.constant = T)
coeftest(mod1)

mod2 <- Arima(data_tf, order = c(0, 1, 1), include.constant = T)
coeftest(mod2)

mod3 <- Arima(data_tf, order = c(1, 1, 2), include.constant = T)
coeftest(mod3)


vcov(mod1)
vcov(mod2)
vcov(mod3)


#Convergencia e invertibilidad
autoplot(mod1)
autoplot(mod2)
autoplot(mod3)


#Analisis de estabilidad
Chow_mod1 <- Fstats(mod1$fitted ~ 1, from = 0.30,to=0.60)
sctest(Chow_mod1)

Chow_mod2 <- Fstats(mod2$fitted ~ 1, from = 0.30,to=0.60)
sctest(Chow_mod2)

Chow_mod3 <- Fstats(mod3$fitted ~ 1, from = 0.30,to=0.60)
sctest(Chow_mod3)




#3.2 Análisis de los residuos  
#3.2.1 Media es igual a cero

par(mfrow = c(1,1))
plot(mod1$residuals)
abline(h = 0, col = "red")
t.test(mod1$residuals, mu = 0)

plot(mod2$residuals)
abline(h = 0, col = "red")
t.test(mod2$residuals, mu = 0)

plot(mod3$residuals)
abline(h = 0, col = "red")
t.test(mod3$residuals, mu = 0)


#3.2.2 Homocedasticidad o varianza constante
par(mfrow = c(3,1))
scatter.smooth(sqrt(abs(mod1$residuals)), lpars=list(col=2), main = "Modelo 1")
scatter.smooth(sqrt(abs(mod2$residuals)), lpars=list(col=2), main = "Modelo 2")
scatter.smooth(sqrt(abs(mod3$residuals)), lpars=list(col=2), main = "Modelo 3")

#Prueba de Breusch - Pagan
obs=get(mod1$series)
bptest(resid(mod1)~I(obs-resid(mod1)))

obs=get(mod2$series)
bptest(resid(mod2)~I(obs-resid(mod2)))

obs=get(mod3$series)
bptest(resid(mod3)~I(obs-resid(mod3)))




#Correlograma de los residuos
resid_m1 <- as.vector(mod1$residuals)
resid_m2 <- as.vector(mod2$residuals)
resid_m3 <- as.vector(mod3$residuals)
par(mfrow = c(3,1))
FAS_e.m1 <- acf(resid_m1, lag.max = 25,
                main="FAS Modelo 1")
FAS_e.m2 <- acf(resid_m2, lag.max = 25,
                main="FAS Modelo 2")
FAS_e.m3 <- acf(resid_m3, lag.max = 25,
                main="FAS Modelo 3")




Box.test(resid_m1,type = "Ljung-Box")
Box.test(resid_m2,type = "Ljung-Box")
Box.test(resid_m3,type = "Ljung-Box")



#Prueba de normalidad
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



#######################################################
#Pronostico
#######################################################

par(mfrow = c(1,1))
# modelo 1
#pronostico de la serie  zt
Pron1 <- forecast(mod1,level=c(95),h=10)
plot(Pron1)
summary(Pron1)

#serie original y valores estimados
data_modelo1 <- exp(mod1$fitted)
grafico_comparativo <- cbind(data_ts,data_modelo1)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2,col=c("black", "red"))



Pron1$mean <- exp(Pron1$mean)
Pron1$lower <- exp(Pron1$lower)
Pron1$upper <- exp(Pron1$upper)
Pron1$x <- exp(Pron1$x)
Pron1$fitted <- exp(Pron1$fitted)
Pron1$residuals <- exp(Pron1$residuals)
summary(Pron1)


plot(Pron1, shaded = FALSE, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO",main = "ARIMA(2,1,0)")
lines(Pron1$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=1985, lwd = 1, col="green")


###
Pron2 <- forecast(mod2,level=c(95),h=10)
plot(Pron2)
summary(Pron2)

#serie original y valores estimados
data_modelo2 <- exp(mod2$fitted)
grafico_comparativo <- cbind(data_ts,data_modelo2)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2,col=c("black", "red"))



Pron2$mean <- exp(Pron2$mean)
Pron2$lower <- exp(Pron2$lower)
Pron2$upper <- exp(Pron2$upper)
Pron2$x <- exp(Pron2$x)
Pron2$fitted <- exp(Pron2$fitted)
Pron2$residuals <- exp(Pron2$residuals)
summary(Pron2)


plot(Pron2, shaded = FALSE, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO",main = "ARIMA(0,1,1)")
lines(Pron2$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=1985, lwd = 1, col="green")


###
Pron3 <- forecast(mod3,level=c(95),h=10)
plot(Pron3)
summary(Pron3)

#serie original y valores estimados
data_modelo3 <- exp(mod3$fitted)
grafico_comparativo <- cbind(data_ts,data_modelo3)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2,col=c("black", "red"))



Pron3$mean <- exp(Pron3$mean)
Pron3$lower <- exp(Pron3$lower)
Pron3$upper <- exp(Pron3$upper)
Pron3$x <- exp(Pron3$x)
Pron3$fitted <- exp(Pron3$fitted)
Pron3$residuals <- exp(Pron3$residuals)
summary(Pron3)


plot(Pron3, shaded = FALSE, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO",main = "ARIMA(1,1,1)")
lines(Pron3$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2020, lwd = 1, col="green")














#######################################
#######################################
#######################################
#######################################
par(mfrow = c(1,1))
data_modelo1 <- (mod1$fitted)
grafico_comparativo <- cbind(data_ts,data_modelo1)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2,col=c("black", "red"))


#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron1, shaded = FALSE, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO",main = "ARIMA(1,1,0)")
lines(Pron1$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006.7, lwd = 1, col="green")



# modelo 2
#pronostico de la serie  zt
Pron2 <- forecast(mod2,level=c(95),h=10)
plot(Pron2)
summary(Pron2)

#serie original y valores estimados

data_modelo2 <- (mod2$fitted)
grafico_comparativo <- cbind(data_ts,data_modelo2)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2,col=c("black", "red"))

#pronostico para la serie original YT
#deshacer la transformacion
Pron2$mean <- exp(Pron2$mean)
Pron2$lower <- exp(Pron2$lower)
Pron2$upper <- exp(Pron2$upper)
Pron2$x <- exp(Pron2$x)
Pron2$fitted <- exp(Pron2$fitted)
Pron2$residuals <- exp(Pron2$residuals)
summary(Pron2)

#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron2, shaded = FALSE, xlab = "Años", ylab = "",main = "ARIMA(0,2,1)")
lines(Pron2$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006.7, lwd = 1, col="green")



# modelo 3
#pronostico de la serie  zt
Pron3 <- forecast(mod3,level=c(95),h=10)
plot(Pron3)
summary(Pron3)

#serie original y valores estimados

data_modelo3 <- (mod3$fitted)
grafico_comparativo <- cbind(data_ts,data_modelo3)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2,col=c("black", "red"))

#pronostico para la serie original YT
#deshacer la transformacion

Pron3$mean <- exp(Pron3$mean)
Pron3$lower <- exp(Pron3$lower)
Pron3$upper <- exp(Pron3$upper)
Pron3$x <- exp(Pron3$x)
Pron3$fitted <- exp(Pron3$fitted)
Pron3$residuals <- exp(Pron3$residuals)
summary(Pron3)

#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron3, shaded = FALSE, xlab = "Años", ylab = "PRODUCCIÓN DE TABACO",main = "ARIMA(1,1,2)")
lines(Pron3$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006.7, lwd = 1, col="green")



accuracy(Pron1) #FIJARSE EN RMSE Y MAE Y DEBEMOS FIJARNOS QUE SEAN LOS MENORESY SI QUEREMOS FIJARNOS EN PORCENTAJES PODEMOS FIJARNOS EN MAPE
accuracy(Pron2)
accuracy(Pron3)
#el numero de parametros no influye
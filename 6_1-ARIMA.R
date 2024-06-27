#importar las librerias
library(forecast)
library(tseries)
library(TSA)
library(urca)
library(ggplot2)
library(lmtest) # inferencia para los coeficientes
library(MASS) #  tranformacion de box-cox
library(nortest) # pruebas de normalidad
library(mFilter) # filtro de H-P
library(readxl)
library(strucchange) #analisis de estabilidad, para usar chow
library(fitdistrplus) #ajuste de normalidad 
#importacion de datos

Pieles <- read_excel("/home/cometa/Descargas/Ejm_6_1-Pieles.xlsx")
###################################
# METODOLOGIA DE BOX-JEMKINS
###################################

Yt <- ts(Pieles$Y, start=1821 , frequency=1)
plot(Yt,xlab="año", ylab="cantidad")


#Analisis de componentes 
#Estimacion de la tendencia

lambda_hp = 1000

Yt_hp <- hpfilter(Yt, type = "lambda", freq = lambda_hp)
plot(Yt_hp)


#ANALISIS DE LA ESTACIONARIEDAD
#estacionariedad en varianza

grupo <- c(rep(1:10 ,rep(11,10)))
boxplot(Pieles$Y~grupo, xlab = "grupo")


#correccion de a+la estacionalidad en varianza

#lambda  = 1 <- yt
qqnorm(Yt,main="lambda = 1")
qqline(Yt)


#lambda =2 <- yt^2
t1.yt <- Yt^2
qqnorm(t1.yt,   main="lambda = 2")
qqline(t1.yt)

#lambda = 0.5 <- aiz(YT)
t3.yt <- sqrt(Yt)
qqnorm(t3.yt, main="lambda = 0.5")
qqline(t3.yt)

#lambda  =  0 <- log(Yt)

t4.yt <- log(Yt)
qqnorm(t4.yt, main="lambda = 0")
qqline(t4.yt)


##lambda  = -0.5 <- 1/raiz(yt)

t5.yt <- 1/sqrt(Yt)
qqnorm(t5.yt, main="lambda = -0.5")
qqline(t5.yt)

#lambda  =  -1 <- 1/Yt

t6.yt <- 1/Yt
qqnorm(t6.yt, main="lambda = -1")
qqline(t6.yt)


#lambda  = -2 <- 1/(Yt^2)
t7.yt <- 1/(Yt^2)
qqnorm(t7.yt, main="lambda = -2")
qqline(t7.yt)

# DETERMINACION DEL VALOR lambda
b <- BoxCox.ar(Yt)
b
# lmabda optimo
lambda <- b$mle
round(lambda,1)

#serie transformada T.yt = log(Yt)
T.yt <- log(Yt)
plot(T.yt,xlab = "Años", ylab = "log(Yt)")


#correlograma 
FAS <- acf(T.yt, lag.max = 15,main = "FAS - T.yt", level= 0.95)
FAP <- pacf(T.yt, lag.max = 15,main= "FAP - T.yt", level=0.95)

# prueba de raiz unitaria DFA(dickey-fuller aumentada)
TY_dfa <- ur.df(T.yt,type = "drift")
summary(TY_dfa)

#modelos
#a) log(Yt) - ARIMA(3,0)
#b)log(Yt) - ARIMA(0,2)
#c)log(Yt) - ARIMA(2,1)


modelo1 = Arima(T.yt, order = c(3,0,0), include.constant = T)
summary(modelo1)
coeftest(modelo1)

modelo2 = Arima(T.yt, order = c(0,0,2),include.constant = T)
summary(modelo2)
coeftest(modelo2)


modelo3 = Arima(T.yt, order = c(2,0,1), include.constant = T)
summary(modelo3)
coeftest(modelo3)


#Validacion 
#3.1)analisis de cieficientes 
# significancia de los coeficientes
coeftest(modelo1)
coeftest(modelo2)
coeftest(modelo3)

#matriz de correlacion entre coeficientes
vcov(modelo1)
vcov(modelo2)
vcov(modelo3)

#-vereficicacion de la condicion de estacionariedad e invertibilidad
modelo1 = Arima(T.yt, order = c(3,0,0), include.constant = T) #recomendable volver a correr para evitar alguns errores
autoplot(modelo1)
# como no tiene media movil se asume que ya tiene invertibilidad

modelo2 = Arima(T.yt, order = c(0,0,2),include.constant = T)
autoplot(modelo2)
#cuando no tiene AR se asume que tiene ya estacionariedad 

modelo3 = Arima(T.yt, order = c(2,0,1), include.constant = T)
autoplot(modelo3)


#para analisar el cambio estructural
chow_mod1 <- Fstats(modelo1$fitted~1, from= 0.43)
sctest(chow_mod1) #prueba pvalue es mayor que valor que el valor de significancia(0.05) se acepta la hipotesis nula

chow_mod2 <- Fstats(modelo2$fitted~1, from= 0.43)
sctest(chow_mod2) 

chow_mod3<- Fstats(modelo3$fitted~1, from= 0.43)
sctest(chow_mod3) 


# 3.2 Analisis de los residuos

# -media igual a cero
# estadisticamente la prueba t: a = 0.05
#ho: E(at) = 0 
#hi: E(at) != 0 
#si el p value es mayor que la significancia aceptamos ho

plot(modelo1$residuals,main="modelo 1")
abline(h=0, col = "red")
t.test(modelo1$residuals, mu = 0) 

plot(modelo2$residuals,main="modelo 2")
abline(h=0, col = "red")
t.test(modelo2$residuals, mu = 0) 

plot(modelo3$residuals,main="modelo 3")
abline(h=0, col = "red")
t.test(modelo3$residuals, mu = 0) 


# -- varianza constante 
#par(mfrow=c(3,1))

scatter.smooth(sqrt(abs(modelo1$residuals)),lpars = list(col=2),main="modelo 1")
scatter.smooth(sqrt(abs(modelo2$residuals)),lpars = list(col=2),main="modelo 2")
scatter.smooth(sqrt(abs(modelo3$residuals)),lpars = list(col=2),main="modelo 3")

#test de breusch -pagan
#prueba 
#ho: existe la homogeneidad 
#hi: existe la heterogeneidad
#si p valud es mayor que  0.05 se acepta la hipotesis nula
obs = get(modelo1$series)
bptest(resid(modelo1)~I(obs-resid(modelo1)))

obs = get(modelo1$series)
bptest(resid(modelo2)~I(obs-resid(modelo2)))

obs = get(modelo1$series)
bptest(resid(modelo3)~I(obs-resid(modelo3)))


# ausencia de correlacion serial
resid_m1 <- as.vector(modelo1$residuals)
resid_m2 <- as.vector(modelo2$residuals)
resid_m3 <- as.vector(modelo3$residuals)

FAS_e.m1 <- acf(resid_m1, lag.max = 25, main="FAS modelo 1", level=0.95)
FAS_e.m2 <- acf(resid_m2, lag.max = 25, main="FAS modelo 2", level=0.95)
FAS_e.m3 <- acf(resid_m3, lag.max = 25, main="FAS modelo 3", level=0.95)

#constraste GLobal : box ljung
Box.test(resid_m1, type = "Ljung-Box")
Box.test(resid_m2, type = "Ljung-Box")
Box.test(resid_m3, type = "Ljung-Box")

#contraste de normalidad



library(fitdistrplus) #tarea

ajuste_m1 <- firdist(data= resid_m1, distr=norm)
ajuste_m2 <- firdist(data= resid_m2, distr=norm)
ajuste_m3 <- firdist(data= resid_m3, distr=norm)


#preba de jarque bera
#h0: existe la normalidad
#hi: no existe la normalidad

# si p value es mayor que 0.05 se acepta la hipotesis nula

JB_m1 <- jarque.bera.test(resid_m1)
JB_m1

JB_m2 <- jarque.bera.test(resid_m2)
JB_m2

JB_m3 <- jarque.bera.test(resid_m3)
JB_m3


#######################################################
#Pronostico
#######################################################


# modelo 1
#pronostico de la serie  zt
Pron1 <- forecast(modelo1,level=c(95),h=10)
plot(Pron1)
summary(Pron1)

#serie original y valores estimados

yt_arima1 <- exp(modelo1$fitted)
grafico_comparativo <- cbind(Yt,yt_arima1)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2)

#pronostico para la serie original YT
#deshacer la transformacion
Pron1$mean <- exp(Pron1$mean)
Pron1$lower <- exp(Pron1$lower)
Pron1$upper <- exp(Pron1$upper)
Pron1$x <- exp(Pron1$x)
Pron1$fitted <- exp(Pron1$fitted)
Pron1$residuals <- exp(Pron1$residuals)
summary(Pron1)

#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron1, shaded = FALSE, xlab = "Años", ylab = "N° DE PIELES",main = "ARIMA(3,0,0)")
lines(Pron1$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=1930, lwd = 1, col="green")

# modelo 2
#pronostico de la serie  zt
Pron2 <- forecast(modelo2,level=c(95),h=10)
plot(Pron2)
summary(Pron2)

#serie original y valores estimados

yt_arima2 <- exp(modelo2$fitted)
grafico_comparativo <- cbind(Yt,yt_arima2)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2)

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
plot(Pron2, shaded = FALSE, xlab = "Años", ylab = "N° DE PIELES",main = "ARIMA(0,0,2)")
lines(Pron2$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=1930, lwd = 1, col="green")



# modelo 3
#pronostico de la serie  zt
Pron3 <- forecast(modelo3,level=c(95),h=10)
plot(Pron3)
summary(Pron3)

#serie original y valores estimados

yt_arima3 <- exp(modelo3$fitted)
grafico_comparativo <- cbind(Yt,yt_arima1)
ts.plot(grafico_comparativo, col=c(1,2), lwd = 1)
legend("topleft",c("yt","yest"),lty = c(1,1), lwd = 2)

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
plot(Pron3, shaded = FALSE, xlab = "Años", ylab = "N° DE PIELES",main = "ARIMA(2,0,1)")
lines(Pron3$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=1930, lwd = 1, col="green")



accuracy(Pron1) #FIJARSE EN RMSE Y MAE Y DEBEMOS FIJARNOS QUE SEAN LOS MENORESY SI QUEREMOS FIJARNOS EN PORCENTAJES PODEMOS FIJARNOS EN MAPE
accuracy(Pron2)
accuracy(Pron3)
#el numero de parametros no influye
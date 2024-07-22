library(forecast)#modelo ARIMA
library(tseries)#Para series de tiempo
library(TSstudio)#correlograma parte regular y estacional
library(TSA)#modelos ARMA
library(ggplot2)#para hacer graficos
library(urca)#para hacer test de raiz unitaria (detectar hay)
library(dplyr)#para la manipulacionde datos
library(lmtest)#inferencia para los coeficioentes
library(MASS)#transformacion de Box-Cox
library(nortest)#pruebas de normalidad
library(mFilter)#para hoodrick - prescot
library(zoo)
library(TTR)
library(sandwich)
library(strucchange)#para analisis de estabilidad - Chow
library(survival)
library(fitdistrplus)

#importacion de datos
library(readxl)
datos <- read_excel("/home/cometa/UNIVERSIDAD/6 semestre/series de tiempo/actividad 7/ejercicio_2.xlsx")

#crear una serie temporal a partir de los datos importados
Yt <- ts(datos$ventas, start=c(2011, 1), frequency=4)
#visualizar la serie temporal
plot(Yt, xlab = "meses", ylab = "ventas", main = "ventas de una librria universitaria")

#como se puede apreciar la serie tiene una tendencia el cual decrece asta el año 2017 y a partir de hay
#la serie tiene una tendencia creciente pero en cuanto a la estacionariedad en varianza tiene indicios
#que si hay estacionariedad en varianza.



###############################################################
### 1) IDENTIFICACION
###############################################################
##analisis de la tendencia y estacionalidad
#descomposicion de la serie temporal
Yt_des <- decompose(Yt, type = "additive")
plot(Yt_des)

# tras la descomposicion de la serie se puede obvservar que si existe tendencia tendendia tal como se 
#se describio al inicio, y entre otros factores como la parte aleatoria.

#analisis de la tendencia
lambda_hp <- 10000
data_hp <- hpfilter(Yt,type="lambda", freq = lambda_hp)
plot(data_hp)

# interpretacion

#analisis de la estacionalidad
ggsubseriesplot(Yt, xlab = "trimestres", ylab = "Ventas") 
ggseasonplot(Yt, xlab = "trimestres", ylab = "Ventas") 


#como se puede apreciar en las lineas apiladas y las lineas separadas se observa que no presenetan indicios
# de estacionalidad en la serie temporal el cual se confirma con el correlograma que tiene un  comportamiento en 
#el que decrece lentamente lo cual es una clara señal que no existe la estacionariedad en la serie.


#analisis de la estacionariedad
##  estacionariedad en varianza
par(mfrow=c(1,1))
Grupos <- rep(1:12, each = 4)
boxplot(datos$ventas~Grupos, xlab = "Grupo",ylab="Ventas")

#dado el grafico podemos inferir que la siere tiene un comportamiento en el cual parace presentar la estacionariedad 
# varianza si no fuera por la tercera caja que muestra tener una variacion.

#correccion de la estacionalidad eb varianza

#b <- BoxCox.ar(Yt)
#b

#lambda <- b$mle
#round(lambda,1)

#segun el dato 1.1 y el grafico de boxcox se puede apreciar que la serie no nesecita una transformacion por lo que podemos
#estar afirmando que si es estacionaria en varianza


#estacionariedad en media
#correlograma FAS y FAP
par(mfrow=c(1,2))
FAS <- acf(Yt, lag.max = 25, main="FAS - Yt", level = 0.95)
FAP <- pacf(Yt, lag.max = 25, main="FAP - Yt", level = 0.95)

#dado el grafico del FAS observamos que la se observa que decrece lentamente ya que en el retardo 9 recien da a 0 lo
#es un gran indicio de que la serie no es estacionaria en media por tanto se deberia hacer una difereciacion para estabilisarlo



#prueba de la raiz unitaria de df aumentada parte regular
Y_adf = ur.df(Yt, lags = 1)
summary(Y_adf)

#en la parte regular tenemos t-caculado = -0.036 lo cual es mayor que el t-critico = -1.95 por tanto se rechaza la hipotesis nula 
#de la existencia de la raiz unitaria, es decir que la serie no es estacionaria en media en la estructura Regular



#prueba de la raiz unitaria para la parte estacional 
Y_adf2 = ur.df(Yt,lags = 4)
summary(Y_adf2)

#en la parte regular tenemos t-caculado = 0.2579 lo cual es mayor que el t-critico = -1.95 por tanto se rechaza la hipotesis nula 
#de la existencia de la raiz unitaria, es decir que la serie no es estacionaria en media en la estructura estacional 
#opr tanto se necesita realizar la diferenciacion en la parte regular y estacional.


#diferenciacion
par(mfrow=c(1,1))
D1.Yt <- diff(Yt);
plot(D1.Yt,xlab="tiempo", ylab= "Ventas" )

# PRUEBA DE dF para la parte regular 
D4Yt_adf <- ur.df(D1.Yt, lags = 1)
summary(D4Yt_adf)

# tenemos t-caculado = -8.1482 lo cual es mayor que el t-critico = -1.95 por tanto se acepta la hipotesis nula 
#de la existencia de la raiz unitaria el cual se corrobora con el grafico el cual tiene una tendencia a 0.


#difereciacion parte estacional
D4D1.Yt <- diff(D1.Yt,4)
plot(D4D1.Yt)

#raiz unitaria parte estacional
D4DYt_adf <- ur.df(D4D1.Yt,lags = 4)
summary(D4DYt_adf)

# tenemos t-caculado = -4.742 lo cual es mayor que el t-critico = -1.95 por tanto se acepta la hipotesis nula 
#de la existencia de la raiz unitaria el cual se corrobora con el grafico.


##correlograma de la parte regular y estacional
ts_cor(diff(D1.Yt,4),lag.max = 50)


###############################################################
### 2) ESTIMACION 
###############################################################
#modelos propuestos
#estimacion de parametros 
# modelo 1: zarima(1,1,0)(0,1,1)4
# modelo 2: zarima(0,1,1)(0,1,1)4
# modelo 3: zarima(0,1,1)(0,1,0)4
# modelo 4: zarima(0,1,0)(0,1,1)4

modelo1 <- Arima(Yt, order = c(1,1,0), seasonal = list(order = c(0,1,1)))
coeftest(modelo1)

modelo2 <- Arima(Yt, order = c(0,1,1), seasonal = list(order = c(0,1,1)))
coeftest(modelo2)

modelo3 <- Arima(Yt, order = c(0,1,1), seasonal = list(order = c(0,1,0)))
coeftest(modelo3)

modelo4 <- Arima(Yt, order = c(0,1,0), seasonal = list(order = c(0,1,1)))
coeftest(modelo4)

###############################################################
### 3) VALIDACION
###############################################################

coeftest(modelo1)

# dado el modelo zarima(1,1,0)(0,1,1)4 se observa que los coeficientes significativos son ar1 y sma 1 donde sma1 es altamente 
#significativo mienetras que el ar1 no.

coeftest(modelo2)

# dado el modelo zarima(0,1,1)(0,1,1)4 se observa que los coeficientes significativos son ma1 y sma 1 donde ma1 es altamente 
#significativo mientras que sma1 solo es significativo.

coeftest(modelo3)
# dado el modelo zarima(0,1,1)(0,1,0)4 se observa que ma1 es altamente significativo. 

coeftest(modelo4)

#dado el modelo zarima(0,1,0)(0,1,1)4 se observa qye sma(1) es significativo. 

#analisis de coeficientes / estabilidad
autoplot(modelo1)
#dado para el primer modelo dado que los puntos estan dentro del circulo indica que son invertibles y estacinarios.

autoplot(modelo2)
# para el segundo modelo ya que solo presenta la parte de ma() infiere que la parte autoregresiva ya presenta invertibilidad y estacionariedad 
# pero segun el grafico se  puede observar que que todo los puntos estan dentro del circulo señal clara de invertibilidad y estacionariedad.

autoplot(modelo3)
#dado que todo los puntos estan dentro del circulo lo cual no indica que son invertibles y estacionarios.



#analisis de los residuos
#prueba de boxlung
checkresiduals(modelo1)
checkresiduals(modelo2)
checkresiduals(modelo3)

#Dados los coeficientes p asociados a nuestras pruebas de LJUNG BOX aplicadas a nuestros modelos indica que no hay evidencia suficiente para
#rechazar la hipótesis nula, que afirma que no hay autocorrelación en los residuos. Esto sugiere que los residuos del modelo
#son consistentes con el ruido blanco.


#normalidad de residuos prueba de jarque bera 
qqnorm(modelo1$residuals,main = "qqplot - modelo1")
qqline(modelo1$residuals,col = 2, lwd=1,lty = 2)
jarque.bera.test(modelo1$residuals)

qqnorm(modelo2$residuals,main = "qqplot - modelo2")
qqline(modelo2$residuals,col = 2, lwd=1,lty = 2)
jarque.bera.test(modelo2$residuals)

qqnorm(modelo3$residuals,main = "qqplot - modelo3")
qqline(modelo3$residuals,col = 2, lwd=1,lty = 2)
jarque.bera.test(modelo3$residuals)


#Como podemos observar de manera exploratorio en nuestros graficos podemos observar claramente que los residuales se ajustan a la linea normal 
#lo que indica que da indicios que la misma sigue una distribucion normal. Esto se puede confirmar con los resultados de nuestra prueba de Jarque
#Bera cuyo coeficiente asociado a p es menor a 0.05 en cada uno de los modelos lo que nos indica que absolutamente todos los modelos se ajustan a 
#una distribucion normal.


#seleccion de mejor modelo
AIC(modelo1);BIC(modelo1)
AIC(modelo2);BIC(modelo2)
AIC(modelo3);BIC(modelo3)

#el modelo 2 tiene el AIC mas bajo (363.5213) lo que sugiere segun el criterio de AIC  y tambien tiene el BIC mas bajo (368.8049) lo que sugiere que
#el es modelo preiferido para el criterio de BIC, Sin embargo la diferencia de los entre los 3 modelos propuestos son minimos lo cual indica que tienen
#valores altamente competivos.

#Por lo tanto seleccinamos el modelo de sarima(0,1,1)(0,1,1)4 es el modelo seleccionado.




###############################################################
### 4)PRONOSTICO
###############################################################

### modelo 1
#pronostico para la serie ZT
Pron <- forecast(modelo1,h=12)
plot(Pron)
summary(Pron)

#pronostico real
#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron, shaded = FALSE, xlab = "Años", ylab = "N° DE ventas",main = "zarima(1,1,0)(0,1,1)")
lines(Pron$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)





### modelo 2
#pronostico para la serie ZT
Pron2 <- forecast(modelo2,h=12)
plot(Pron2)
summary(Pron2)

#pronostico real
#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron2, shaded = FALSE, xlab = "Años", ylab = "N° DE ventas ",main = "zarima(0,1,1)(0,1,1)")
lines(Pron2$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)

#### modelo 3
#pronostico para la serie ZT
Pron3 <- forecast(modelo3,h=12)
plot(Pron3)
summary(Pron3)

#pronostico real
#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron3, shaded = FALSE, xlab = "Años", ylab = "N° DE ventas",main = "zarima(0,1,1)(0,1,0)")
lines(Pron3$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)

#accuracy
accuracy(modelo1)
accuracy(modelo2)
accuracy(modelo3)

#Basado en los criterion de informacion de (AIC Y BIC) y las medidad de error(RMSE,MAE,MAPE,MASE),el mejor modelo sarima(0,1,1)(0,1,1)4 es el que indica ser el mejor entre los 3
#modelos seleccionados.Tiene un AIC y BIC más bajos, lo cual indica un buen ajuste según estos criterios, y también presenta las menores medidas de  error en términos de RMSE, 
#MAE, y MAPE. Por lo tanto, recomendaría utilizar  sarima(0,1,1)(0,1,1)4 para realizar predicciones sobre la serie temporal.
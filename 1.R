# Libreria necesaria
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
library(readxl)

##############################
# METODOLOGUIA DE BOX-JENKINS
###############################

datos <- read_excel("D:/SERIES DE TIEMPO/ACTIVIDAD 7/1.xlsx")
View(datos)


# 1) IDENTIFICACION 
# Graficar la serie 
Yt <- ts(datos$Yt, start = c(1994, 1), frequency = 12)     
plot(Yt, xlab = "Año", ylab ="Yt" ) 

# Descomposicion de la serie 
Yt_desc <- decompose(Yt, type = "additive")
plot(Yt_desc, xlab = "Año/Meses")


#ANALISIS DE COMPONENTES
# Estacionalidad 
ggsubseriesplot(Yt, xlab = "Meses", ylab = "Precio" )   
ggseasonplot(Yt, xlab = "Meses",ylab = "Precio" )   
FAS <- acf(Yt, lag.max = 61, main="FAS", level = 0.95)

#CICLICIDAD
# Generación de t
datos$t <- seq(1:NROW(datos))
# Construir series
cosP <- cos(2*pi/144*12*datos$t)
senP <- sin(2*pi/144*12*datos$t)
# Ajuste del modelo
ciclo <- lm(Yt ~ cosP + senP)
plot(ciclo$residuals, type = "l", xlab="t", ylab="Residuos", col = "red")

#ESTCIONARIEDAD EN VARIANZA
#el siguente grafico no es estacionaria en varianza

boxplot(datos$Yt ~ datos$año, xlab = "Grupo", ylab = "Yt" ) 

#Del grafico podemos concluir que la serie estacionaria en varianza

#ESTACIONARIDAD EN MEDIA

par(mfrow=c(1,2))
FAS <- acf(Yt,lag.max = 25, main = "FAS", level = 0.95) 
FAP <- pacf(Yt, lag.max = 25, main = "FAP", level = 0.95 )
FAP$acf[1]

#Dadod que el grafico FAS vemos que decrece rapidamente a 0 en el tercer
#retardo añadido al comportamiento oscilatorio del mismo tenemos indicios de
#que la serie es estacionaria y podemos confirmar esto al evaluar el primer
#coeficiente resultante de las FAP que es menor a 0.9

#Prueba de Dickey-Fuller Aumentado (ADF) para la estructura REGULAR
Y_ru = ur.df(Yt, lags = 1)
summary(Y_ru)

#como ..... entonces rechazamos la hipótesis
#nula de no estacionariedad y se
#concluye que, en efecto, la serie
#temporal es estacionaria.

#La prueba ADF para la serie Yt para la estructura ESTACIONAL.
Y_ru2 = ur.df(Yt, lags = 12)
summary(Y_ru2)

#como .... entonces aceptamos la hipótesis
#nula de no estacionariedad y se
#concluye que, la serie temporal no
#es estacionaria en su estructura
#estacional.

#COMO LA SERIE NO ES ESTACIONARIA EN SU PARTE ESTACIONARIA PROCEREMOS
#A LA DIFERENCIACIA ESTACIONAL DE LA SERIE

par(mfrow=c(1,1))

#DIFERENCIACION EN LA PARTE ESTACIONAL
D12.Yt <- diff(Yt, 12)
plot(D12.Yt, xlab="Años/Meses", ylab="°Temperaturas")

#DFa
D12.Yt_adf_s <- ur.df(D12.Yt, lags = 12)
summary(D12.Yt_adf_s)

#como .... entonces rechazamos la hipótesis
#nula de no estacionariedad y se
#concluye que, la serie temporal
#es estacionaria en su estructura
#estacional.

#2.- ESTIMACION
#SELECCION DE ORDENES
ts_cor(diff(Yt,12), lag.max = 50)

#SARIMA (0,0,1)x(2,1,1)12
#SARIMA (0,0,1)x(3,1,1)12


#para la ejecucion del modelo arima , debemos tomar 
#en cuenta una cosa muy importante, es que el numero 
#de diferenciaciones que hisimos son importantes ya 
#que lo integraremos junto con los calores del ar y ma

# Significancia de los coeficientes
mod1 = Arima(Yt, order = c(0,0,1),seasonal =list(order = c(2,1,1)))
coeftest(mod1)

mod2 = Arima(Yt, order = c(0,0,1), seasonal =list(order = c(3,1,1)))
coeftest(mod2)

mod3 = Arima(Yt, order = c(0,0,0), seasonal =list(order = c(2,1,0)))
coeftest(mod3)

mod4 = Arima(Yt, order = c(0,0,0), seasonal =list(order = c(3,1,0)))
coeftest(mod4)

mod5 = Arima(Yt, order = c(0,0,0), seasonal =list(order = c(0,1,1)))
coeftest(mod5)

#3.- VALIDACION
# Validacion si as variables son signifiativas 
vcov(mod3)      
vcov(mod4)                  
vcov(mod5) 

# Analissi de estabilidad 
autoplot(mod3)
autoplot(mod4)
autoplot(mod5)

#prueba de boxlung
checkresiduals(mod3)
checkresiduals(mod4)
checkresiduals(mod5)

#dados coeficientes p asociados a nuestras pruebas de LJUNG BOX podemos
#aceptamos la hipotesis nula y por lo tanto nuestros residuos pueden
#considerarse ruido blanco

#se puede observar que los datos presentan una variabilidad considerable

#normalidad delos residuales 
par(mfrow = c(1,1))
#MODELO 1
qqnorm(mod3$residuals, main = "qq plot-mod3")
qqline(mod3$residuals, col= 2, lwd = 1, lty = 2)

jarque.bera.test(mod1$residuals)

#LOS RESIDUOS SIGUEN UNA DISTRIBUCION NORMAL

#MODELO 2
qqnorm(mod4$residuals, main = "qq plot-mod4")
qqline(mod4$residuals, col= 2, lwd = 1, lty = 2)

jarque.bera.test(mod4$residuals)

#ACEPTAMOS LA HIPOTESIS NULA POR LO TANTO LOS RESIDUOS SIGUEN UNA
#DISTRIBUCION NORMAL

#MODELO 3
qqnorm(mod5$residuals, main = "qq plot-mod5")
qqline(mod5$residuals, col= 2, lwd = 1, lty = 2)

jarque.bera.test(mod5$residuals)

#ACEPTAMOS LA HIPOTESIS NULA POR LO TANTO LOS RESIDUOS SIGUEN UNA 
#DISTRIBUCION NORMAL

AIC(mod3); BIC(mod3)
AIC(mod4); BIC(mod4)
AIC(mod5); BIC(mod5)

#El modelo 5 (mod5) tiene el AIC más bajo (736.2537), lo que sugiere 
#que es el modelo preferido según el criterio de AIC.

#El modelo 5 (mod5) también tiene el BIC más bajo (742.0193), lo que 
#sugiere que es el modelo preferido según el criterio de BIC.


###################################################333
#4.- pronostico
################################################################

#MODELO 1
pron <- forecast(mod3, h = 12)
plot(pron)
summary(pron)

#MODELO 2
pron2 <- forecast(mod4, h = 12)
plot(pron2)
summary(pron2)

#MODELO 3
pron3 <- forecast(mod5, h = 12)
plot(pron3)
summary(pron3)

#Serie original (Yt) y pronosticada (D12.Yt)..
Yt_S <- mod3$fitted
grafico_comparativo <- cbind(Yt, Yt_S)
ts.plot(grafico_comparativo, col = c(1:2), lwd = 2)
legend("topleft", c("Yt", "Yest"), lty=c(1,1), col=c(1:2), lwd = 2)

#Serie original (Yt) y pronosticada (D12.Yt)..
Yt_S2 <- mod4$fitted
grafico_comparativo2 <- cbind(Yt, Yt_S2)
ts.plot(grafico_comparativo2, col = c(1:2), lwd = 2)
legend("topleft", c("Yt", "Yest"), lty=c(1,1), col=c(1:2), lwd = 2)

#Serie original (Yt) y pronosticada (D12.Yt)..
Yt_S3 <- mod5$fitted
grafico_comparativo3 <- cbind(Yt, Yt_S3)
ts.plot(grafico_comparativo3, col = c(1:2), lwd = 2)
legend("topleft", c("Yt", "Yest"), lty=c(1,1), col=c(1:2), lwd = 2)


#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
#MODELO 1
plot(pron, shaded = FALSE, xlab = "Años", ylab = "precio",main = "SARMA(2,1,0)")
lines(pron$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006, lwd = 3, col="green")

accuracy(pron)

#MODELO 2
plot(pron2, shaded = FALSE, xlab = "Años", ylab = "precio",main = "SARMA(3,1,0)")
lines(pron2$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006, lwd = 3, col="green")

accuracy(pron2)

#MODELO 3
plot(pron3, shaded = FALSE, xlab = "Años", ylab = "precio",main = "SARMA(0,1,0)")
lines(pron3$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006, lwd = 3, col="green")

accuracy(pron3)


#Análisis de Resultados
#Basado en los criterios de información (AIC, BIC) y las medidas de error 
#(RMSE, MAE, MAPE, MASE), el modelo SARMA(0,1,0) es el que parece ser el 
#entre los tres evaluados. Tiene un AIC y BIC más bajos, lo cual indica un 
#ajuste según estos criterios, y también presenta las menores medidas de 
#error en términos de RMSE, MAE, y MAPE. Por lo tanto, recomendaría utilizar 
#SARMA(0,1,0) para realizar predicciones sobre la serie temporal que estás 
#analizando.
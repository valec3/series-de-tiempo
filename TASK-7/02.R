# Ejercicio N° 2 
#
library(forecast)# mdelos ARIMA
library(tseries) # series de tiempo 
library(TSA)   
library(urca)    # prueba de raises unitarias 
library(ggplot2) # graficos 
library(dplyr)  
library(lmtest)  #Inferencia para los coeficientes 
library(MASS)    #Tranformacion de box.cox
library(nortest) #pruebas de normalidad
library(mFilter) #filtro de jodric prescot (p-h)
library(strucchange)    # analisis de estabilidad    http://127.0.0.1:29089/graphics/plot_zoom_png?width=941&height=778
library(fitdistrplus)
library(readxl)
library(TSstudio)
##############################
# METODOLOGUIA DE BOX-JENKINS
###############################


datos <- read_excel("F:\\777--Programacion repos\\Una\\r\\data\\actividad-07.xlsx",sheet = "02")
View(datos)


# 1) IDENTIFICACION 
# Graficar la serie 
Yt <- ts(datos$Ventas, start = c(2011, 1), frequency = 4)     
plot(Yt, xlab = "Año", ylab ="Yt" ) 

# Descomposicion de la serie 
Yt_desc <- decompose(Yt, type = "additive")
plot(Yt_desc, xlab = "Año/Meses")

# Descomposicion de la serie 
Yt_desc <- decompose(Yt, type = "multiplicative")
plot(Yt_desc, xlab = "Años")

# Estacionalidad 
ggsubseriesplot(Yt, xlab = "Años", ylab = "Ventas" )   
ggseasonplot(Yt, xlab = "Años",ylab = "Ventas" )   
ggAcf(Yt, xlab = "Años", ylab = "Ventas")

###################
#TENDENCIA
###################
lambda_hp <- 1600  
data_hp <- hpfilter(Yt, type="lambda", freq=lambda_hp)
plot(data_hp)



##################################
#ESTACIONARIEDAD VARIANZA
##################################

boxplot(datos$Ventas ~ datos$Año, xlab = "Año", ylab="Yt")

b <- BoxCox.ar(Yt)
lambda <- b$mle
round(lambda,2)


##################################
#ESTACIONARIEDAD EN MEDIA
##################################
# Verificación con la prueba de Raíz unitaria de Dickey-Fuller Aumentada.
data_adf <- ur.df(Yt,type="drift", lags = 1)
summary(data_adf)

par(mfrow = c(1,2))
FAS <- acf(Yt,lag.max = 25, main = "FAS") 
FAP <- pacf(Yt, lag.max = 25, main = "FAP") 


#TRANSFORMNACION DE LA SERIE , PERO APLICAREMOS LO QUE ES EN ESTE CASO LOG



#diferenciacion regular

Yt_diff1 <- diff(Yt)
plot(Yt_diff1,xlab="tiempo", ylab= "Ventas" )

# Prueba dickey fuller a los datos diferenciados
Yt_diff1_adf <- ur.df(Yt_diff1,lags = 1)
summary(DZt_adf)


#Estructura estacional

Yt_diff1_adf2 <- ur.df(Yt_diff1, lags = 4)
summary(Yt_diff1_adf2)


#DIFERENCIACION EN LA PARTE ESTACIONAL

D12D1.zt <- diff(D1.zt, 12)
plot(D12D1.zt,xlab="tiempo", ylab= "pasajeros" )


#DFa
D12DZt_adf <- ur.df(D12D1.zt, lags = 12)
summary(D12DZt_adf)

#coorelograma de la parte  regular y estacional
library(TSstudio)

ts_cor(diff(D1.zt,12), lag.max = 50)



D1V = diff(zt,1)  # con ste se corrige  
plot(D1V, xlab = "tiempo", ylab = "Yt")


#segunda diferenciacion 


Y_ru3 = ur.df(D1V, lags = 1)
summary(Y_ru3)


ts_cor(D1V, lag.max = 50)




##############################################################3
#Inclusión del termino independiente () o intercepto
Z <- mean(DIV_2)
Co <- var(DIV_2)
Tn <- length(Yt)
Ta <- Tn - 1
Sigma <- Co/Ta
t <- Z/Sigma
tt <- qt(1-0.05/2,Ta-1)
pruebaT <- c(t, tt)
names(pruebaT) <- c("t-calculado","t-critico")
pruebaT

# IDENTIFICACION DE LOS MODELOS ARIMA
FAS <- acf(Yt,lag.max = 15, main = "FAS") 
FAP <- pacf(Yt, lag.max = 15, main = "FAP") 


#para la ejecucion del modelo arima , debemos rtomar en cuenta una cosa muy importante, es que el numero de diferenciaciones que hisimos son importantes ya que lo integraremos junto con los calores del ar y ma
# como los valores del ar es ar()
# Significancia de los coeficientes
mod1 = arima(zt, order = c(1,1,0), seasonal =list(order = c(0,1,1)))
coeftest(mod1)

mod2 = arima(zt, order = c(0,1,1), seasonal =list(order = c(0,1,1)))
coeftest(mod2)

mod3 = arima(zt, order = c(1,1,1), seasonal =list(order = c(0,1,1)))
coeftest(mod3)

# Validacion si as variables son signifiativas 
vcov(mod1)      
vcov(mod2)                  
vcov(mod3) 

# Analissi de estabilidad 
autoplot(mod1)
autoplot(mod2)
autoplot(mod3)

# Generar un modelo respecto a una constante
# Nos ayudara a ver si los coeficientes son estables
# test de chow 
Chow_mod1 <- Fstats(mod1$fitted ~ 1, from = 0.36)
sctest(Chow_mod1)

Chow_mod2 <- Fstats(mod2$fitted ~ 1, from = 0.36)
sctest(Chow_mod2)

Chow_mod3 <- Fstats(mod3$fitted ~ 1, from = 0.36)
sctest(Chow_mod3)

# dado los resultados de la test de chow , se puede ver que no ay estabilidad de coeficientes

# Analisis de los residuos 
plot(mod1$residuals, main = "Modulo 1" )
abline(h=0, col = "red")
plot(mod2$residuals, main = "Modulo 2" )
abline(h=0, col = "green")
plot(mod3$residuals, main = "Modulo 3" )
abline(h=0, col = "blue")

# prueba de t.test    h0 = R(ai) = 0 , h1 = E(ai) =/ 0
t.test(mod1$residuals, mu = 0)
t.test(mod2$residuals, mu = 0)
t.test(mod3$residuals, mu = 0)

#analisis de los residuales

#analisis de los graficos y prueba  de bo ljung
checkresiduals(mod1)
checkresiduals(mod2)

#normalidad delos residuales 
qqnorm(mod1$residuals, main = "qq plot-mod1")
qqline(mod1$residuals, col= 2, lwd = 1, lty = 2)
qqnorm(mod2$residuals, main = "qq plot-mod2")
qqline(mod2$residuals, col= 2, lwd = 1, lty = 2)

#prueba de jarque - bera o shapiro-wilk

jarque.bera.test(mod1$residuals)
jarque.bera.test(mod2$residuals)

AIC(mod1); BIC(mod1)
AIC(mod2); BIC(mod2)

###################################################333
# pronostico
################################################################

#pronostico para la serie zt
pron <- forecast(mod1, h = 12)
plot(pron)
summary(pron)

#############################
#   PRONOSTICOS
#############################
#pronostico
# modelo 1
#pronostico de la serie  zt




#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron1, shaded = FALSE, xlab = "Años", ylab = "precio",main = "ARIMA(3,2,0)")
lines(Pron1$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006.65, lwd = 3, col="green")



# modelo 2
#pronostico de la serie  zt
Pron2 <- forecast(mod2,level=c(95),h=12)
plot(Pron2)
summary(Pron2)


#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron2, shaded = FALSE, xlab = "Años", ylab = "precio",main = "ARIMA(3,2,1)")
lines(Pron2$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006.65, lwd = 3, col="green")


# modelo 3
#pronostico de la serie  zt
Pron3 <- forecast(mod3,level=c(95),h=12)
plot(Pron3)
summary(Pron3)


#GRÁFICA DEL AJUSTE Y PRONÓSTICO CON VALORES REALES
plot(Pron3, shaded = FALSE, xlab = "Años", ylab = "precio",main = "ARIMA(3,2,1)")
lines(Pron3$fitted, col = "red")
legend("topleft", legend=c("SERIE", "PREDICCION", "INTERVALO DE COINFIANZA AL 95%", "AJUSTE"),col=c("black", "blue", "black", "red"), lty=c(1,1,2,1), lwd = 2,cex = 0.6)
abline(v=2006.65, lwd = 3, col="green")

accuracy(Pron1)
accuracy(Pron2)
accuracy(Pron3)





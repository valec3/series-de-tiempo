require(readxl)
require(forecast)
require(tseries)
require(TSA)
require(urca)
require(ggplot2)
require(lmtest) # inferencia para los coeficientes
require(MASS) #  tranformacion de box-cox
require(nortest) # pruebas de normalidad
require(mFilter) # filtro de H-P
require(readxl)
require(strucchange) #analisis de estabilidad, para usar chow
require(fitdistrplus)

data <- read_excel("D:/SERIES DE TIEMPO/ACTIVIDAD 5/1.xlsx", sheet = "Sheet1")
View(data)

#GRAFICAMOS LA SERIE
Yt <- ts(data$IPC, start=c(1999,1) , frequency=12)
plot(Yt,xlab="AÑO/MES", ylab="IPC")

#1 ANALISIS DE COMPONENTES

#YA QUE ES UNA SERIE MENOR A UN AÑO ANALIZAMOS LA ESTACIONALIDAD
Yt_des <- decompose(Yt, type="additive")
plot(Yt_des, xlab("AÑOS/MESES"))

#ANALISIS DE LA TENDENCIA
lambda_hp <-  1600
Yhp <- hpfilter(Yt, type="lambda", freq = lambda_hp)
plot(Yhp)

#ANALISIS DE LA ESTACIONALIDAD
corr <- acf(Yt, lag.max = 25, main = "FAS", level = 0.95)
ggsubseriesplot(Yt, xlab="Meses", ylab="IPC")
ggseasonplot(Yt, xlab="Meses", ylab = "IPC")

#PODEMOS OBSERVAR QUE NO EXISTE ESTACIONALIDAD

#ESTACIONARIEDAD EN VARIANZA
boxplot(data$IPC ~ data$Año, xlab = "AÑOS/MESES", ylab="IPC", main="Distribucion" )

qqplot_Yt <- qqnorm(Yt, main = "lambda = 1")
qqline(Yt, col = "red")

#lambda  = 1 <- yt
qqnorm(Yt,main="lambda = 1")
qqline(Yt)


#lambda =2 <- yt^2
t1.yt <- Yt^2
qqnorm(t1.yt, main="lambda = 2")
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


#DADO QUE NUESTRO GRAFICO DA INDICIOS DE VARIANZA BUSCAMOS ESTABILIZAR
#LA SERIE
boxcox_result <- boxcox(Yt ~ 1, lambda = seq(-2, 2, by = 0.1))

# Obtener el lambda óptimo basado en la máxima verosimilitud
lambda_optimo <- boxcox_result$x[which.max(boxcox_result$y)]
lambda_optimo <- round(lambda_optimo, 1)

lambda_optimo

#TRANSFORMAMOS LA SERIE
T.Yt <- 1/(Yt^2)
qqnorm(T.Yt, main = "Lambda =-2")
qqline(T.Yt)

Tr.Yt<- 1/(Yt^2)
plot(Tr.Yt, xlab="AÑOS", ylab="IPC")

#DADO QUE LA VARIANZA NO LOGRA ESTABILIZARSE

#ESTACIONARIEDAD EN MEDIA
FAS <- acf(Yt, lag.max = 15, main="FAS - T.Yt", level = 0.95)
FAP <- pacf(Yt, lag.max = 15, main="FAP - T.Yt", level = 0.95)

FAP$acf[1]

#DADO Q HAY UN DECRECIMIENTO LENTO HAY INDICIOS DE LA NO ESTACIONARIEDAD EN MEDIA
#ACUÑADO AL VALOR DEL COEFICIENTE QUE ES MAYOR A 0.9

TY_adf <- ur.df(Yt, type="trend", lags = 1)
summary(TY_adf)

adf.test(Yt, k = 0)

#-1.0777 > -3.45

#NO ES ESTACIONARIO EN MEDIA NI VARIANZA

#DIFERENCIACION
#PRIMERA DIFERENCIA
D1Y <- diff(Yt)
plot(D1Y, xlab = "Años", ylab="IPC")

#ESTACIONARIEDAD EN MEDIA
FAS <- acf(D1Y, lag.max = 25, main="FAS - D1.Yt", level = 0.95)
FAP <- pacf(D1Y, lag.max = 25, main="FAP - D1.Yt", level = 0.95)

FAP$acf[1]

#DADO Q HAY UN DECRECIMIENTO LENTO HAY INDICIOS DE LA NO ESTACIONARIEDAD EN MEDIA
TY_adf <- ur.df(D1Y, type="drift", lags = 1)
summary(TY_adf)

adf.test(D1Y, k = 0)


#SEGUNDA DIFERENCIA
D2Y <- diff(D1Y)
plot(D2Y, xlab = "Años", ylab="IPC")

#ESTACIONARIEDAD EN MEDIA
FAS <- acf(D2Y, lag.max = 25, main="FAS - D2.Yt", level = 0.95)
FAP <- pacf(D2Y, lag.max = 25, main="FAP - D2.Yt", level = 0.95)

FAP$acf[1]

#
TY_adf2 <- ur.df(D2Y, type="drift", lags = 1)
summary(TY_adf2)

adf.test(D2Y, k = 0)

#DADO EL COMPORTAMIENTO DE LOS DIAGRAMAS DE AUTOCORRELACION CONSIDERAREMOS
#TANTO LOS MODELOS CON 1 Y 2 DIFERENCIACIONES

#INCLUSION DEL INTERCEPTO
plot(D1Y, xlab="Años", ylab="IPC")
abline(h = mean(D1Y), col = "red")

#INCLUSION DEL INTERCEPTO
plot(D2Y, xlab="Años", ylab="IPC")
abline(h = mean(D2Y), col = "red")

#PRUEBA DE HIPOTESIS PARA D1Y
Z <- mean(D1Y)
CO <- var(D1Y)
Tn <- length(Yt)
Ta <- Tn - 1
Sigma <- CO/Ta
t <- Z/Sigma
tt <- qt(1-0.05/2,Ta-1)
pruebaT <- c(t, tt)
names(pruebaT) <- c("t-calculado","t-critico")
pruebaT


#PRUEBA DE HIPOTESIS PARA D2Y
Z <- mean(D2Y)
CO <- var(D2Y)
Tn <- length(Yt)
Ta <- Tn - 1
Sigma <- CO/Ta
t <- Z/Sigma
tt <- qt(1-0.05/2,Ta-1)
pruebaT <- c(t, tt)
names(pruebaT) <- c("t-calculado","t-critico")
pruebaT

#PONEMOS ZT YA QUE HEMOS DIFERENCIADO LA SERIE

#PLANTEAMIENTO DE MODELOS
#OJO: PARA EL PLANTEAMIENTO DE MODELOS DE ARIMA USAR LA SERIE ORIGINAL
#ARIMA (2,2,0)
mod1 <- Arima(Yt, order = c(2,2,0), include.constant = TRUE)
coeftest(mod1)

#ARIMA (2,2,1)
mod2 <- Arima(Yt, order = c(2,2,1), include.constant = TRUE)
coeftest(mod2)

#ARIMA (1,1,2)
mod3 <- Arima(Yt, order = c(1,1,2), include.constant = TRUE)
coeftest(mod3)

#VALIDACION
#3.1)analisis de cieficientes
# significancia de los coeficientes
coeftest(mod1)
coeftest(mod2)
coeftest(mod3)

#matriz de correlacion entre coeficientes cov(modelo1)
vcov(mod1)
vcov(mod2)
vcov(mod3)

#Condición de Convergencia e Invertibilidad
autoplot(mod1)
autoplot(mod2)
autoplot(mod3)

#para analisar el cambio estructural
Chow_mod1 <- Fstats(mod1$fitted ~ 1, from = 0.49)
sctest(Chow_mod1)

chow_mod2 <- Fstats(mod2$fitted ~ 1, from = 0.49)
sctest(chow_mod2)

chow_mod3 <- Fstats(mod3$fitted ~ 1, from = 0.49)
sctest(chow_mod3)

#ANALISIS DE LOS RESIDUOS
#media igual a cero

plot(mod1$residuals)
abline(h = 0, col = "red")

t.test(mod1$residuals, mu = 0)


plot(mod2$residuals)
abline(h = 0, col = "red")

t.test(mod2$residuals, mu = 0)

plot(mod3$residuals)
abline(h = 0, col = "red")

t.test(mod3$residuals, mu = 0)

# HOMOCEDASTICIDAD DE LOS RESIDUOS
par(mfrow = c(1,1))

# Modelo 1
scatter.smooth(sqrt(abs(mod1$residuals)),
               main = "Modelo 1",
               col = "blue")

# Modelo 2
scatter.smooth(sqrt(abs(mod2$residuals)),
               main = "Modelo 2",
               col = "blue")

# Modelo 3
scatter.smooth(sqrt(abs(mod3$residuals)),
               main = "Modelo 3",
               col = "blue")


#Breusch-Pagan

obs=get(mod1$series)
bptest(resid(mod1)~I(obs-resid(mod1)))

obs=get(mod2$series)
bptest(resid(mod2)~I(obs-resid(mod2)))

obs=get(mod3$series)
bptest(resid(mod3)~I(obs-resid(mod3)))

#aUSENCIA DE CORRELACION SERIAL
resid_m1 <- as.vector(mod1$residuals[4:92])
resid_m2 <- as.vector(mod2$residuals[4:92])
resid_m3 <- as.vector(mod3$residuals[4:92])

FAS_e.m1 <- acf(resid_m1, lag.max = 25,
                main="FAS Modelo 1", level = 0.95)
FAS_e.m2 <- acf(resid_m2, lag.max = 25,
                main="FAS Modelo 2", level = 0.95)
FAS_e.m2 <- acf(resid_m3, lag.max = 25,
                main="FAS Modelo 3", level = 0.95)

#constraste GLobal : box ljung
Box.test(resid_m1,type = "Ljung-Box")
Box.test(resid_m2,type = "Ljung-Box")
Box.test(resid_m3,type = "Ljung-Box")

#contraste de normalidad
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


#4 PRONOSTICO
#modelo 1
Pron1 = forecast(mod1, level = 0.95, h = 10)
plot(Pron1)
summary(Pron1)

#SERIE ORIGINAL Y VALORES ESTIMADOS
Yt_arima1 <- mod1$fitted
grafico_comparativo <- cbind(Yt, Yt_arima1)
ts.plot(grafico_comparativo, col = c(1:2), lwd = 1)
legend("topleft", c("Yt", "Yest"), lty = c(1,1), col=c(1:2), lwd=2)

#PRONOSTICO PARA LA SERIE 1
summary(Pron1)

#PRONOSTICO REAL
plot(Pron1, shaded = FALSE, xlab = "AÑOS", ylab="IPC", main="ARIMA (2,2,0)")
lines(Pron1$fitted, col ="red")
legend("topleft", legend=c("Serie", "PREDICION", "INTERVALO DE CONFIANZA 95%","AJUSTE"),
       lty = c(1,1,2,1), lwd = 2, cex=0.6)
abline(v=2006.7, lwd=1, col="green")

#MODELO 2
Pron2 = forecast(mod2, level = 0.95, h = 10)
plot(Pron2)
summary(Pron2)

#SERIE ORIGINAL Y VALORES ESTIMADOS
Yt_arima2 <- mod2$fitted
grafico_comparativo2 <- cbind(Yt, Yt_arima2)
ts.plot(grafico_comparativo2, col = c(1:2), lwd = 1)
legend("topleft", c("Yt", "Yest"), lty = c(1,1), col=c(1:2), lwd=2)

#PRONOSTICO PARA LA SERIE 2
#DESHACER
summary(Pron2)

#PRONOSTICO REAL
plot(Pron2, shaded = FALSE, xlab = "AÑOS", ylab="IPC", main="ARIMA (2,2,1)")
lines(Pron2$fitted, col ="red")
legend("topleft", legend=c("Serie", "PREDICION", "INTERVALO DE CONFIANZA 95%","AJUSTE"),
       lty = c(1,1,2,1), lwd = 2, cex=0.6)
abline(v=2006.7, lwd=1, col="green")


#MODELO 3
Pron3 = forecast(mod3, level = 0.95, h = 10)
plot(Pron3)
summary(Pron3)

#SERIE ORIGINAL Y VALORES ESTIMADOS
Yt_arima3 <- mod3$fitted
grafico_comparativo3 <- cbind(Yt, Yt_arima3)
ts.plot(grafico_comparativo3, col = c(1:2), lwd = 1)
legend("topleft", c("Yt", "Yest"), lty = c(1,1), col=c(1:2), lwd=2)

#PRONOSTICO PARA LA SERIE 2
#DESHACER
summary(Pron3)

#PRONOSTICO REAL
plot(Pron3, shaded = FALSE, xlab = "AÑOS", ylab="IPC", main="ARIMA (1,1,2)")
lines(Pron2$fitted, col ="red")
legend("topleft", legend=c("Serie", "PREDICION", "INTERVALO DE CONFIANZA 95%","AJUSTE"),
       lty = c(1,1,2,1), lwd = 2, cex=0.6)
abline(v=2006.7, lwd=1, col="green")


#METRICAS
#PARA DETERMINAR UN MEJOR MODELO NOS FIJAMOS EN EL MAE

accuracy(Pron1)
accuracy(Pron2)
accuracy(Pron3)

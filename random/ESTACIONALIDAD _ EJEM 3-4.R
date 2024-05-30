# ejemplo 3.4 - ESTACIONALIDAD
#LIBRERIAS
library(ggplot2)
library(forecast)
library(readxl)

#library(ggfortify)

serie_v <- read_excel("MI_CARPETA/4-len R/r-series de tiempo/Ejm_3_4.xlsx", 
                      sheet = "Hoja1")
View(serie_v)

serie_a <- read_excel("MI_CARPETA/4-len R/r-series de tiempo/Ejm_3_4.xlsx", 
                      sheet = "Hoja2")
View(serie_a)

#Metodos graficos

yv <- ts(serie_v$Ventas, start = c(2015,1), frequency = 12)
plot(yv,xlab="año",ylab="ventas")

ya <- ts(serie_a$Activos, start = c(2005,1), frequency = 4)
plot(ya,xlab="año",ylab="activos")


################################################################
# A) METODO DE LINEAS APILADAS
################################################################
ggsubseriesplot(yv,xlab="meses",ylab="ventas")
ggsubseriesplot(ya,xlab="trimestre",ylab="activos")

#############################################################
# B)Diagrama de cajas
#############################################################
boxplot(yv~cycle(yv),xlab="meses",ylab="ventas")
boxplot(ya~cycle(ya),xlab="trimestre",ylab="activos")

###########################################################
# c) LINEAS SEPARADAS
###########################################################
ggseasonplot(yv,xlab="meses",ylab="ventas")
ggseasonplot(ya,xlab="trimestre",ylab="activos")

###########################################################
# d) CORRELOGRAMAS
###########################################################

ggAcf(yv, lag.max = 48)
ggAcf(ya, lag.max = 9)


##########################################################
##########################################################
# CUANTIFICACION DE LA ESTACIONALIDAD para ventas
##########################################################
##########################################################
#tareas descargar estas,librerias
library(fastDummies)#creacion de variables dummies
library(scales) # formtato de fechas en el eje x
library(dplyr) #generar tablas de resumen

#VENTAS 
# ventas con retardo
serie_v$vent_1<- lag(serie_v$Ventas)
View(serie_v)

#variables dummies

serie_v$dummy <- serie_v$Mes
#tabla dummy0
tabla_dummy <- dummy_columns(serie_v, select_columns = c("dummy"),remove_first_dummy= TRUE)



# datafrme para regresion 
tabla_reg <- tabla_dummy[c(4:5,7:17)]

# regresion con variables dummy}
reg_1 <- lm(serie_v$Ventas ~.,data = tabla_reg)
summary(reg_1)


##########################################################
##########################################################
# CUANTIFICACION DE LA ESTACIONALIDAD para activos
##########################################################
##########################################################
#ACTIVOS
# sctivos con retardo
serie_a$act_1<- lag(serie_a$Activos)
View(serie_v)

#variables dummies

serie_a$dummy <- serie_a$Trimestre
#tabla dummy0
tabla_dummy_a <- dummy_columns(serie_a, select_columns = c("dummy"),remove_first_dummy= TRUE)



# datafrme para regresion 
tabla_reg_a <- tabla_dummy_a[c(4:5,7:9)]# arreglar cuando de acuerdo a los datos

# regresion con variables dummy}
reg_2 <- lm(serie_a$Activos ~.,data = tabla_reg_a)
summary(reg_2)

################################################################################
# DESESTACIONALIZACION
################################################################################
# VENTAS 
# promedio moviles
library(TTR)
yv_ms <- SMA(yv,12) # media movil simple
yv_mc <- ma(yv,12) #media movil  centrada
plot(yv,xlab="año/meses",ylab="VENTAS")
lines(yv_ms, type="l", col = "red")
lines(yv_mc, type="l", col = "blue")
legend(x= "bottomright", legend = c("yv","yv_ms", "yv_mc"), col = c("black", "red", "blue"), lwd = c(1,1))


# suavisamiento o Alisado
yhw_m = HoltWinters(yv, seasonal = "multiplicative")
plot(yhw_m)





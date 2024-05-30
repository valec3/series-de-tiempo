################
# ACTIVIDAD 2  #
################

# EJERCICIO 1
#LIBRERIAS
library(ggplot2)
library(forecast)
library(readxl)

# A.1) tendencia: T = 2*t + 1

t <- 1:150
tendencia <- 2*t + 1
plot(tendencia, type = "l", xlab="t")

# A.2) estacional: Et = 30*sin((2*pi/12)*(t+1))+100

estacional <- 30*sin((2*pi/12)*(t+1))+100
plot(estacional, type = "l", xlab="t")

# A.3) irregular 
irregular = rnorm(length(t), 0,5)
plot(irregular, type = "l", xlab="t")


# simular los modelos: 
#modelo aditivo : 
ya <- tendencia + estacional + irregular
plot(ya,type="l", xlab="t")

# modelo mixto:
    # calcular irregular 
irregular = rnorm(length(t),0,500)

ym <- tendencia*estacional + irregular
plot(ym,type="l", xlab="t")

# b) tendencia exponencial
# tendencia: Tt = e^2t

t <- seq(0,2, length= length(t))
tendencia_e <- exp(2*t)
plot(tendencia_e, type = "l", xlab="t")

# Ejericio 2



# EJERCICIO 3
# EJERCICIO 4
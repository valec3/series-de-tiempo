#3.3 estimacion de la sentencia

library(ggplot2)
library(readxl)

# Cargar datos desde Excel
serie <- read_excel("C:/Users/JHONYNANI/Downloads/Ejm_3_3.xlsx")
View(serie)

# Crear serie de tiempo
Yt <- ts(serie$Y, start = c(1984, 3), frequency = 4)

# Graficar la serie de tiempo
plot(Yt,xlab = "Años")

t<-time(Yt)
Y1<- lm(Yt ~ t)

Yes1<- ts(predict(Y1, t),start = c(1984, 3), frequency = 4 )
lines(Yes1, type = "l", col = "pink")

#ajuste cuadratico y = a + b*t + c*(t^2)

Y2<- lm(Yt ~ t + I(t^2))
Yes2 <- ts(predict(Y2, t), start = c(1984, 3), frequency = 4)
lines(Yes2, type = "l", col = "purple")

#metodos de suavisamineto
#MEtodo de suavisamiento
#grafico de la serie

library(TTR) #metodo de media movil
k <- 4 # perdiodo

Yt_ma <- SMA(Yt, k)
print(Yt_ma)
plot(Yt, xlab ="años")
lines(Yt_ma,type="l", col = "red")
legend(x = "bottomright",legend = c("Yt","Yt_ma"), col = c('black','red'), lty = c(1,1))

#metodo holt-W
#libreria forecast
library(forecast)
Yholt <- holt(Yt, h = 4)
plot(Yt, xlab ="años")
lines(Yholt$fitted, type = "l", col= "blue")
legend(x = "bottomright",legend = c("Yt","Yholt"), col = c('black','blue'), lty = c(1,1))

#filtro hodrick - Prescott
#libreria mFilter
library(mFilter)
Yhp <- hpfilter(Yt, type = "lambda", freq = 1600)
plot(Yhp)
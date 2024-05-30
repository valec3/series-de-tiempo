library(readxl)
datos <- read_excel("~/Descargas/Ejem_2_4.xlsx")
View(datos)

library(ggplot2)
library(forecast)

#concertir los datos a serie temporal

VENts <- ts(datos$VEN, start = 1, frequency = 1)
print(VENts)

#grafico de la serie

plot(VENts,type = "l", xlab="Meses",ylab="Y")




#########################################################
#   METODO DE SUAVISAMIENTO EXPONECIAL DOBLE
#########################################################

library(fpp2)
VENses <- ses(yts,alpha = 0.136,h = 12)
VEN2 <- ses(VENses$fitted,alpha = 0.136,h=1)

a <- 2*VENses$fitted-VEN2$fitted
b <- (0.136/(1-0.136))*(VENses$fitted-VEN2$fitted)


p <- 1

VENses2 <- a +b*p

plot(VENts, type = "l", xlab = "meses", ylab=" ")
lines(VENses2,type="l", col="red")
legend(x="bottomright", legend = c("VENts","VENses2"), col = c("black","red"))

###########################################################
#  METODO  DE HOLT
###########################################################

VENholt <- holt(VENts,h=1,alpha = 0.3,beta = 0.05)

plot(VENts, type = "l", xlab = "meses", ylab=" ")
lines(VENholt$fitted,type="l", col="red")
legend(x="bottomright", legend = c("VENts","VENses2"), col = c("black","red"))



VENholt$model
summary(VENholt)

















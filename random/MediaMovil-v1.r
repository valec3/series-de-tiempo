# Librerias necesarias
library(ggplot2)
library(forecast)
library(TTR)
library(readxl)
###########################################################

# Cargar datos:
# # file.choose() abre una ventana para seleccionar el archivo
datos = read_excel("F:/777--Programacion repos/Una/r/data/Ejem_2_1.xlsx")
View(datos)

#Convertir los datos a serie temporal

yts <- ts(datos$Y, start = c(2011,1), frequency = 12)
print(yts)

#Graficar la serie temporal with ggplot2

ggplot(data = NULL, aes(x = time(yts), y = yts)) +
    geom_line() +
    labs(x = "Meses", y = "Y") +
    ggtitle("Serie Temporal") +
    theme_minimal()

# Calculo de la media
k = 12
yts_ma <- SMA(yts,k)
yts_ma_df <- data.frame(time = time(yts), yts_ma = as.numeric(yts_ma))

# Graficar la serie temporal con la media movil
ggplot() +
    geom_line(data = data.frame(time = time(yts), yts = yts), aes(x = time, y = yts, color = "yts")) +
    geom_line(data = yts_ma_df, aes(x = time, y = yts_ma, color = "yts_ma")) +
    labs(x = "Meses", y = "Y") +
    ggtitle("Serie Temporal con Media Móvil") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(name = "Series", 
                        values = c("yts" = "black", "yts_ma" = "red"),
                        labels = c("yts", "yts_ma"))



# Calcular los pronósticos
pred <- forecast(yts_ma, h = 12)

# Convertir los pronósticos a un data frame
pred_df <- data.frame(time = time(pred$mean), pred = as.numeric(pred$mean))

# Graficar la serie temporal, la media móvil y los pronósticos con ggplot2
ggplot() +
  geom_line(data = data.frame(time = time(yts), yts = yts), aes(x = time, y = yts, color = "yts")) +
  geom_line(data = data.frame(time = time(yts), yts_ma = yts_ma), aes(x = time, y = yts_ma, color = "yts_ma")) +
  geom_line(data = pred_df, aes(x = time, y = pred, color = "pronósticos")) +
  labs(x = "Meses", y = "Y") +
  ggtitle("Serie Temporal con Media Móvil y Pronósticos") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Series", 
                     values = c("yts" = "black", "yts_ma" = "red", "pronósticos" = "blue"))

##############################################
####        MEDIA MOVIL DOBLE
##############################################

# Calcular la media móvil doble
k = 12
yts_ma2 = SMA(yts_ma,k)
a = 2*yts_ma - yts_ma2
b = (2/(k-1))*(yts_ma - yts_ma2)

p = 1
yma2 = a + b*p

# Convertir la media móvil doble a un data frame
yma2_df <- data.frame(time = time(yts), yma2 = as.numeric(yma2))

# Graficar la serie temporal original y la media móvil doble con ggplot2
ggplot() +
    geom_line(data = data.frame(time = time(yts), yts = yts), aes(x = time, y = yts), color = "black") +
    geom_line(data = yma2_df, aes(x = time, y = yma2), color = "blue") +
    labs(x = "Meses", y = "Y") +
    ggtitle("Serie Temporal con Media Móvil Doble") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(name = "Series", values = c("black", "blue"), labels = c("yts", "yma2"))

# Calcular los pronósticos
pred2 <- forecast(yma2, h = 12)

# Convertir los pronósticos a un data frame
pred2_df <- data.frame(time = time(pred2$mean), pred = as.numeric(pred2$mean))

# Graficar la serie temporal, la media móvil doble y los pronósticos con ggplot2
ggplot() +
    geom_line(data = data.frame(time = time(yts), yts = yts), aes(x = time, y = yts, color = "yts")) +
    geom_line(data = yma2_df, aes(x = time, y = yma2, color = "yma2")) +
    geom_line(data = pred2_df, aes(x = time, y = pred, color = "pronósticos")) +
    labs(x = "Meses", y = "Y") +
    ggtitle("Serie Temporal con Media Móvil Doble y Pronósticos") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(name = "Series", 
                        values = c("yts" = "black", "yma2" = "blue", "pronósticos" = "red"))


##############################################
#### Comparación entre los dos métodos
##############################################

# Graficar ambos metodos media movil y media movil doble

ggplot() +
    geom_line(data = data.frame(time = time(yts), yts = yts), aes(x = time, y = yts, color = "yts")) +
    geom_line(data = yts_ma_df, aes(x = time, y = yts_ma, color = "yts_ma")) +
    geom_line(data = yma2_df, aes(x = time, y = yma2, color = "yma2")) +
    labs(x = "Meses", y = "Y") +
    ggtitle("Comparación entre Media Móvil y Media Móvil Doble") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(name = "Series", values = c("yts" = "black", "yts_ma" = "red", "yma2" = "blue"))

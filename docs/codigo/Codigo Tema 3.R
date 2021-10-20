#----------------------------------------------------------
# CODIGO TEMA 3
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Libros
libros <- read.csv2("./series/libros.csv", 
                    header = TRUE)

libros <- ts(libros[, 2], start = 1993, 
             frequency  = 1)

autoplot(libros,
         xlab = "",
         ylab = "Títulos",
         main = "Títulos publicados (libros y folletos)")

# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos mensuales")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Metodos sencillos
#----------------------------------------------------------
# Libros
mediaLibros <- meanf(libros, h = 5)
naiveLibros <- naive(libros, h = 5)
derivaLibros <- rwf(libros,  h = 5, drift = TRUE)

summary(mediaLibros)
summary(naiveLibros) 
summary(derivaLibros)

accuracy(mediaLibros)
accuracy(naiveLibros)
accuracy(derivaLibros)

autoplot(libros, series = "Libros",
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción por métodos sencillos") +
  autolayer(mediaLibros, series="Media", PI = FALSE) +
  autolayer(naiveLibros, series="Ingenuo", PI = FALSE) +
  autolayer(derivaLibros, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Libros", "Media", "Ingenuo", "Deriva")) +
  guides(colour = guide_legend(title = "Métodos")) + 
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1))


# Nacimientos
snaive.nacimientos <- snaive(nacimientos, 
                             h = 24, 
                             level = 95)

accuracy(snaive.nacimientos)

autoplot(snaive.nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y predicción por el método Ingenuo con estacionalidad")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Evaluación de las predicciones: training set/test set
#----------------------------------------------------------
# Libros
librosIntra <- subset(libros, end = length(libros) - 6)
librosExtra <- subset(libros, start = length(libros) - 5)

librosExtraPre <- rwf(librosIntra,  h = 6, drift = TRUE)

accuracy(librosExtraPre, librosExtra)

autoplot(libros, series = "Libros",
         main="Libros, predicción intra- y extra-muestral",
         xlab="", 
         ylab="Títulos") +
  autolayer(fitted(librosExtraPre), series = "Libros (ajustada)") + 
  autolayer(librosExtraPre$mean, series = "Predicción") + 
  geom_vline(xintercept = 2012.5, lty = 2, col = "black") +
  scale_colour_manual(values=c("Libros"="black",
                               "Libros (ajustada)"="blue", 
                               "Predicción" = "red")) +
  guides(colour = guide_legend(title = "Series")) +
  annotate("text", x=1999, y=65000, label="6.5%", colour = "blue") +
  annotate("text", x=2016, y=72000, label="26.7%", colour = "red") +
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1)) 

# Nacimientos
nacimientosIntra <- subset(nacimientos, end = length(nacimientos) - 36)
nacimientosExtra <- subset(nacimientos, start = length(nacimientos) - 35)

nacimientosExtraPre <- snaive(nacimientosIntra, h = 36)

accuracy(nacimientosExtraPre, nacimientosExtra)

autoplot(nacimientos, series = "Nacimientos",
         main="Nacimientos, predicción intra- y extra-muestral",
         xlab="", 
         ylab="Nacimientos") +
  autolayer(fitted(nacimientosExtraPre), series = "Nacimientos (ajustada)") + 
  autolayer(nacimientosExtraPre$mean, series = "Predicción") + 
  scale_colour_manual(values=c("Nacimientos"="black",
                               "Nacimientos (ajustada)"="blue", 
                               "Predicción" = "red")) +
  guides(colour = guide_legend(title = "Series")) +
  annotate("text", x=2012, y=45000, label="3.6%", colour = "blue") +
  annotate("text", x=2018, y=40000, label="7.7%", colour = "red") + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Evaluación de las predicciones:  Origen de predicción móvil
#----------------------------------------------------------
# Nacimientos
nacAnual <- aggregate(nacimientos, FUN = sum)
k <- 20                   
h <- 5                    
TT <- length(nacAnual)    
s <- TT - k - h           

mapeRwf <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(nacAnual, start = i + 1, end = i + k)
  test.set <-  subset(nacAnual, start = i + k + 1, end = i + k + h)
  
  fcast <- rwf(train.set, h = h, drift = TRUE)
  mapeRwf[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeRwf <- colMeans(mapeRwf)
round(mapeRwf, 2)

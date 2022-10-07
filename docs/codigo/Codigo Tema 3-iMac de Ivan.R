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

libros <- ts(libros[, 2], 
             start = 1993, 
             frequency  = 1)

autoplot(libros,
         xlab = "",
         ylab = "Títulos",
         main = "")

# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Demanda electrica
electricidad <- read.csv2("./series/Consumo electrico.csv", 
                          header = TRUE)

electricidad <- ts(electricidad[, 2],
                   start = c(1, 5),
                   frequency = 7)

autoplot(electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")
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

autoplot(libros, series = "Libros",
         xlab = "",
         ylab = "Títulos",
         main = "") +
  autolayer(mediaLibros, series="Media", PI = FALSE) +
  autolayer(naiveLibros, series="Ingenuo", PI = FALSE) +
  autolayer(derivaLibros, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Libros", "Media", "Ingenuo", "Deriva")) +
  guides(colour = guide_legend(title = "Métodos")) + 
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1))

accuracy(mediaLibros)
accuracy(naiveLibros)
accuracy(derivaLibros)

# Nacimientos
snaive.nacimientos <- snaive(nacimientos, 
                             h = 24, 
                             level = 95)
summary(snaive.nacimientos)

accuracy(snaive.nacimientos)

autoplot(snaive.nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Demanda electrica
snaive.electricidad <- snaive(electricidad, 
                              h = 28, 
                              level = 95)
summary(snaive.electricidad)

accuracy(snaive.electricidad)

autoplot(snaive.electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Evaluación de las predicciones: training set/test set
#----------------------------------------------------------
# Libros
librosIntra <- subset(libros, end = length(libros) - 7)
librosExtra <- subset(libros, start = length(libros) - 6)

librosExtraPre <- rwf(librosIntra,  h = 7, drift = TRUE)

accuracy(librosExtraPre, librosExtra)

# Nacimientos
nacimientosIntra <- subset(nacimientos, end = length(nacimientos) - 36)
nacimientosExtra <- subset(nacimientos, start = length(nacimientos) - 35)

nacimientosExtraPre <- snaive(nacimientosIntra, h = 36)

accuracy(nacimientosExtraPre, nacimientosExtra)

# Demanda electrica
electricidadIntra <- subset(electricidad, end = length(electricidad) - 56)
electricidadExtra <- subset(electricidad, start = length(electricidad) - 55)

electricidadExtraPre <- snaive(electricidadIntra, h = 56)

accuracy(electricidadExtraPre, electricidadExtra)
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

# Demanda electrica
k <- 140                  
h <- 28                   
TT <- length(electricidad)
s <- TT - k - h           

mapeRwf <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(electricidad, start = i + 1, end = i + k)
  test.set <-  subset(electricidad, start = i + k + 1, end = i + k + h)
  
  fcast <- snaive(train.set, h = h)
  mapeRwf[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeRwf <- colMeans(mapeRwf)
round(mapeRwf, 2)

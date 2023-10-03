#----------------------------------------------------------
# CODIGO TEMA 5
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

nacimientosb <- window(nacimientos, start = 2000)

autoplot(nacimientosb,
         xlab = "",
         ylab = "Bebés",
         main = "")

# Demanda electrica
electricidad <- read.csv2("./series/Consumo electrico.csv", 
                          header = TRUE)

electricidad <- ts(electricidad[, 1],
                   start = c(1, 6),
                   frequency = 7)

electricidad <- window(electricidad, start = c(6, 1), end = c(22, 7)) 

electricidadSemanal <- aggregate(electricidad, FUN = sum)

autoplot(electricidadSemanal,
         xlab = "",
         ylab = "GWh",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial
#----------------------------------------------------------
# Alisado simple
electricidadEts <- ets(electricidadSemanal, 
                       model = "ANN")

summary(electricidadEts)

electricidadEts$states

electricidadf <- forecast(electricidadEts,
                          h = 5, 
                          level = 95)
electricidadf

autoplot(electricidadf,
         xlab = "",
         ylab = "GWh",
         main = "")

# Alisado de Holt
librosEts <- ets(libros, 
                 model = "AAN",
                 damped = FALSE)

summary(librosEts)

librosEts$states

librosf <- forecast(librosEts,
                    h = 5, 
                    level = 95)
librosf

autoplot(librosf,
         xlab = "",
         ylab = "Títulos",
         main = "")

# Alisado de Holt con pendiente amortiguada
librosEtsD <- ets(libros, 
                  model = "AAN", 
                  damped = TRUE)

summary(librosEtsD)

librosfD <- forecast(librosEtsD,
                     h = 15,
                     level = 95)
librosfD

autoplot(librosfD,
         xlab = "",
         ylab = "Títulos",
         main = "",
         PI = FALSE)

# Alisado de HW Aditivo
electricidadEts <- ets(electricidad, 
                       model = "AAA", 
                       damped = FALSE)
summary(electricidadEts)

TT <- nrow(electricidadEts$states)
electricidadEts$states[TT,]

electricidadEts$states[TT, 1] + (1:7)*electricidadEts$states[TT, 2] + 
  electricidadEts$states[TT, 9:3]

electricidadf <- forecast(electricidadEts,
                          h = 14, 
                          level = 95)
electricidadf

autoplot(electricidadf,
         xlab = "",
         ylab = "GWh",
         main = "",
         PI = FALSE)

# Alisado de HW multiplicativo 
nacimientosbEts <- ets(nacimientosb, 
                       model = "MAM", 
                       damped = FALSE)

summary(nacimientosbEts)

TT <- nrow(nacimientosbEts$states)
nacimientosbEts$states[TT,]

(nacimientosbEts$states[TT, 1] + (1:12)*nacimientosbEts$states[TT, 2]) * 
  nacimientosbEts$states[TT, 14:3]

nacimientosbf <- forecast(nacimientosbEts,
                          h = 24, 
                          level = 95)
nacimientosbf

autoplot(nacimientosbf,
         xlab = "",
         ylab = "Nacimientos",
         main = "",
         PI = FALSE)

# Alisado de Holt-Winters con transformacion logaritmica
nacimientosbEtsl <- ets(nacimientosb, 
                        model = "AAA",
                        damped = FALSE,
                        lambda = 0, 
                        biasadj = TRUE)

summary(nacimientosbEtsl)

nacimientosbfl <- forecast(nacimientosbEtsl,
                           h = 24,
                           level = 95,
                           biasadj = TRUE)
nacimientosbfl

autoplot(nacimientosb,
         xlab = "",
         ylab = "Nacimientos",
         main = "") + 
  autolayer(nacimientosbf, series = "Nacimientos", PI = FALSE) + 
  autolayer(nacimientosbfl, series = "Nacimientos (log)", PI = FALSE) + 
  guides(colour = guide_legend(title = "Predicción")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Libros
#----------------------------------------------------------
# Ajuste
librosEts <- ets(libros)
summary(librosEts) 

# Prediccion
librosEtsPre <- forecast(librosEts, 
                         h = 5,
                         level = 95)
librosEtsPre

autoplot(librosEtsPre,
         xlab = "",
         ylab = "Títulos",
         main = "")

# Analisis error
error <- residuals(librosEts, type = "response")
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1993, 2019, 2)) 

# Error extramuestral: training set/test set
# Definimos las observaciones intra- y extra-muestrales
librosIntra <- subset(libros, end = length(libros) - 6)
librosExtra <- subset(libros, start = length(libros) - 5)

librosIntraEts <- ets(librosIntra, model = "MNN")

librosExtraPre <- forecast(librosIntraEts, h = 6)

accuracy(librosExtraPre, librosExtra)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Nacimientos
#----------------------------------------------------------
# Ajuste
nacimientosEts <- ets(nacimientosb, 
                      damped = FALSE)

summary(nacimientosEts) 

autoplot(nacimientosEts,
         xlab = "Periodo",
         main = "")

# Prediccion
TT <- nrow(nacimientosEts$states)
nacimientosEts$states[TT,]

nacimientosEts$states[TT, 1] + (1:12) * nacimientosEts$states[TT, 2] + nacimientosEts$states[TT, 14:3]

nacimientosEtsPre <- forecast(nacimientosEts, 
                              h = 24, 
                              level = 95)
nacimientosEtsPre

autoplot(nacimientosEtsPre,
         xlab = "",
         ylab = "Bebés",
         main = "")

# Analisis del error
error <- residuals(nacimientosEts, type = "response")
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2019, 2)) 

abs(error) > 3 * sderror
time(error)[abs(error) > 3 * sderror]

# Error extramuestral: origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(nacimientosb)
s <- TT - k - h          

mapeAlisado <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(nacimientosb, start = i + 1, end = i + k)
  test.set <-  subset(nacimientosb, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MAA", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorAlisado <- colMeans(mapeAlisado)
errorAlisado

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado)) +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Demanda electrica
#----------------------------------------------------------
# Ajuste
electricidadEts <- ets(electricidad)
summary(electricidadEts) 

# Prediccion
TT <- nrow(electricidadEts$states)
electricidadEts$states[TT,]

electricidadEts$states[TT, 1] + electricidadEts$states[TT, 8:2]

electricidadEtsPre <- forecast(electricidadEts, 
                               h = 28, 
                               level = 95)
electricidadEtsPre

autoplot(electricidadEtsPre,
         xlab = "",
         ylab = "GWh",
         main = "")

# Analisis del error
error <- residuals(electricidadEts)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Semana",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(6, 26, 2)) 

abs(error) > 3 * sderror
time(error)[abs(error) > 3 * sderror]

# Error extramuestral: origen de prediccion movil
k <- 70                
h <- 14                  
TT <- length(electricidad)
s <- TT - k - h          

mapeAlisado <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(electricidad, start = i + 1, end = i + k)
  test.set <-  subset(electricidad, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "ANA", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorAlisado <- colMeans(mapeAlisado)
errorAlisado

ggplot() +
  geom_line(aes(x = 1:14, y = errorAlisado)) +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:14)

#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Otras alternativas para predecir Nacimientos
#----------------------------------------------------------
# Serie Nacimientos
accuracy(ets(nacimientosb))[5]

accuracy(ets(nacimientosb, 
             opt.crit = "mse"))[5]

accuracy(ets(nacimientosb, 
             opt.crit = "amse",
             nmse = 4))[5]

# Transformación logarítmica
accuracy(ets(nacimientosb, 
             lambda = 0))[5]

accuracy(ets(nacimientosb, 
             lambda = 0, 
             opt.crit = "mse"))[5]

accuracy(ets(nacimientosb, 
             lambda = 0, 
             opt.crit = "amse",
             nmse = 4))[5]

# Transformación logarítmica insesgada
accuracy(ets(nacimientosb, 
             lambda = 0,
             biasadj = TRUE))[5]

accuracy(ets(nacimientosb, 
             lambda = 0, 
             biasadj = TRUE,
             opt.crit = "mse"))[5]

accuracy(ets(nacimientosb, 
             lambda = 0, 
             biasadj = TRUE,
             opt.crit = "amse",
             nmse = 4))[5]

# Nacimientos por dia
accuracy(ets(nacimientosb/monthdays(nacimientosb)))[5]

accuracy(ets(nacimientosb/monthdays(nacimientosb), 
             opt.crit = "mse"))[5]

accuracy(ets(nacimientosb/monthdays(nacimientosb), 
             opt.crit = "amse",
             nmse = 4))[5]


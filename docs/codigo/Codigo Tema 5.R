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
         main = "Títulos publicados (libros y folletos)")

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
         main = "Nacimientos")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial
#----------------------------------------------------------
# Alisado simple
librosf <- ses(libros, 
               h = 5, 
               level = 95)

summary(librosf)

librosf$model$states

autoplot(librosf,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado simple")

# Alisado de Holt
librosf <- holt(libros, 
                h = 5, 
                level = 95)

summary(librosf)

librosf$model$states

autoplot(librosf,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado de Holt")

# Alisado de Holt con pendiente amortiguada
librosfd <- holt(libros, 
                 damped = TRUE, 
                 h = 15, 
                 phi = 0.9)

summary(librosfd)

autoplot(librosfd,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado exponencial con amortiguamiento",
         PI = FALSE)

# Alisado de Holt-Winters
nacimientosbf <- hw(nacimientosb, 
                    seasonal = "mult", 
                    h = 24)

summary(nacimientosbf)

TT <- nrow(nacimientosbf$model$states)
nacimientosbf$model$states[TT,]

(nacimientosbf$model$states[TT, 1] + (1:12)*nacimientosbf$model$states[TT, 2]) * 
  nacimientosbf$model$states[TT, 14:3]

autoplot(nacimientosbf,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y predicción con alisado de Holt-Winters multiplicativo",
         PI = FALSE)

# Alisado de Holt-Winters con transformacion logaritmica
nacimientosbfl <- hw(nacimientosb, 
                     seasonal = "addit", 
                     h = 24, 
                     lambda = 0, 
                     biasadj = TRUE)

summary(nacimientosbfl)

autoplot(nacimientosb,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y dos predicciones con alisado de Holt-Winters") + 
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
                         h = 5)
librosEtsPre

autoplot(librosEtsPre,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción a 5 años vista")

# Analisis error
error <- residuals(librosEts, type = "response")
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1993, 2019, 2)) 

# Error extramuestral: training set/test set
librosIntra <- subset(libros, end = length(libros) - 6)
librosExtra <- subset(libros, start = length(libros) - 5)

librosIntraEts <- ets(librosIntra, model = "MNN", damped = FALSE)

librosExtraPre <- forecast(librosIntraEts, h = 6)

accuracy(librosExtraPre, librosExtra)

autoplot(libros, series = "Libros",
         main="Libros, predicción intra- y extra-muestral",
         xlab="", 
         ylab="Títulos") +
  autolayer(fitted(librosIntraEts), series = "Libros (ajustada)") + 
  autolayer(librosExtraPre$mean, series = "Predicción") + 
  geom_vline(xintercept = 2012.5, lty = 2, col = "black") +
  scale_colour_manual(values=c("Libros"="black",
                               "Libros (ajustada)"="blue", 
                               "Predicción" = "red")) +
  guides(colour = guide_legend(title = "Series")) +
  annotate("text", x=1999, y=65000, label="7.2%", colour = "blue") +
  annotate("text", x=2016, y=72000, label="17.8%", colour = "red") +
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Nacimientos
#----------------------------------------------------------
# Ajuste
nacimientosEts <- ets(nacimientosb, damped = FALSE)

summary(nacimientosEts) 

autoplot(nacimientosEts,
         xlab = "Periodo",
         main = "Componentes del modelo óptimo para Nacimientos")

# Prediccion
TT <- nrow(nacimientosEts$states)
nacimientosEts$states[TT,]

nacimientosEts$states[TT, 1] + (1:12) * nacimientosEts$states[TT, 2] + nacimientosEts$states[TT, 14:3]

nacimientosEtsPre <- forecast(nacimientosEts, h = 24, level = 95)
nacimientosEtsPre

autoplot(nacimientosEtsPre,
         xlab = "",
         ylab = "Bebés",
         main = "Nacimientos y predicción")

# Analisis del error
error <- residuals(nacimientosEts, type = "response")
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2019, 2)) 

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
  ggtitle("Error de predicción según horizonte temporal") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Otras alternativas para predecir Nacimientos
#----------------------------------------------------------
# Serie Nacimientos
accuracy(ets(nacimientosb, 
             damped = FALSE))[5]
accuracy(ets(nacimientosb, 
             damped = FALSE, 
             opt.crit = "mse"))[5]

# Transformación logarítmica
accuracy(ets(nacimientosb, 
             lambda = 0, 
             damped = FALSE))[5]
accuracy(ets(nacimientosb, 
             lambda = 0, 
             damped = FALSE, 
             opt.crit = "mse"))[5]

# Transformación logarítmica insesgada
accuracy(ets(nacimientosb, 
             lambda = 0, 
             biasadj = TRUE,
             damped = FALSE))[5]
accuracy(ets(nacimientosb, 
             lambda = 0, 
             biasadj = TRUE,
             damped = FALSE, 
             opt.crit = "mse"))[5]

# Nacimientos por dia
accuracy(ets(nacimientosb/monthdays(nacimientosb), 
             damped = FALSE))[5]
accuracy(ets(nacimientosb/monthdays(nacimientosb), 
             damped = FALSE, 
             opt.crit = "mse"))[5]


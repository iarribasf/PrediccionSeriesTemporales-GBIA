#----------------------------------------------------------
# CODIGO TEMA 6
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(aod)
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
             frequency = 1)

autoplot(libros,
         xlab = "", 
         ylab = "", 
         main = "Títulos publicados")

# Aforo vehículos
aforo <- read.csv2("./series/aforo_oropesa.csv", 
                   header = TRUE)

aforo <- ts(aforo, 
            start = 1960, 
            freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos (000)",
         main = "Aforo de vehículos en N-340, Oropesa")

# Consumo de alimnetos per capita
alimentospc <- read.csv2("./series/alimentacionpc.csv", 
                         header = TRUE)

alimentospc <- ts(alimentospc, 
                  start = 1987, 
                  freq = 1)

autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "Consumo alimentario en hogar")

# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

nacimientosb <- window(nacimientos, start = 2000)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Transformaciones: log y diff
#----------------------------------------------------------
# Diff y log
cbind("Nacidos" = nacimientos,
      "Dif. de log nacidos" = diff(log(nacimientos))) %>%
  autoplot(facets = TRUE,
           xlab = "",
           ylab = "",
           main = "Nacimientos y diferencia del logaritmo de Nacimientos")

# Diff
cbind("Nacidos" = nacimientos,
      "Dif. regular" = diff(nacimientos),
      "Dif. estacional" = diff(nacimientos, lag = 12),
      "Dif. reg. y esta." = diff(diff(nacimientos, lag = 12))) %>%
  autoplot(facets = TRUE,
           xlab = "",
           ylab = "",
           main = "Nacimientos")

# Log
cbind("Nacidos" = nacimientos,
      "log(Nacidos)" = log(nacimientos)) %>%
  autoplot(facets = TRUE,
           xlab = "",
           ylab = "",
           main = "Serie Nacimientos y su transformaciones logarítmica")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion de autocorrelacion
#----------------------------------------------------------
# Graficas
ggAcf(nacimientos, lag = 48)
ggAcf(log(nacimientos), lag = 48)
ggAcf(diff(nacimientos), lag = 48)
ggAcf(diff(log(nacimientos)), lag = 48)
ggAcf(diff(nacimientos, lag = 12),lag = 48)
ggAcf(diff(log(nacimientos), lag = 12), lag = 48)
ggAcf(diff(diff(nacimientos, lag=12)), lag = 48)
ggAcf(diff(diff(log(nacimientos), lag=12)), lag = 48)

# Autocorrelaciones
ggAcf(diff(diff(nacimientos), lag = 12), 
      lag=12, 
      plot = FALSE)

#- Ergodicidad
ndiffs(nacimientos)
nsdiffs(nacimientos)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Libros
#----------------------------------------------------------
# Transformacion
autoplot(libros, 
         xlab = "", 
         ylab = "", 
         main = "Libros")
autoplot(diff(libros), 
         xlab = "", 
         ylab = "", 
         main = "Diferencia libros")

ggAcf(libros, 
      xlab = "", 
      ylab = "FAC", 
      main = "")
ggAcf(diff(libros), 
      xlab = "", 
      ylab = "FAC", 
      main = "")

ndiffs(libros)

# Identificacion
auto.arima(libros, 
           trace = TRUE)

# Estimacion
arima010 <- Arima(libros, 
                  order=c(0, 1, 0), 
                  include.constant = FALSE)
arima010

# Intervencion
error <- residuals(arima010)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1993, 2019, 2)) 

# Error de ajuste
accuracy(arima010)

# Prediccion
parima010 <- forecast(arima010, 
                      h = 5, 
                      level = 95)
parima010

autoplot(parima010, 
         xlab = "", 
         ylab = "Títulos",
         main = "Libros (1993-2018) y predicción (2019-2023)") +
  scale_x_continuous(breaks= seq(1993, 2023, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Aforo de vehículos
#----------------------------------------------------------
# Transformacion
autoplot(log(aforo), 
         xlab = "log(Aforo)", 
         ylab = "", 
         main = "")
autoplot(diff(log(aforo)), 
         xlab = "Una diferencia de log(Aforo)", 
         ylab = "", 
         main = "")
autoplot(diff(log(aforo), differences = 2), 
         xlab = "Dos diferencias de log(Aforo)", 
         ylab = "", 
         main = "")

ggAcf(log(aforo), 
      xlab = "", 
      ylab = "FAC", 
      main = "")
ggAcf(diff(log(aforo)), 
      xlab = "", 
      ylab = "FAC", 
      main = "")
ggAcf(diff(log(aforo), differences = 2), 
      xlab = "", 
      ylab = "FAC", 
      main = "")

ndiffs(log(aforo))

# Identificacion
auto.arima(aforo, 
           lambda = 0)

# Estimacion
arima022 <- Arima(aforo, 
                  order = c(0, 2, 2),
                  lambda = 0)

# Intervencion
error <- residuals(arima022)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2014, 4)) 

# Identificacion y Estimacion
d1979 <- 1*(time(error) == 1979)
d1981 <- 1*(time(error) == 1981)
d1984 <- 1*(time(error) == 1984)
d2011 <- 1*(time(error) == 2011)

auto.arima(aforo, 
           lambda = 0, 
           xreg = cbind(d1979, d1981, d1984, d2011))

arima120 <- Arima(aforo, 
                  order = c(1, 2, 0), 
                  lambda = 0,  
                  xreg = cbind(d1979, d1981, d1984, d2011))
arima120

# Coeficientes significativos
wald.test(b = coef(arima120), 
          Sigma = vcov(arima120), 
          Terms = 1)

wald.test(b = coef(arima120), 
          Sigma = vcov(arima120), 
          Terms = 2)

wald.test(b = coef(arima120), 
          Sigma = vcov(arima120), 
          Terms = 3)

wald.test(b = coef(arima120), 
          Sigma = vcov(arima120), 
          Terms = 4)

wald.test(b = coef(arima120), 
          Sigma = vcov(arima120), 
          Terms = 5)

# Intervencion
error <- residuals(arima120)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2014, 4)) 

# Error de ajuste
accuracy(arima120)

# Prevision
parima120 <- forecast(arima120, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 5), d1981=rep(0, 5), 
                                   d1984=rep(0, 5), d2011=rep(0, 5)))
parima120

autoplot(parima120, 
         ylab = 'Vehículos (000)',
         main = 'Aforo (1960-2018) y predicción (2019-2023)') +
  scale_x_continuous(breaks= seq(1960, 2023, 4)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Aforo de vehículos: eleccion del proceso
#----------------------------------------------------------
p <- 0:3
d <- 0:2
q <- 0:3
l <- 0:1

parametros <- expand.grid(p,d,q,l)

colnames(parametros) <- c("p", "d", "q", "log")

k <- 40                 
h <-  5                 
TT <- length(aforo)     
s <- TT - k - h         

MAPE <- matrix(NA, nrow(parametros), h)

for (para in 1:nrow(parametros)) {
  
  identificacion <- as.numeric(parametros[para, - 4])
  mapeArima <- matrix(NA, s + 1, h)
  for (i in 0:s) {
    train.set <- subset(aforo, start = i + 1, end = i + k)
    test.set <-  subset(aforo, start = i + k + 1, end = i + k + h)
    
    if(parametros[para, 4] == 0) 
      fit <- Arima(train.set, order = identificacion, method = "ML") else 
        fit <- Arima(train.set, order = identificacion, 
                     lambda = 0, method = "ML")
    fcast <- forecast(fit, h = h)
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
  
  MAPE[para, ] <- colMeans(mapeArima)
  
}

# Mejores modelos si h = 1
ii <- order(MAPE[, 1], 
            decreasing = FALSE)

cbind(parametros[ii[1:3],], 
      error = round(MAPE[ii[1:3], 1], 3))

# Mejores modelos si h = 2
ii <- order(MAPE[, 2], 
            decreasing = FALSE)

cbind(parametros[ii[1:3],], 
      error = round(MAPE[ii[1:3], 2], 3))

# Mejores modelos si h = 3
ii <- order(MAPE[, 3], 
            decreasing = FALSE)

cbind(parametros[ii[1:3],], 
      error = round(MAPE[ii[1:3], 3], 3))

# Mejores modelos si h = 4
ii <- order(MAPE[, 4], 
            decreasing = FALSE)

cbind(parametros[ii[1:3],], 
      error = round(MAPE[ii[1:3], 4], 3))

# Mejores modelos si h = 5
ii <- order(MAPE[, 5], 
            decreasing = FALSE)

cbind(parametros[ii[1:3],], 
      error = round(MAPE[ii[1:3], 5], 3))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Consumo de alimnetos per capita
#----------------------------------------------------------
# Transformacion
autoplot(alimentospc, 
         xlab = "", 
         ylab = "", 
         main = "Alimentos")
autoplot(diff(alimentospc), 
         xlab = "", 
         ylab = "",
         main = "Diferencia alimentos")

ggAcf(alimentospc, 
      xlab = "", 
      ylab = "FAC", 
      main = "")
ggAcf(diff(alimentospc), 
      xlab = "", 
      ylab = "FAC", 
      main = "")

ndiffs(alimentospc)

# Identificacion
auto.arima(alimentospc)

# Estimacion
arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0))

# Intervencion
error <- residuals(arima100)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1987, 2018, 3)) 

# Significatividad
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 1)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 2)

# Error de ajuste
accuracy(arima100)

# Prediccion
parima100 <- forecast(arima100, 
                      h = 5, 
                      level = 95)

parima100

autoplot(parima100, 
         ylab = "Kilos per cápita",
         main = "Consumo de alimentos y predicción") +
  scale_x_continuous(breaks= seq(1987, 2023, 4)) 


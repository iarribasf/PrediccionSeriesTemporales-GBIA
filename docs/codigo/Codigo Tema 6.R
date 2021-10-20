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

# Diferenciaciones
ndiffs(nacimientos)
nsdiffs(nacimientos)

# Autocorrelaciones
ggAcf(diff(diff(nacimientos), lag = 12), 
      lag = 12, 
      plot = FALSE)
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
autoplot(aforo, 
         xlab = "log(Aforo)", 
         ylab = "", 
         main = "")
autoplot(diff(aforo), 
         xlab = "Una diferencia de log(Aforo)", 
         ylab = "", 
         main = "")
autoplot(diff(aforo, differences = 2), 
         xlab = "Dos diferencias de log(Aforo)", 
         ylab = "", 
         main = "")

ggAcf(aforo, 
      xlab = "", 
      ylab = "FAC", 
      main = "")
ggAcf(diff(aforo), 
      xlab = "", 
      ylab = "FAC", 
      main = "")
ggAcf(diff(aforo, differences = 2), 
      xlab = "", 
      ylab = "FAC", 
      main = "")

ndiffs(aforo)

# Identificacion, Intervencion y Estimacion
auto.arima(aforo)

arima212 <- Arima(aforo, 
                  order = c(2, 1, 2))

error <- residuals(arima212)
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

d1979 <- 1*(time(error) == 1979)
d2011 <- 1*(time(error) == 2011)

auto.arima(aforo,
           xreg = cbind(d1979,  d2011))

arima210 <- Arima(aforo, 
                  order = c(2, 1, 0),
                  xreg = cbind(d1979, d2011))
arima210

error <- residuals(arima210)
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

# Validacion: Coeficientes significativos
wald.test(b = coef(arima210), 
          Sigma = vcov(arima210), 
          Terms = 1)

wald.test(b = coef(arima210), 
          Sigma = vcov(arima210), 
          Terms = 2)

wald.test(b = coef(arima210), 
          Sigma = vcov(arima210), 
          Terms = 3)

# Error de ajuste
accuracy(arima210)

# Prevision
parima210 <- forecast(arima210, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 5), d1981=rep(0, 5)))

parima210

autoplot(parima210, 
         ylab = 'Vehículos (000)',
         main = 'Aforo (1960-2018) y predicción (2019-2023)') +
  scale_x_continuous(breaks= seq(1960, 2023, 4)) 

# Validación con origen de predicción movil
k <- 30                  
h <- 5                    
T <- length(aforo)     
s <- T - k - h    

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(d1979, d2011))

for (i in 0:s) {
  train.set <- subset(aforo, start = i + 1, end = i + k)
  test.set <-  subset(aforo, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     order = c(2, 1, 0),
                     xreg=as.matrix(X.train)))
  } else {
    fit <- try(Arima(train.set, 
                     order = c(2, 1, 0)))
  }
  
  if (!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) 
      fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
        fcast <- forecast(fit, h = h)
      mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

mapeArima <- colMeans(mapeArima, na.rm = TRUE)
mapeArima
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Consumo de alimentos per capita
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

# Identificacion, Intervencion y Estimacion
auto.arima(alimentospc)

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0))

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

d1993 <- 1* (time(alimentospc) == 1993)
d1995 <- 1* (time(alimentospc) == 1995)
d2009 <- 1* (time(alimentospc) == 2009)

arima100 <- Arima(alimentospc, 
                  include.constant = TRUE,
                  order = c(1, 0, 0),
                  xreg = cbind(d1993, d1995, d2009))
arima100

# Significatividad
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 1)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 2)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 3)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 4)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 5)

# Error de ajuste
accuracy(arima100)

# Prediccion
parima100 <- forecast(arima100, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(rep(0, 5), rep(0, 5), rep(0, 5)))

parima100

autoplot(parima100, 
         ylab = "Kilos per cápita",
         main = "Consumo de alimentos y predicción") +
  scale_x_continuous(breaks= seq(1987, 2023, 4)) 


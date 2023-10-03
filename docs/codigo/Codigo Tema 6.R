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
library(lmtest)
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
         main = "")

# Aforo vehículos
aforo <- read.csv2("./series/aforo_oropesa.csv", 
                   header = TRUE)

aforo <- ts(aforo, 
            start = 1960, 
            freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos (000)",
         main = "")

# Consumo de alimentos per capita
alimentospc <- read.csv2("./series/alimentacionpc.csv", 
                         header = TRUE)

alimentospc <- ts(alimentospc, 
                  start = 1990, 
                  freq = 1)

autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "")

# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

nacimientosAnual <- aggregate(nacimientos, FUN = sum)

autoplot(nacimientosAnual, 
         xlab = "", 
         ylab = "Bebés",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Transformaciones: diff y log
#----------------------------------------------------------
# Diferencia
nacimientosAnual
diff(nacimientosAnual)
diff(nacimientosAnual, differences = 2)

cbind("Nacidos" = nacimientosAnual,
      "Dif. regular" = diff(nacimientosAnual),
      "Doble dif. regular" = diff(nacimientosAnual, differences = 2)) %>%
  autoplot(facets = TRUE,
           xlab = "",
           ylab = "",
           main = "")

# Log
nacimientos
log(nacimientos)

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
ggAcf(nacimientosAnual, lag = 10)
ggAcf(log(nacimientosAnual), lag = 10)
ggAcf(diff(nacimientosAnual), lag = 10)
ggAcf(diff(log(nacimientosAnual)), lag = 10)
ggAcf(diff(nacimientosAnual, differences = 2), lag = 10)
ggAcf(diff(log(nacimientosAnual), differences = 2), lag = 10)

ndiffs(nacimientos)

ggAcf(diff(nacimientosAnual, differences = 2), 
      lag=10, 
      plot = FALSE)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Libros
#----------------------------------------------------------
# Transformacion
autoplot(libros, xlab = "", ylab = "", main = "")
autoplot(diff(libros), xlab = "", ylab = "", main = "")

ggAcf(libros, xlab = "", ylab = "", main = "")
ggAcf(diff(libros), xlab = "", ylab = "", main = "")

ndiffs(libros)

# Identificacion
auto.arima(libros, 
           d = 1, 
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
         main = "") +
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
         main = "") +
  scale_x_continuous(breaks= seq(1993, 2024, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Aforo de vehículos
#----------------------------------------------------------
# Transformacion
autoplot(aforo, xlab = "", ylab = "", main = "")
autoplot(diff(aforo), xlab = "", ylab = "", main = "")

ggAcf(aforo, xlab = "", ylab = "", main = "")
ggAcf(diff(aforo), xlab = "", ylab = "", main = "")

ndiffs(aforo)

# Identificacion, Intervencion y Estimacion
auto.arima(aforo,
           d = 1)

arima010 <- Arima(aforo, 
                  order = c(0, 1, 0),
                  include.constant = FALSE)

error <- residuals(arima010)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2020, 4)) 

time(error)[abs(error) > 2.5*sderror]

d1979 <- 1*(time(error) == 1979)
d2011 <- 1*(time(error) == 2011)
d2020 <- 1*(time(error) == 2020)

auto.arima(aforo,
           d = 1,
           xreg = cbind(d1979,  d2011, d2020))

arima210 <- Arima(aforo, 
                  order = c(2, 1, 0),
                  include.constant = FALSE,
                  xreg = cbind(d1979, d2011, d2020))
arima210

error <- residuals(arima210)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2020, 4)) 

# Validacion: Coeficientes significativos
coeftest(arima210)

# Error de ajuste
accuracy(arima210)

# Prevision
parima210 <- forecast(arima210, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 5), d2011=rep(0, 5), d2020=rep(0, 5)))
parima210

autoplot(parima210, 
         xlab = "",
         ylab = "Vehículos (000)",
         main = "") +
  scale_x_continuous(breaks= seq(1960, 2026, 4))  

# Validación con origen de predicción movil
k <- 30                  
h <- 5                    
T <- length(aforo)     
s <- T - k - h    

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(d1979, d2011, d2020))

for (i in 0:s) {
  train.set <- subset(aforo, start = i + 1, end = i + k)
  test.set <-  subset(aforo, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- Arima(train.set, 
                 include.constant = FALSE,
                 order = c(2, 1, 0),
                 xreg=as.matrix(X.train))
    
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test))
  } else {
    fit <- Arima(train.set, 
                 include.constant = FALSE,
                 order = c(2, 1, 0))
    
    fcast <- forecast(fit, h = h)
  }
  
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeArima <- colMeans(mapeArima)
mapeArima
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Consumo de alimentos per capita
#----------------------------------------------------------
# Transformacion
autoplot(alimentospc, xlab = "", ylab = "", main = "", ylim = c(0, 700))
autoplot(diff(alimentospc), xlab = "", ylab = "", main = "")

ggAcf(alimentospc, xlab = "", ylab = "", main = "")
ggAcf(diff(alimentospc), xlab = "", ylab = "", main = "")

ndiffs(alimentospc)

# Identificacion, Intervencion y Estimacion
arima001 <- Arima(alimentospc, 
                  include.constant = TRUE,
                  order = c(0, 0, 1))

error <- residuals(arima001)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1990, 2022, 3)) 

time(alimentospc)[abs(error) > 2.5 * sderror]

d2020 <- 1* (time(alimentospc) == 2020)
auto.arima(alimentospc, d = 0, xreg = d2020)

d1993 <- 1* (time(alimentospc) == 1993)
d1995 <- 1* (time(alimentospc) == 1995)
d1999 <- 1* (time(alimentospc) == 1999)
d2022 <- 1* (time(alimentospc) == 2022)

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0),
                  include.constant = TRUE,
                  xreg = cbind(d1993, d1995, d1999, d2020, d2022))
arima100

# Significatividad
coeftest(arima100)

# Error de ajuste
accuracy(arima100)

# Prediccion
parima100 <- forecast(arima100, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(rep(0, 5), rep(0, 5), rep(0, 5), rep(0, 5), rep(0, 5)))
parima100

autoplot(parima100, 
         ylab = "Kilos per cápita",
         main = "") +
  scale_x_continuous(breaks= seq(1990, 2026, 4)) 


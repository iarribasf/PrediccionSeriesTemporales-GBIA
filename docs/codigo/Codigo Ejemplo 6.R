#----------------------------------------------------------
# CODIGO EJEMPLO 6
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
# Importamos Pernoctaciones
#----------------------------------------------------------
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[,2], 
                     start = 2000, 
                     freq = 12)

Pernoctaciones <- aggregate(Pernoctaciones/10^6, FUN = sum)

autoplot(Pernoctaciones,
         xlab = "",
         ylab = "Noches (millones)",
         main = "Pernoctaciones (datos anuales)") +
  scale_x_continuous(breaks= seq(2000, 2020, 2))  
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Transformacion
#----------------------------------------------------------
autoplot(Pernoctaciones, 
         xlab = "Serie original", 
         ylab = "", 
         main = "")

autoplot(diff(Pernoctaciones), 
         xlab = "Serie diferenciada", 
         ylab = "", 
         main = "")

ggAcf(Pernoctaciones,
      xlab = "", 
      ylab = "FAC", 
      main = "")

ggAcf(diff(Pernoctaciones), 
      xlab = "", 
      ylab = "FAC", 
      main = "")

ndiffs(Pernoctaciones)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Pernoctaciones - I(0)
#----------------------------------------------------------
# Identificación
auto.arima(Pernoctaciones, d = 0)

# Estimacion
arima200 <- Arima(Pernoctaciones, 
                  order = c(2, 0, 0),
                  include.constant = TRUE)

# Intervencion
error <- residuals(arima200)
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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

# Estimacion + Intervencion
d2009 <- 1*(time(Pernoctaciones) == 2009)

arima200 <- Arima(Pernoctaciones, 
                  order = c(2, 0, 0),
                  include.constant = TRUE,
                  xreg = d2009)
arima200

error <- residuals(arima200)
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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

# Variables son significativas
wald.test(b = coef(arima200), Sigma = vcov(arima200), Terms = 1)
wald.test(b = coef(arima200), Sigma = vcov(arima200), Terms = 2)
wald.test(b = coef(arima200), Sigma = vcov(arima200), Terms = 3)
wald.test(b = coef(arima200), Sigma = vcov(arima200), Terms = 4)

# Medidas de error
accuracy(arima200)

# Error de previsión extra-muestral: origen de prevision movil
k <- 10                  
h <- 3                    
T <- length(Pernoctaciones)     
s <- T - k - h    

mapeArima200 <- matrix(NA, s + 1, h)

X <- data.frame(d2009)

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- Arima(train.set, 
                 order = c(2, 0, 0),
                 include.constant = TRUE,
                 xreg=as.matrix(X.train))
  } else {
    fit <- Arima(train.set, 
                 include.constant = TRUE,
                 order = c(2, 0, 0))
  }
  
  
  if (length(X.train) > 0) 
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
      fcast <- forecast(fit, h = h)
  
  mapeArima200[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeArima200 <- colMeans(mapeArima200, na.rm = TRUE)
mapeArima200
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Pernoctaciones - I(1)
#----------------------------------------------------------
# Identificación
auto.arima(Pernoctaciones, d = 1)

# Estimacion + intervencion
arima010 <- Arima(Pernoctaciones, 
                  order = c(0, 1, 0),
                  include.constant = FALSE)

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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

arima010 <- Arima(Pernoctaciones, 
                  order = c(0, 1, 0),
                  include.constant = FALSE,
                  xreg = d2009)
arima010
wald.test(b = coef(arima010), Sigma = vcov(arima010), Terms = 1)

# Medidas de error
accuracy(arima010)

# rror de previsión extra-muestral: origen de prevision movil
k <- 10                  
h <- 3                    
T <- length(Pernoctaciones)     
s <- T - k - h    

mapeArima010 <- matrix(NA, s + 1, h)

X <- data.frame(d2009)

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- Arima(train.set, 
                 order = c(0, 1, 0),
                 xreg=as.matrix(X.train))
  } else {
    fit <- Arima(train.set,
                 order = c(0, 1, 0))
  }
  
  
  if (length(X.train) > 0) 
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
      fcast <- forecast(fit, h = h)
  
  mapeArima010[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeArima010 <- colMeans(mapeArima010, na.rm = TRUE)
mapeArima010

# Predicción
parima010 <- forecast(arima010, 
                      h = 5, 
                      level = 95,
                      xreg = rep(0, 5))
parima010


autoplot(parima010, 
         ylab = "Noches (millones)",
         main = "Pernoctaciones") +
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 


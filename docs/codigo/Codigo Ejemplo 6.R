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
                     frequency = 12)

Pernoctaciones <- aggregate(Pernoctaciones/10^6, FUN = sum)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Transformacion
#----------------------------------------------------------
autoplot(Pernoctaciones, xlab = "", ylab = "", main = "")
autoplot(diff(Pernoctaciones), xlab = "", ylab = "", main = "")
ggAcf(Pernoctaciones, xlab = "", ylab = "", main = "")
ggAcf(diff(Pernoctaciones), xlab = "", ylab = "", main = "")

ndiffs(Pernoctaciones)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Pernoctaciones - I(0)
#----------------------------------------------------------
# Identificación
auto.arima(Pernoctaciones, 
           d = 0)

# Estimacion
arima101 <- Arima(Pernoctaciones, 
                  order = c(1, 0, 1),
                  include.constant = TRUE)

# Intervencion
error <- residuals(arima101)
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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 


# Variables son significativas
wald.test(b = coef(arima101), Sigma = vcov(arima101), Terms = 1)
wald.test(b = coef(arima101), Sigma = vcov(arima101), Terms = 2)
wald.test(b = coef(arima101), Sigma = vcov(arima101), Terms = 3)

# Medidas de error
accuracy(arima101)

# Error de previsión extra-muestral: origen de prevision movil
k <- 10                  
h <- 3                    
T <- length(Pernoctaciones)     
s <- T - k - h    

mapeArima101 <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  fit <- Arima(train.set, 
               include.constant = TRUE,
               order = c(1, 0, 1))
  
  fcast <- forecast(fit, h = h)
  
  mapeArima101[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeArima101 <- colMeans(mapeArima101)
mapeArima101
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Pernoctaciones - I(1)
#----------------------------------------------------------
# Identificación
auto.arima(Pernoctaciones, 
           d = 1)

# Estimacion
arima010 <- Arima(Pernoctaciones, 
                  order = c(0, 1, 0),
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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

# Medidas de error
accuracy(arima010)

# Error de previsión extra-muestral: origen de prevision movil
k <- 10                  
h <- 3                    
T <- length(Pernoctaciones)     
s <- T - k - h    

mapeArima010 <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  fit <- Arima(train.set, 
               include.constant = FALSE,
               order = c(0, 1, 0))
  
  fcast <- forecast(fit, h = h)
  
  mapeArima010[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeArima010 <- colMeans(mapeArima010)
mapeArima010

# Predicción
parima010 <- forecast(arima010, 
                      h = 5, 
                      level = 95)

parima010

autoplot(parima010, 
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 


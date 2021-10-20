#----------------------------------------------------------
# CODIGO TEMA 7
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(aod)
library(seasonal)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  freq = 12)

nacimientos <- window(nacimientos, 
                      start = 2000)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos en España")

# Chocolate
chocolate <- read.csv2("./series/Chocolate.csv", 
                       header = TRUE)

chocolate <- ts(chocolate, 
                start = 1958, 
                freq = 12)

autoplot(chocolate, 
         xlab = "", 
         ylab = "Toneladas",
         main = "Producción de Chocolate (Australia)")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Nacimientos
#----------------------------------------------------------
# Identificación, Intervencion y Estimación
monthdays(nacimientos)
easter(nacimientos)

DiasMes <- monthdays(nacimientos)
SemanaSanta <- easter(nacimientos)
d0111 <- 1*(cycle(nacimientos) == 1 & trunc(time(nacimientos)) == 2011)

auto.arima(nacimientos, 
           d = 1, 
           D = 1, 
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta, d0111))

summary(seas(nacimientos))


d1210 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2010)

nac.ar1 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 2),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, d1210,  d0111))
nac.ar1

error <- residuals(nac.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2018, 2)) 

d1206 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2006)
d0416 <- 1*(cycle(nacimientos) == 4 & trunc(time(nacimientos)) == 2016)
d0616 <- 1*(cycle(nacimientos) == 6 & trunc(time(nacimientos)) == 2016)

nac.ar2 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal =  c(0, 1, 2),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, d1206, d1210, d0111, d0416, d0616))
nac.ar2

error <- residuals(nac.ar2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2018, 2)) 

# Significatividad
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 1)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 2)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 3)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 4)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 5)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 6)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 7)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 8)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 9)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 10)

# Estimación
nac.ar2 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1), 
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, d1206, d1210, d0111, d0416, d0616))
nac.ar2

# Error de ajuste
accuracy(nac.ar2)

# Error de predicción extra-muestral origen de prediccion movil
k <- 120                   
h <- 12                    
T <- length(nacimientos)   
s<-T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasMes, SemanaSanta, d1206, d1210, d0111, d0416, d0616))

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- Arima(train.set, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg=as.matrix(X.train))
  } else {
    fit <- Arima(train.set, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1), 
                 lambda = 0)
  }
  
  if (length(X.train) > 0) fcast <- forecast(fit, h = h, biasadj = TRUE, xreg = as.matrix(X.test)) else
    fcast <- forecast(fit, h = h, biasadj = TRUE)
  
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorArima <- colMeans(mapeArima)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("Error de predicción (MAPE) según horizonte temporal") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

# Predicción
tmp <- ts(rep(0, 48), start = 2019, freq = 12)
pdm <- monthdays(tmp)
pss <- easter(tmp)

pnac.ar2 <- forecast(nac.ar2, 
                     h = 48,
                     biasadj = TRUE,
                     xreg = cbind(pdm, pss, rep(0,48), rep(0,48), rep(0,48), rep(0,48), rep(0,48)), 
                     level = 95)
pnac.ar2

autoplot(pnac.ar2, 
         ylab = 'Nacimientos',
         main = 'Nacimientos (2000-2018) y predicción (2019-2022)') +
  scale_x_continuous(breaks= seq(2000, 2022, 4)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Chocolate
#----------------------------------------------------------
# Transformación
ggAcf(log(chocolate), 
      lag = 48, 
      xlab = "", 
      ylab = "d = D = 0", 
      main = "")
ggAcf(diff(log(chocolate)), 
      lag = 48, 
      xlab = "",
      ylab = "d = 1 - D = 0", 
      main = "")
ggAcf(diff(log(chocolate), lag = 12), 
      lag = 48, 
      xlab = "", 
      ylab = "d = 0 - D = 1",
      main = "")
ggAcf(diff(diff(log(chocolate), lag = 12)), 
      lag = 48, 
      xlab = "", 
      ylab = "d = 1 - D = 1", 
      main = "")

ndiffs(chocolate)
nsdiffs(chocolate)

# Identificación, Intervencion y Estimacion
DiasLaborables <- bizdays(chocolate, FinCenter = "London")
DiasLaborables

auto.arima(chocolate, d = 1, D = 1, 
           lambda = 0,
           xreg = cbind(DiasLaborables))

summary(seas(diff(diff(log(chocolate), lag = 12))))

choco.ar1 <- Arima(chocolate, order=c(1, 1, 1),
                   seasonal = c(0, 1, 1), 
                   lambda = 0,
                   xreg = DiasLaborables)
choco.ar1

error <- residuals(choco.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1958, 1994, 2)) 

d0975 <- 1*(cycle(chocolate) == 9 & trunc(time(chocolate)) == 1975)
d0186 <- 1*(cycle(chocolate) == 1 & trunc(time(chocolate)) == 1986)
d0191 <- 1*(cycle(chocolate) == 1 & trunc(time(chocolate)) == 1991)
d0194 <- 1*(cycle(chocolate) == 1 & trunc(time(chocolate)) == 1994)

choco.ar2 <- Arima(chocolate, order=c(1, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = cbind(DiasLaborables, d0975, d0186, d0191, d0194))
choco.ar2

error <- residuals(choco.ar2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) +
  scale_x_continuous(breaks= seq(1958, 1994, 2)) 

# Significatividad
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 1)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 2)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 3)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 4)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 5)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 6)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 7)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 8)

# Error de ajuste
accuracy(choco.ar2)

# Error de predicción extra-muestral origen de prediccion movil
k <- 240                
h <- 12                 
T <- length(chocolate)  
s<-T - k - h            

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasLaborables, d0975, d0186, d0191, d0194))

for (i in 0:s) {
  train.set <- subset(chocolate, start = i + 1, end = i + k)
  test.set <-  subset(chocolate, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- Arima(train.set, 
                 order = c(1, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg=as.matrix(X.train))
  } else {
    fit <- Arima(train.set, 
                 order = c(1, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0)
  }
  
  if (length(X.train) > 0) fcast <- forecast(fit, h = h, biasadj = TRUE, xreg = as.matrix(X.test)) else
    fcast <- forecast(fit, h = h, biasadj = TRUE)
  
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorArima <- colMeans(mapeArima)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("Error de predicción (MAPE) según horizonte temporal") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

# Prediccion
tmp <- ts(rep(0, 48), start = 1995, frequency = 12)
pdl <- bizdays(tmp, FinCenter = "London")

pchoco.ar2 <- forecast(choco.ar2, 
                       h = 48,
                       xreg = cbind(pdl, rep(0,48), rep(0,48), rep(0,48), rep(0,48)), 
                       level = 95,
                       biasadj = TRUE)
autoplot(pchoco.ar2, 
         xlab = "",
         ylab = 'Toneladas',
         main = 'Chocolate (1958-1994) y predicción (1995-1998)') +
  scale_x_continuous(breaks= seq(1958, 1998, 4)) 


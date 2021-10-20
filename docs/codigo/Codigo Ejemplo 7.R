#----------------------------------------------------------
# CODIGO EJEMPLO 7
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
library(timeDate)
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

autoplot(Pernoctaciones/1000000,
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
ggAcf(log(Pernoctaciones), 
      lag = 48)

ggAcf(diff(log(Pernoctaciones)), 
      lag = 48)

ggAcf(diff(log(Pernoctaciones), lag = 12), 
      lag = 48)

ggAcf(diff(diff(log(Pernoctaciones), lag=12)), 
      lag = 48)

ndiffs(log(Pernoctaciones))
nsdiffs(log(Pernoctaciones))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Identificacion
#----------------------------------------------------------
# Auto.arima
DiasMes <- monthdays(Pernoctaciones)
SemanaSanta <- easter(Pernoctaciones)

auto.arima(Pernoctaciones, d = 1, D = 1,
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta))

# Seas
summary(seas(Pernoctaciones))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Estimacion (Ajuste + Intervencion)
#----------------------------------------------------------
# Semana Santa
LunSanto <- Easter(2000:2022, shift = -6)
MarSanto <- Easter(1996:2024, shift = -5)
MieSanto <- Easter(1996:2024, shift = -4)
JueSanto <- Easter(1996:2024, shift = -3)
VieSanto <- Easter(1996:2024, shift = -2)
SabSanto <- Easter(1996:2024, shift = -1)
DomSanto <- Easter(1996:2024, shift =  0)
LunPascu <- Easter(1996:2024, shift =  1)
MarPascu <- Easter(1996:2024, shift =  2)
MiePascu <- Easter(1996:2024, shift =  3)
JuePascu <- Easter(1996:2024, shift =  4)
ViePascu <- Easter(1996:2024, shift =  5)


SemanaSanta <- c(LunSanto, MarSanto, MieSanto, JueSanto, VieSanto,
                 SabSanto, DomSanto, LunPascu ,MarPascu, MiePascu, 
                 JuePascu, ViePascu)
fechaDiaria <- timeSequence(from = "2000-01-01", to = "2022-12-31")
biz <- fechaDiaria[isBizday(fechaDiaria, holidays = SemanaSanta, wday = 0:6)]
bizdays <- format(biz, format = "%Y-%m")

SemanaSanta <- table(bizdays)
SemanaSanta <- ts(SemanaSanta, start = 2000, frequency = 12)
SemanaSanta
SemanaSanta <- (monthdays(SemanaSanta) - SemanaSanta)/12 #Nuestra SS tiene 12 dias
round(SemanaSanta, 2)

pSemanaSanta <- subset(SemanaSanta, start = length(SemanaSanta) - 35)
SemanaSanta <- subset(SemanaSanta, end = length(SemanaSanta) - 36)

# Ajuste
Arima1 <- Arima(Pernoctaciones, 
                order = c(1, 1, 1),  
                seasonal = c(0, 1, 1),
                lambda = 0,
                xreg = SemanaSanta)
Arima1

#Intervencion
error <- residuals(Arima1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

d0411 <- 1*(cycle(Pernoctaciones) == 4 & trunc(time(Pernoctaciones)) == 2011)
d0513 <- 1*(cycle(Pernoctaciones) == 5 & trunc(time(Pernoctaciones)) == 2013)

#Ajuste
Arima2 <- Arima(Pernoctaciones, 
                order = c(1, 1, 1),  
                seasonal = c(0, 1, 1),
                lambda = 0,
                xreg = cbind(SemanaSanta, d0411, d0513))
Arima2

#Intervencion
error <- residuals(Arima2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Validacion
#----------------------------------------------------------
# Coeficientes significativos
wald.test(b = coef(Arima2), Sigma = vcov(Arima2), Terms = 1)
wald.test(b = coef(Arima2), Sigma = vcov(Arima2), Terms = 2)
wald.test(b = coef(Arima2), Sigma = vcov(Arima2), Terms = 3)
wald.test(b = coef(Arima2), Sigma = vcov(Arima2), Terms = 4)
wald.test(b = coef(Arima2), Sigma = vcov(Arima2), Terms = 5)
wald.test(b = coef(Arima2), Sigma = vcov(Arima2), Terms = 6)

# Error de ajuste
accuracy(Arima2)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Prediccion
#----------------------------------------------------------
pArima2 <- forecast(Arima2, 
                    h = 36,
                    xreg = cbind(pSemanaSanta, rep(0, 36), rep(0, 36)), 
                    level = 95)
autoplot(pArima2, 
         xlab = "",
         ylab = "Defunciones",
         main = "Pernoctaciones (2000-2019) y predicción (2000-2022)") +
  scale_x_continuous(breaks= seq(2000, 2022, 4)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Comparacion entre modelos
#----------------------------------------------------------
# Error de ajuste
Arima22 <- Arima(Pernoctaciones, 
                 order = c(1, 1, 1),  
                 seasonal = list(order = c(0, 1, 1), period = 12),
                 xreg = cbind(SemanaSanta, d0411, d0513))

accuracy(snaive(Pernoctaciones))
accuracy(snaive(Pernoctaciones, lambda = 0))
accuracy(ets(Pernoctaciones, model = "MAM", damped = TRUE))
accuracy(ets(Pernoctaciones, model = "AAA", damped = TRUE, lambda = 0))
accuracy(Arima22)
accuracy(Arima2)

# Errores con origen de predicción movil
k <- 120                   
h <- 12                    
T <- length(Pernoctaciones)     
s <- T - k - h               

mapeIngenuo <- matrix(NA, s + 1, h)
mapeAlisado <- matrix(NA, s + 1, h)
mapeAlisadoLog <- matrix(NA, s + 1, h)
mapeArima <- matrix(NA, s + 1, h)
mapeArimaLog <- matrix(NA, s + 1, h)


X <- data.frame(cbind(SemanaSanta, d0411, d0513))

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  #Ingenuo
  fit <- snaive(train.set, h = h)
  mapeIngenuo[i + 1,] <- 100*abs(test.set - fit$mean)/test.set
  
  #Alisado sin log
  fit <- ets(train.set, model = "MAM", damped = TRUE)
  fcast <- forecast(fit, h = h) 
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  #Alisado con log
  fit <- ets(train.set, model = "AAA", damped = TRUE, lambda = 0)
  fcast <- forecast(fit, h = h) 
  mapeAlisadoLog[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  #Arima sin log
  fit <- try(Arima(train.set, 
                   order = c(1, 1, 1),
                   seasonal = c(0, 1, 1),
                   xreg = as.matrix(X.train)), silent = TRUE)
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test))
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
  
  #Arima con log
  fit <- try(Arima(train.set, 
                   order = c(1, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = as.matrix(X.train)), silent = TRUE)
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test), biasadj = TRUE) 
    mapeArimaLog[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
  
}

mapeIngenuo <- colMeans(mapeIngenuo)
mapeAlisado <- colMeans(mapeAlisado)
mapeAlisadoLog <- colMeans(mapeAlisadoLog)
mapeArima <- colMeans(mapeArima, na.rm = TRUE)
mapeArimaLog <- colMeans(mapeArimaLog, na.rm = TRUE)

ggplot() +
  geom_line(aes(x = 1:12, y = mapeIngenuo, colour = "Ingenuo")) +
  geom_line(aes(x = 1:12, y = mapeAlisado, colour = "Alisado")) + 
  geom_line(aes(x = 1:12, y = mapeAlisadoLog, colour = "Alisado (log)")) +
  geom_line(aes(x = 1:12, y = mapeArima, colour = "Arima")) +
  geom_line(aes(x = 1:12, y = mapeArimaLog, colour = "Arima (log)")) +
  ggtitle("Errores de previsión extra-muestral. Varios modelos") +
  xlab("") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12) +
  scale_color_discrete(name = "Modelos")


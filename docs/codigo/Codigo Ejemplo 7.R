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
library(lmtest)
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
                     frequency = 12)

autoplot(Pernoctaciones,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2020, 2))

Pernoctacionesb <- window(Pernoctaciones, end = c(2019, 12))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Transformacion
#----------------------------------------------------------
ggAcf(log(Pernoctacionesb), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(Pernoctacionesb)), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(Pernoctacionesb), lag = 12), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(diff(log(Pernoctacionesb), lag=12)), lag = 48, xlab = "", ylab = "", main = "")

ndiffs(log(Pernoctacionesb))
nsdiffs(log(Pernoctacionesb))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Identificacion
#----------------------------------------------------------
# Auto.arima
DiasMes <- monthdays(Pernoctacionesb)
SemanaSanta <- easter(Pernoctacionesb)

auto.arima(Pernoctacionesb, 
           d = 1, 
           D = 1,
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta))

# Seas
summary(seas(Pernoctacionesb))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Estimacion (Ajuste + Intervencion)
#----------------------------------------------------------
# Semana Santa
LunSanto <- Easter(2000:2022, shift = -6)
MarSanto <- Easter(2000:2022, shift = -5)
MieSanto <- Easter(2000:2022, shift = -4)
JueSanto <- Easter(2000:2022, shift = -3)
VieSanto <- Easter(2000:2022, shift = -2)
SabSanto <- Easter(2000:2022, shift = -1)
DomSanto <- Easter(2000:2022, shift =  0)
LunPascu <- Easter(2000:2022, shift =  1)
MarPascu <- Easter(2000:2022, shift =  2)
MiePascu <- Easter(2000:2022, shift =  3)
JuePascu <- Easter(2000:2022, shift =  4)
ViePascu <- Easter(2000:2022, shift =  5)


SemanaSanta <- c(LunSanto, MarSanto, MieSanto, JueSanto, VieSanto,
                 SabSanto, DomSanto, LunPascu ,MarPascu, MiePascu, 
                 JuePascu, ViePascu)
fechaDiaria <- timeSequence(from = "2000-01-01", to = "2022-12-31")
biz <- fechaDiaria[isBizday(fechaDiaria, holidays = SemanaSanta, wday = 0:6)]
bizdays <- format(biz, format = "%Y-%m")

SemanaSanta <- table(bizdays)
SemanaSanta <- ts(SemanaSanta, start = 2000, frequency = 12)
SemanaSanta

SemanaSanta <- (monthdays(SemanaSanta) - SemanaSanta)/12 
round(SemanaSanta, 2)

pSemanaSanta <- subset(SemanaSanta, start = length(SemanaSanta) - 35)
SemanaSanta <- subset(SemanaSanta, end = length(SemanaSanta) - 36)

# Ajuste
DiasMes <- monthdays(Pernoctacionesb)

Arima1 <- Arima(Pernoctacionesb, 
                order = c(0, 1, 1),  
                seasonal = c(0, 1, 1),
                lambda = 0,
                xreg = cbind(DiasMes, SemanaSanta))
Arima1

#Intervencion
error <- residuals(Arima1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2020, 2))  

d0305 <- 1*(cycle(Pernoctacionesb) == 3 & trunc(time(Pernoctacionesb)) == 2005)
d0406 <- 1*(cycle(Pernoctacionesb) == 4 & trunc(time(Pernoctacionesb)) == 2006)
d0513 <- 1*(cycle(Pernoctacionesb) == 5 & trunc(time(Pernoctacionesb)) == 2013)

#Ajuste
Arima2 <- Arima(Pernoctacionesb, 
                order = c(0, 1, 1),  
                seasonal = c(0, 1, 1),
                lambda = 0,
                xreg = cbind(DiasMes, SemanaSanta, d0305, d0406, d0513))
Arima2

#Intervencion
error <- residuals(Arima2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
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
coeftest(Arima2)

# Error de ajuste
accuracy(Arima2)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Prediccion
#----------------------------------------------------------
tmp <- ts(rep(0, 36), start = 2020, frequency = 12)
pDiasMes <- monthdays(tmp)

pArima2 <- forecast(Arima2, 
                    h = 36,
                    xreg = cbind(pDiasMes, pSemanaSanta, 
                                 rep(0, 36), rep(0, 36), rep(0, 36)), 
                    level = 95)
autoplot(pArima2, 
         xlab = "",
         ylab = "Pernoctaciones",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 


aggregate(Pernoctaciones - pArima2$mean, FUN = sum) / 1000000
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Comparacion entre modelos
#----------------------------------------------------------
# Error de ajuste
modelSen <- snaive(Pernoctacionesb)
modelSenL <- snaive(Pernoctacionesb, lambda = 0)
modelAli <- ets(Pernoctacionesb, model = "MNM")
modelAliL <- ets(Pernoctacionesb, model = "ANA", lambda = 0)
Arima22 <- Arima(Pernoctacionesb, 
                 order = c(0, 1, 1),  
                 seasonal = c(0, 1, 1),
                 xreg = cbind(DiasMes, SemanaSanta, d0305, d0406, d0513))
accuracy(modelSen)
accuracy(modelSenL)
accuracy(modelAli)
accuracy(modelAliL)
accuracy(Arima22)
accuracy(Arima2)

# Errores con origen de predicción movil
k <- 120                   
h <- 12                    
T <- length(Pernoctacionesb)     
s <- T - k - h               

mapeIngenuo <- matrix(NA, s + 1, h)
mapeAlisado <- matrix(NA, s + 1, h)
mapeAlisadoLog <- matrix(NA, s + 1, h)
mapeArima <- matrix(NA, s + 1, h)
mapeArimaLog <- matrix(NA, s + 1, h)


X <- data.frame(cbind(DiasMes, SemanaSanta, d0305, d0406, d0513))

for (i in 0:s) {
  train.set <- subset(Pernoctacionesb, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctacionesb, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  #Ingenuo
  fit <- snaive(train.set, h = h)
  mapeIngenuo[i + 1,] <- 100*abs(test.set - fit$mean)/test.set
  
  #Alisado sin log
  fit <- ets(train.set, model = "MNM")
  fcast <- forecast(fit, h = h) 
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  #Alisado con log
  fit <- ets(train.set, model = "ANA", lambda = 0)
  fcast <- forecast(fit, h = h) 
  mapeAlisadoLog[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  #Arima sin log
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   xreg = as.matrix(X.train)), silent = TRUE)
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test))
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
  
  #Arima con log
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = as.matrix(X.train)), silent = TRUE)
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) 
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
  ggtitle("") +
  xlab("") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12) +
  scale_color_discrete(name = "Modelos")


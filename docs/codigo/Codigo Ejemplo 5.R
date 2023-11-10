#----------------------------------------------------------
# CODIGO EJEMPLO 5
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
# Importamos Pernoctaciones
#----------------------------------------------------------
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[, 2], 
                     start = 2000, 
                     frequency = 12)

autoplot(Pernoctaciones/1000000,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial para la serie anual
#----------------------------------------------------------
# Serie anual
PernoctacionesAnual <- aggregate(Pernoctaciones/1000000, FUN = sum)

autoplot(PernoctacionesAnual,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 

# Ajuste
PernoctacionesAnualb <- window(PernoctacionesAnual, end = 2019)
PernoctacionesAnualEts <- ets(PernoctacionesAnualb)
summary(PernoctacionesAnualEts) 

# Prediccion
forecast(PernoctacionesAnualEts, 
         h = 5, 
         level = 95)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial para la serie mensual
#----------------------------------------------------------
# Ajuste
Pernoctacionesb <- window(Pernoctaciones, end = c(2019, 12))
PernoctacionesEts <- ets(Pernoctacionesb)
summary(PernoctacionesEts) 

autoplot(PernoctacionesEts,
         xlab = "",
         main = "")

# Ultimos valores
TT <- nrow(PernoctacionesEts$states)
PernoctacionesEts$states[TT,]

# Estacionalidad
componenteEstacional <- PernoctacionesEts$states[TT, 13:2]

ggplot() +
  geom_line(aes(x = 1:12, y = componenteEstacional)) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 

# Predicción
PernoctacionesEtsPre <- forecast(PernoctacionesEts, 
                                 h = 36, 
                                 level = 95)

PernoctacionesEtsPre

autoplot(PernoctacionesEtsPre,
         xlab = "",
         ylab = "Casos",
         main = "",
         PI = FALSE)

# Efecto Covid
aggregate(Pernoctaciones - PernoctacionesEtsPre$mean, FUN = sum)/1000000

# Análisis del error
error <- residuals(PernoctacionesEts)
sderror <- sd(error)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "",
         colour = "black") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2020, 2))  

time(error)[abs(error) > 3 * sderror]

# Prueba de Tukey 
atipicos <- tsoutliers(error)
time(error)[atipicos$index]

# Error por origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(Pernoctacionesb)
s <- TT - k - h          

mapeAlisado <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(Pernoctacionesb, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctacionesb, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MNM")
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
  ylim(0, 6) + 
  scale_x_continuous(breaks= 1:12)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Modelos alternativos
#----------------------------------------------------------
k <- 120                 
h <- 12                  
TT <- length(Pernoctacionesb)
s <- TT - k - h

mapeAlisado1 <- matrix(NA, s + 1, h)
mapeAlisado2 <- matrix(NA, s + 1, h)
mapeAlisado3 <- matrix(NA, s + 1, h)
mapeAlisado4 <- matrix(NA, s + 1, h)
mapeAlisado5 <- matrix(NA, s + 1, h)
mapeAlisado6 <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(Pernoctacionesb, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctacionesb, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MNM")
  fcast<-forecast(fit, h = h)
  mapeAlisado1[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "MNM", opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h)
  mapeAlisado2[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "ANA", lambda = 0, damped = FALSE)
  fcast<-forecast(fit, h = h, biasadj = TRUE)
  mapeAlisado3[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "ANA", lambda = 0, opt.crit = "amse", nmse = 2, damped = FALSE)
  fcast<-forecast(fit, h = h, biasadj = TRUE)
  mapeAlisado4[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set/monthdays(train.set), model = "MNM")
  fcast<-forecast(fit, h = h)
  mapeAlisado5[i + 1,] <- 100*abs(test.set - fcast$mean * monthdays(fcast$mean))/test.set
  
  fit <- ets(train.set/monthdays(train.set), model = "MNM", opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h)
  mapeAlisado6[i + 1,] <- 100*abs(test.set - fcast$mean * monthdays(fcast$mean))/test.set
}

errorAlisado1 <- colMeans(mapeAlisado1)
errorAlisado2 <- colMeans(mapeAlisado2)
errorAlisado3 <- colMeans(mapeAlisado3)
errorAlisado4 <- colMeans(mapeAlisado4)
errorAlisado5 <- colMeans(mapeAlisado5)
errorAlisado6 <- colMeans(mapeAlisado6)

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado1, colour = "Modelo 1")) +
  geom_line(aes(x = 1:12, y = errorAlisado2, colour = "Modelo 2")) + 
  geom_line(aes(x = 1:12, y = errorAlisado3, colour = "Modelo 3")) +
  geom_line(aes(x = 1:12, y = errorAlisado4, colour = "Modelo 4")) +
  geom_line(aes(x = 1:12, y = errorAlisado5, colour = "Modelo 5")) +
  geom_line(aes(x = 1:12, y = errorAlisado6, colour = "Modelo 6")) +
  ggtitle("") +
  xlab("") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12) +
  scale_color_discrete(name = "Modelos")


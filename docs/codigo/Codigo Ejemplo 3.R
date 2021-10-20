#----------------------------------------------------------
# CODIGO EJEMPLO 3
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

Pernoctaciones <- ts(Pernoctaciones[,2], 
                     start = 2000, 
                     freq = 12)

autoplot(Pernoctaciones/1000000,
         xlab = "",
         ylab = "Noches (millones)",
         main = "Pernoctaciones") +
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Metodo ingenuo para la serie anual
#----------------------------------------------------------
PernoctacionesAnual = aggregate(Pernoctaciones/1000000, 
                                FUN = sum)

autoplot(PernoctacionesAnual,
         xlab = "",
         ylab = "Noches (millones)",
         main = "Pernoctaciones") +
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

# Ajustes
mediaPernoctaciones <- meanf(PernoctacionesAnual, h = 5)
naivePernoctaciones <- naive(PernoctacionesAnual, h = 5)
derivaPernoctaciones <- rwf(PernoctacionesAnual,  h = 5, drift = TRUE)

autoplot(PernoctacionesAnual, series = "Pernoctaciones",
         xlab = "",
         ylab = "Noches (millones)",
         main = "Pernoctaciones y predicción por métodos sencillos") +
  autolayer(mediaPernoctaciones, series="Media", PI = FALSE) +
  autolayer(naivePernoctaciones, series="Ingenuo", PI = FALSE) +
  autolayer(derivaPernoctaciones, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Pernoctaciones", "Media", "Ingenuo", "Deriva")) +
  guides(colour = guide_legend(title = "Métodos")) + 
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1))

# Error de ajuste
accuracy(mediaPernoctaciones)
accuracy(naivePernoctaciones)
accuracy(derivaPernoctaciones)

# Error con origen de predicciones movil
k <- 10                  
h <- 5                   
TT <- length(PernoctacionesAnual) 
s <- TT - k - h          

mapeNaiveI <- matrix(NA, s + 1, h)
mapeDeriva <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(PernoctacionesAnual, start = i + 1, end = i + k)
  test.set <-  subset(PernoctacionesAnual, start = i + k + 1, end = i + k + h)
  
  fcast <- naive(train.set, h = h)
  mapeNaiveI[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fcast <- rwf(train.set, h = h,  drift = TRUE)
  mapeDeriva[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeNaiveI <- colMeans(mapeNaiveI)
mapeDeriva <- colMeans(mapeDeriva)

mapeNaiveI
mapeDeriva

# Predicciones
naivePernoctaciones
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Metodo ingenuo con estacionalidad
#----------------------------------------------------------
# Ajuste
PernoctacionesPre <- snaive(Pernoctaciones, h = 36, level = 0.95)
PernoctacionesPre

# Error de ajuste
accuracy(PernoctacionesPre)

# Prevision
autoplot(PernoctacionesPre,
         xlab = "",
         ylab = "Noches",
         main = "Pernoctaciones (2000-2019) y predicción (2020-2022)",
         PI = FALSE)

# Error con origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(Pernoctaciones)
s <- TT - k - h          

mapeSnaive <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h)
  
  fit <- snaive(train.set, h = h)
  mapeSnaive[i + 1,] <- 100*abs(test.set - fit$mean)/test.set
}

mapeSnaive <- colMeans(mapeSnaive)
mapeSnaive

ggplot() +
  geom_line(aes(x = 1:12, y = mapeSnaive)) +
  ggtitle("Error de predicción según horizonte temporal") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  ylim(0, 5) + 
  scale_x_continuous(breaks= 1:12)


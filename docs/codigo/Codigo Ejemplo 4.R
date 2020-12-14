#----------------------------------------------------------
# CODIGO EJEMPLO 4
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
# 
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[,2], 
                     start = 2000, 
                     freq = 12)

Pernoctaciones <- aggregate(Pernoctaciones, FUN = sum)

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
# Doble media movil
#----------------------------------------------------------
# Funcion
dmm <- function(x, r = 3, h = 5) {
  z <- NULL
  z$x <- x
  z$orden = r
  
  TT <- length(x)
  inicio <- start(x)
  frecuencia <-frequency(x)
  
  z$mm1 <- filter(x, rep(1/r, r), side = 1)
  z$mm2 <- filter(z$mm1, rep(1/r, r), side = 1)
  
  z$l <- 2 * z$mm1 - z$mm2
  z$b <- 2 * (z$mm1 - z$mm2) / (r - 1)
  
  z$fitted <- ts(c(NA, z$l[-TT] + z$b[-TT]), start = inicio, freq = frecuencia)
  z$mean <- ts(z$l[TT] + (1:h) * z$b[TT], start = time(x)[TT] + 1/frecuencia, freq = frecuencia)
  z$residuals <- x - z$fitted
  class(z) <- "forecast"
  z
}

# Ajuste
PernoctacionesPre <- dmm(Pernoctaciones, 
                         r = 4, 
                         h = 5)

# Prediccion
autoplot(Pernoctaciones, series = "Pernoctaciones",
         xlab = "",
         ylab = "Noches",
         main = "Pernoctaciones y predicción") +
  autolayer(PernoctacionesPre$fitted, series = "Pred. intra") + 
  autolayer(PernoctacionesPre$mean, series = "Pred. extra")

# Error de ajuste
accuracy(PernoctacionesPre)

# Error con origen de prediccion movil
k <- 10                   
h <- 5                    
TT <- length(Pernoctaciones) 
s <- TT - k - h           

MAPE <- matrix(NA, nrow = 4, ncol = 5)
rownames(MAPE) <- 2:5
colnames(MAPE) <- 1:h

for (r in 2:5) {
  
  tmpMape <- matrix(NA, s + 1, h)
  
  for (i in 0:s) {
    
    train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
    test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h)
    
    fit <- dmm(train.set, r = r, h = 5)
    tmpMape[i + 1, ] <- 100*abs(test.set - fit$mean)/test.set
  }
  MAPE[r - 1, ] <- colMeans(tmpMape)
}
MAPE

# El mejor modelo
PernoctacionesPre <- dmm(Pernoctaciones, 
                         r = 2, 
                         h = 5)

autoplot(Pernoctaciones, series = "Pernoctaciones",
         xlab = "",
         ylab = "Noches",
         main = "Pernoctaciones y predicción") +
  autolayer(PernoctacionesPre$fitted, series = "Pred. intra") + 
  autolayer(PernoctacionesPre$mean, series = "Pred. extra")


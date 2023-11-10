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
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[, 2], 
                     start = 2000, 
                     frequency = 12)

Pernoctaciones <- aggregate(Pernoctaciones/1000000, FUN = sum)

Pernoctacionesb <- window(Pernoctaciones, end = 2019)

autoplot(Pernoctacionesb,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2020, 2))  
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
#  Media movil
#----------------------------------------------------------
# Funcion
mmf <- function(x, r = 3, h = 5) {
  z <- NULL
  z$x <- x
  z$orden = r
  
  TT <- length(x)
  inicio <- start(x)
  frecuencia <-frequency(x)
  
  z$mm <- stats::filter(x, rep(1/r, r), side = 1)
  
  z$fitted <- ts(c(NA, z$mm[-TT]), 
                 start = inicio, 
                 freq = frecuencia)
  
  z$mean <- ts(rep(z$mm[TT], h), 
               start = time(x)[TT] + 1/frecuencia, 
               freq = frecuencia)
  
  z$residuals <- x - z$fitted
  
  class(z) <- "forecast"
  z
}

# Ajuste
mmPernoctaciones <- mmf(Pernoctacionesb, 
                        r = 4, 
                        h = 5)

# Prediccion
autoplot(mmPernoctaciones,
         xlab = "",
         ylab = "Noches",
         main = "") +
  autolayer(mmPernoctaciones$fitted) +
  theme(legend.position="none")

# Error de ajuste
accuracy(mmPernoctaciones)

# Impacto de la Covid-19
Pernoctaciones - mmPernoctaciones$mean

# Error con origen de prediccion movil
k <- 10                   
h <- 5                    
TT <- length(Pernoctacionesb)
s <- TT - k - h           


for (r in 1:5) {
  
  tmpMape <- matrix(NA, s + 1, h)
  
  for (i in 0:s) {
    
    train.set <- subset(Pernoctacionesb, start = i + 1, end = i + k)
    test.set <-  subset(Pernoctacionesb, start = i + k + 1, end = i + k + h)
    
    fit <- mmf(train.set, r = r, h = 5)
    tmpMape[i + 1, ] <- 100*abs(test.set - fit$mean)/test.set
  }
  tmpMape <- colMeans(tmpMape)
  
  cat("\nPara un orden de", 
      r, 
      "los errores son", 
      formatC(tmpMape, format = "f", digits = 2))  
}

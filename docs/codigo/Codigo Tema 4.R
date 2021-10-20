#----------------------------------------------------------
# CODIGO TEMA 4
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
# Importamos series
#----------------------------------------------------------
# Libros
libros <- read.csv2("./series/libros.csv", 
                    header = TRUE)

libros <- ts(libros[, 2], 
             start = 1993, 
             frequency  = 1)

autoplot(libros,
         xlab = "",
         ylab = "Títulos",
         main = "Títulos publicados (libros y folletos)")

#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Media movil simple
#----------------------------------------------------------
# Funcion
mm <- function(x, r = 3, h = 5) {
  z <- NULL
  z$x <- x
  z$orden = r
  
  TT <- length(x)
  inicio <- start(x)
  frecuencia <-frequency(x)
  
  z$mm <- stats::filter(x, rep(1/r, r), side = 1)
  z$fitted <- ts(c(NA, z$mm[-TT]), start = inicio, freq = frecuencia)
  z$mean <- ts(rep(z$mm[TT], h), start = time(x)[TT] + 1/frecuencia, freq = frecuencia)
  z$residuals <- x - z$fitted
  
  class(z) <- "forecast"
  z
}

# Libros
mmLibros <- mm(libros, 
               r = 5, 
               h = 5)

autoplot(libros, series = "Libros",
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con media móvil de orden 5") +
  autolayer(mmLibros$fitted, series = "Pred. intra") + 
  autolayer(mmLibros$mean, series = "Pred. extra")

accuracy(mmLibros)
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
  
  z$mm1 <- stats::filter(x, rep(1/r, r), side = 1)
  z$mm2 <- stats::filter(z$mm1, rep(1/r, r), side = 1)
  
  z$l <- 2 * z$mm1 - z$mm2
  z$b <- 2 * (z$mm1 - z$mm2) / (r - 1)
  
  z$fitted <- ts(c(NA, z$l[-TT] + z$b[-TT]), start = inicio, freq = frecuencia)
  z$mean <- ts(z$l[TT] + (1:h) * z$b[TT], start = time(x)[TT] + 1/frecuencia, freq = frecuencia)
  z$residuals <- x - z$fitted
  class(z) <- "forecast"
  z
}

# Libros
accuracy(dmmLibros2 <- dmm(libros, r = 2))
accuracy(dmmLibros3 <- dmm(libros, r = 3))
accuracy(dmmLibros4 <- dmm(libros, r = 4))

k <- 15              
h <- 3               
TT <- length(libros) 
s <- TT - k - h      

for(r in 2:4){
  
  tmpMape <- matrix(NA, s + 1, h)
  for (i in 0:s) {
    
    train.set <- subset(libros, start = i + 1, end = i + k)
    test.set <-  subset(libros, start = i + k + 1, end = i + k + h)
    
    dmmLibros <- dmm(train.set, r = r, h = h)
    tmpMape[i + 1, ] <- 100*abs(test.set - dmmLibros$mean)/test.set
  }
  tmpMape <- colMeans(tmpMape)
  
  cat("\nPara un orden de", r, "los errores son", formatC(tmpMape, format = "f", digits = 2)) 
  
}

autoplot(dmmLibros3,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con doble media móvil de orden 3") +
  autolayer(dmmLibros3$fitted) +
  theme(legend.position="none")

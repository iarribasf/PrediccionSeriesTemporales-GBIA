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

# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

nacimientos <- aggregate(nacimientos, FUN = sum)

autoplot(nacimientos,
         xlab = "",
         ylab = "Bebés",
         main = "Nacimientos")
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
  
  z$mm <- filter(x, rep(1/r, r), side = 1)
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

# Nacimientos
for (r in 2:10) {
  dmmNacimientos <- dmm(nacimientos, r = r)
  print(c(r, accuracy(dmmNacimientos)[5]))
}


k <- 30                   
h <- 5                    
TT <- length(nacimientos) 
s <- TT - k - h           

MAPE <- matrix(NA, nrow = 9, ncol = 5)
rownames(MAPE) <- 2:10
colnames(MAPE) <- 1:h

for (r in 2:10) {
  
  tmpMape <- matrix(NA, s + 1, h)
  
  for (i in 0:s) {
    
    train.set <- subset(nacimientos, start = i + 1, end = i + k)
    test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h)
    
    dmmNacimientos <- dmm(train.set, r = r, h = h)
    tmpMape[i + 1, ] <- 100*abs(test.set - dmmNacimientos$mean)/test.set
  }
  MAPE[r - 1, ] <- colMeans(tmpMape)
}
MAPE

dmmNacimiemtos <- dmm(nacimientos, 
                      r = 2, 
                      h = 5)

autoplot(nacimientos, series = "Nacimientos",
         xlab = "",
         ylab = "Bebés",
         main = "Nacimientos y predicción con doble media móvil de orden 2") +
  autolayer(dmmNacimiemtos$fitted, series = "Pred. intra") + 
  autolayer(dmmNacimiemtos$mean, series = "Pred. extra")

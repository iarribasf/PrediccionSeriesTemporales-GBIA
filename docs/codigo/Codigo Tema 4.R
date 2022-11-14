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
# Demanda electrica
electricidad <- read.csv2("./series/Consumo electrico.csv", 
                          header = TRUE)

electricidad <- ts(electricidad[, 2],
                   start = c(1, 5),
                   frequency = 7)

electricidad <- aggregate(electricidad, FUN = sum) 

autoplot(electricidad,
         xlab = "",
         ylab = "GWh",
         main = "",
         ylim= c(0, 6000))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Media movil simple
#----------------------------------------------------------
# Funcion
mmf <- function(x, r = 3, h = 5) {
  z <- NULL
  z$x <- x
  z$orden = r
  
  TT <- length(x)
  inicio <- start(x)
  frecuencia <-frequency(x)
  
  z$mm <- stats::filter(x, rep(1/r, r), 
                        side = 1)
  
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

# Libros
mmelectricidad <- mmf(electricidad, 
                      r = 5, 
                      h = 5)

autoplot(mmelectricidad,
         xlab = "",
         ylab = "GWh",
         main = "") +
  autolayer(mmelectricidad$fitted) + 
  theme(legend.position="none")

accuracy(mmelectricidad)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Identificacion del mejor modelo
#----------------------------------------------------------
# Selección usando previsiones intra-muestrales a un periodo vista
for(r in 1:4) {
  error <- accuracy(mmf(electricidad, r = r))[5]
  cat("\nPara un orden de", 
      r, 
      "el error es", 
      formatC(error, format = "f", digits = 2),
      " %")
}

# Selección usando origen de predicción móvil
k <- 20              
h <- 4               
TT <- length(electricidad) 
s <- TT - k - h      

for(r in 1:4){
  
  mapemm <- matrix(NA, s + 1, h)
  for (i in 0:s) {
    
    train.set <- subset(electricidad, start = i + 1, end = i + k)
    test.set <-  subset(electricidad, start = i + k + 1, end = i + k + h)
    
    mmElectricidad <- mmf(train.set, r = r, h = h)
    mapemm[i + 1, ] <- 100*abs(test.set - mmElectricidad$mean)/test.set
  }
  mapemm <- colMeans(mapemm)
  
  cat("\nPara un orden de", 
      r, 
      "los errores son", 
      formatC(mapemm, format = "f", digits = 2)) 
  
}

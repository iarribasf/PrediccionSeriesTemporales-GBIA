#----------------------------------------------------------
# CODIGO TEMA 2
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

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos mensuales")

start(nacimientos)
end(nacimientos)
frequency(nacimientos)
time(nacimientos)
cycle(nacimientos)

# Sucursal
sucursal <- read.table("./series/sucursal.csv", 
                       header = TRUE)

sucursal <- ts(sucursal, 
               start = c(1, 5), 
               freq = 7)

sucursalb <- window(sucursal, start = c(22, 1), end = c(25, 7))

autoplot(sucursalb,
         xlab = "",
         ylab = "Euros",
         main = "Extracción de dinero de un cajero") + 
  scale_x_continuous(breaks= 22:26, labels = 22:26) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Tendencia
#----------------------------------------------------------
# Extracción de la tendencia
nacimientosAnual <- aggregate(nacimientos, FUN = sum)

autoplot(nacimientosAnual/1000,
         xlab = "",
         ylab = "Nacimientos (miles)",
         main = "Nacimientos por año")

sucursalAnual <- aggregate(sucursal,FUN = sum)

autoplot(sucursalAnual,
         xlab = "",
         ylab = "Euros",
         main = "Extracción de dinero de un cajero por semana")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Estacionalidad
#----------------------------------------------------------
# Graficas
nacimientosb <- window(nacimientos, start = 2000)

ggseasonplot(nacimientosb, 
             year.labels=TRUE, 
             year.labels.left=TRUE,
             xlab = "",
             ylab = "Nacimientos",
             main = "Gráfico estacional de lineas")

ggseasonplot(nacimientosb, polar=TRUE,
             xlab = "",
             ylab = "",
             main = "Gráfico estacional polar")

ggsubseriesplot(nacimientosb) +
  ylab("Nacimientos") +
  xlab("") +
  ggtitle("Gráfico estacional de subseries")

# Componente numerica
componenteEstacional <- tapply(nacimientosb/mean(nacimientosb), 
                               cycle(nacimientosb), 
                               FUN = mean)
componenteEstacional
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Descomposición por medias móviles
#----------------------------------------------------------
#Esquema aditivo
nacDesAdi <- decompose(nacimientos, 
                       type = "addi")

autoplot(nacDesAdi,
         xlab = "",
         main = "Descomposición aditiva de Nacimientos por medias móviles")

summary(nacimientos - trendcycle(nacDesAdi) - seasonal(nacDesAdi) - remainder(nacDesAdi))

autoplot(nacimientos, series="Nacimientos",
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos en España: serie y tendencia") +
  autolayer(trendcycle(nacDesAdi), series="Tendencia") +
  scale_colour_manual(values=c("Nacimientos"="black","Tendencia"="red"),
                      breaks=c("Nacimientos","Tendencia"))

nacDesAdi$figure
sum(nacDesAdi$figure)

ggplot() +
  geom_line(aes(x = 1:12, y = nacDesAdi$figure)) + 
  geom_hline(yintercept = 0, colour = "blue", lty = 2) +
  ggtitle("Componente estacional de Nacimientos (esquema aditivo)") +
  xlab("") +
  ylab("Componente estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 


# Esquema multiplicativo
nacDesMul <- decompose(nacimientos, 
                       type = "mult")
autoplot(nacDesMul,
         xlab = "",
         main = "Descomposición multiplicativa de Nacimientos por medias móviles")

nacDesMul$figure
sum(nacDesMul$figure)

ggplot() +
  geom_line(aes(x = 1:12, y = componenteEstacional, colour = "black")) + 
  geom_line(aes(x = 1:12, y = nacDesMul$figure, colour = "red")) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("Componente estacional de Nacimientos") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_color_discrete(name = "Componente estacional", 
                       labels = c("Descriptiva simple", "Medias móviles")) +
  theme(legend.position=c(0.98,0.02), legend.justification=c(1,0))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Descomposición por regresiones locales ponderadas
#----------------------------------------------------------
nacStl <- stl(nacimientos, 
              s.window = "periodic",
              robust = TRUE)

head(nacStl$time.series)

autoplot(nacStl,
         xlab = "",
         main = "Descomposición de Nacimientos por regresores locales ponderados")

head(seasonal(nacStl), 12)
sum(head(seasonal(nacStl), 12))

# Estacionalidad no fija
nacStl17 <- stl(nacimientos, 
                s.window = 17,
                robust = TRUE)

xx <- window(seasonal(nacStl), start = 1995, end = 2005 - 1/12)
yy <- window(seasonal(nacStl17), start = 1995, end = 2005 - 1/12)
autoplot(xx, series="s.window = 'periodic'",
         ylab = "Componente estacional",
         main = "Componente estacional para Nacimientos") +
  autolayer(yy, series="s.window = 17") +
  scale_colour_manual(values=c("s.window = 'periodic'"="black","s.window = 17"="red"))


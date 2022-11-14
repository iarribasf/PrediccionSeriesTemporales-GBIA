#----------------------------------------------------------
# CODIGO EJEMPLO 2
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
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2020, 2))  
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Esquema
#----------------------------------------------------------
CasosAnual <- aggregate(Pernoctaciones, FUN = sum)
DesviacionAnual <- aggregate(Pernoctaciones, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosAnual, y = DesviacionAnual), size = 2) +
  xlab("Pernoctaciones anuales") + 
  ylab("Desviación típica intra-anual") + 
  ggtitle("") 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Componentes
#----------------------------------------------------------
# Tendencia
autoplot(CasosAnual/1000000,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

# Estacionalidad
ggmonthplot(Pernoctaciones, 
            polar=TRUE,
            xlab = "",
            ylab = "",
            main = "") +
  guides(colour=FALSE)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Descomposicion
#----------------------------------------------------------
PernoctacionesDesMul <- decompose(Pernoctaciones, 
                                  type = "mult")

autoplot(PernoctacionesDesMul,
         xlab = "",
         main = "")

PernoctacionesDesMul$figure

PernoctacionesDiaDesMul <- decompose(Pernoctaciones/monthdays(Pernoctaciones), 
                                     type = "mult")

ggplot() +
  geom_line(aes(x = 1:12, y = PernoctacionesDesMul$figure, colour = "black")) + 
  geom_line(aes(x = 1:12, y = PernoctacionesDiaDesMul$figure, colour = "red")) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_color_discrete(name = "Componente estacional", 
                       labels = c("Pernoctaciones", "Pernoctaciones por día")) +
  theme(legend.position=c(0.2,0.8))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Analisis del error
#----------------------------------------------------------
error <- Pernoctaciones - trendcycle(PernoctacionesDesMul) * seasonal(PernoctacionesDesMul)

sderror <- sd(error, na.rm = TRUE)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "",
         colour = "black") +
  geom_hline(yintercept = c(3, 2, -2, -3)*sderror, 
             colour = c("red", "green", "green", "red"),
             lty = 2) + 
  scale_x_continuous(breaks= seq(1980, 2020, 4)) 



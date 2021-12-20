################################
####### SETEO DE CARPETA #######
################################

path = "C:/Users/leand/Google Drive/Maestría Estadística Aplicada/Series de Tiempo/TPs"

setwd(path)

getwd()

dir()


#####################
## CARGAR PAQUETES ##
#####################

suppressPackageStartupMessages({
  library(tseries)
  library(forecast)
  library(ggplot2)
  library(dplyr)
  library(pastecs)  #Estadísticas descriptivas
  library(lessR)    #Histograma
  library(moments)  #Curtosis y asimetría
  library(cowplot)
})


##################
## CARGAR DATOS ##
##################

## ARCHIVO CSV
leche <- read.csv2(file = "produccion_leche_mes.csv",
                        sep = ",", 
                        header = T)

produccion_leche <- as.numeric(as.character(leche$produccion_leche))

## SERIE DE TIEMPO
leche_ts <- ts(produccion_leche,
               start = c(2015,1),
               frequency = 12)

## DATA FRAME PARA GRÁFICOS
leche_desc <- as.data.frame(produccion_leche)

leche_desc <- leche_desc %>%
  mutate(año = as.numeric(format(seq(as.Date('2015-01-01'),
                                     by = "months",
                                     length.out = 69),
                                 '%Y')))


##########################
## ANÁLISIS DESCRIPTIVO ##
##########################

## HISTOGRAMA
hist_leche <- ggplot(leche_desc, aes(x=produccion_leche)) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 breaks=seq(600,1200,by=100)) +
  scale_x_continuous(breaks=seq(600,1200,by=100),
                     lim=c(600,1200)) +
  xlab("") +
  scale_y_continuous(breaks=seq(0,25,by=5),
                     lim=c(0,25)) +
  ylab("Densidad")

## BOXPLOT
boxplot_leche <- ggplot(leche_desc,
       aes(x=produccion_leche)) + 
  geom_boxplot(color = "darkblue",
               fill = "skyblue3") +
  scale_x_continuous(breaks = seq(600,1200,
                                by=100),
                     lim = c(600,1200)) +
  xlab("Producción de leche [millones de L]") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## GRÁFICOS JUNTOS
plot_grid(hist_leche,
          boxplot_leche,
          ncol = 1,
          rel_heights = c(2, 1),
          align = 'v',
          axis = 'lr')

## BOXPLOT POR AÑO
ggplot(leche_desc,
       aes(x=as.factor(año),
           y=produccion_leche,
           fill=as.factor(año))) + 
  geom_boxplot() +
  xlab("Año") +
  scale_y_continuous(lim = c(600,1200)) +
  ylab("Producción de leche [millones de L]") +
  theme(legend.position = "none")

## ESTADÍSTICAS DESCRIPTIVAS
round(stat.desc(leche_ts),2)
quantile(leche_ts, na.rm = T)
round(skewness(produccion_leche),2)
round(kurtosis(produccion_leche),2)
stat.desc(produccion_leche[1:12])
stat.desc(produccion_leche[13:69])


#####################
## SERIE DE TIEMPO ##
#####################

## GRÁFICA DE LA SERIE
autoplot(leche_ts) +
  xlab("Tiempo") +
  ylab("Producción de leche [millones de L]") +
  scale_y_continuous(lim=c(600,1200))

## AUTOCORRELACIONES
ggAcf(leche_ts,
      type="correlation",
      lag.max = 24) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FAC")+
  scale_y_continuous(breaks=seq(-0.8,0.8,by=0.2),
                     lim=c(-0.8,0.8))

ggAcf(leche_ts,
      type = "partial") + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACP") +
  scale_y_continuous(breaks=seq(-0.8,0.8,by=0.2),
                     lim=c(-0.8,0.8))

## DESCOMPOSICIÓN
autoplot(decompose(leche_ts, type = "additive")) +
  ggtitle("") +
  labs(x = "Tiempo")

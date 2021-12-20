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
  library(cowplot)
  library(dplyr)
  library(pastecs)  #Estadísticas descriptivas
  library(lessR)    #Histograma
  library(moments)  #Curtosis y asimetría
  library(nortest)  #Tests de normalidad
})

source(paste0(path,"/Funciones.R"))


##################
## CARGAR DATOS ##
##################

## ARCHIVO RDS
series <- readRDS("series.rds")

## SERIE DE TIEMPO ELEGIDA
serie <- series[[9]]


##########################
## ANÁLISIS DESCRIPTIVO ##
##########################

## HISTOGRAMA
hist_serie <- ggplot(serie,
       aes(x=serie)) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 bins = 12) +
  scale_x_continuous(breaks=seq(-3,3,by=1),
                     lim=c(-3,3)) +
  xlab("") +
  ylab("Densidad")

## BOXPLOT
boxplot_serie <- ggplot(serie,
       aes(x=serie)) + 
  geom_boxplot(color = "darkblue",
               fill = "skyblue3") +
  scale_x_continuous(breaks=seq(-3,3,by=1),
                     lim=c(-3,3)) +
  xlab("Variable de Interés") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## GRÁFICOS JUNTOS
plot_grid(hist_serie,
          boxplot_serie,
          ncol = 1,
          rel_heights = c(2, 1),
          align = 'v',
          axis = 'lr')

## ESTADÍSTICAS DESCRIPTIVAS
round(stat.desc(serie),2)
quantile(serie, na.rm = T)
round(skewness(serie),2)
round(kurtosis(serie),2)

periodos <- stat.desc(serie)$x[1]
variancia <- stat.desc(serie)$x[12]


#####################
## SERIE DE TIEMPO ##
#####################

## GRÁFICA DE LA SERIE
autoplot(serie) +
  xlab("Tiempo") +
  ylab("") +
  scale_y_continuous(breaks=seq(-3,3,by=1),
                     lim=c(-3,3)) +
  geom_point()

## AUTOCOVARIANZAS Y AUTOCORRELACIONES
fas <- acf(x = serie, lag.max = 25,
           type = "covariance",
           plot = F)

fasnumeric <- as.numeric(fas[[1]][2:26])

fasData <- tibble(rezago = c(1:25),
                  y = fasnumeric)

FAS_serie <- ggplot(fasData, aes(x = rezago,
                                 y = fasnumeric,
                                 xend = rezago,
                                 yend = 0)) +
  geom_segment() +
  geom_hline(yintercept= 0,
             col="black") +
  geom_line(y = (variancia*2/sqrt(periodos)),
            linetype = "dashed",
            col = "Blue") +
  geom_line(y = (-variancia*2/sqrt(periodos)),
            linetype = "dashed",
            col = "Blue") +
  scale_x_continuous("",
                     limits = c(1,25)) +
  ylab("FASM") +
  scale_y_continuous(breaks = seq(-0.40,0.40,
                                  by = 0.1),
                     limits = c(-0.40,0.40))

FAC_serie <- ggAcf(serie,
                   type = "correlation",
                   lag.max = 25) +
  ggtitle("") +
  labs(x = "",
       y = "FACM") +
  scale_y_continuous(breaks = seq(-0.4,0.4,
                                by = 0.1),
                     lim = c(-0.4,0.4))

FACP_serie <- ggAcf(serie,
                   type = "partial",
                   lag.max = 25) + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACPM") +
  scale_y_continuous(breaks = seq(-0.4,0.4,
                                  by = 0.1),
                     lim = c(-0.4,0.4))

plot_grid(FAS_serie,
          FAC_serie,
          FACP_serie,
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'v',
          axis = 'lr')

## PRUEBAS DE LJUNG-BOX
Incorrelation(serie,
              "Ljung-Box")


#############
## MODELOS ##
#############

## MODELO 1: ARMA(1,1)
modelo_1 <- arima(serie,
                  order = c(1, 0, 1),
                  fixed = c(NA, NA, 0),
                  method = "ML")

summary(modelo_1)

pnorm(modelo_1[["coef"]][["ar1"]]/modelo_1[["var.coef"]][1,1],
      mean = 0,
      sd = 1)

pnorm(modelo_1[["coef"]][["ma1"]]/modelo_1[["var.coef"]][2,2],
      mean = 0,
      sd = 1)

ggAcf(resid(modelo_1),
      type = "correlation",
      lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM")

Incorrelation(resid(modelo_1),
              type = "Ljung-Box")

Normality_Test(resid(modelo_1),
               type = "SW")

autoplot(resid(modelo_1)) +
  xlab("Tiempo") +
  ylab("") +
  scale_y_continuous(breaks=seq(-3,3,by=1),
                     lim=c(-3,3)) +
  geom_point()

## MODELO 2: ARMA(14,0) CON REZAGOS DE ORDEN 2, 13 y 14
modelo_2 <- arima(serie,
                  order = c(14, 0, 0),
                  fixed = c(0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, 0),
                  method = "ML")

summary(modelo_2)

pnorm(-0.1648/0.0916,
      mean = 0,
      sd = 1)

pnorm(-0.1498/0.1028,
      mean = 0,
      sd = 1)

## MODELO 3: ARMA(14,0) SOLO CON REZAGOS DE ORDEN 14
modelo_3 <- arima(serie,
                  order = c(14, 0, 0),
                  fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0),
                  method = "ML")

summary(modelo_3)

pnorm(0.3290/0.1029,
      mean = 0,
      sd = 1)

ggAcf(resid(modelo_3),
      type = "correlation",
      lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM")

Incorrelation(resid(modelo_3),
              type = "Ljung-Box")

Normality_Test(resid(modelo_3),
               type = "SW")

autoplot(resid(modelo_3)) +
  xlab("Tiempo") +
  ylab("") +
  scale_y_continuous(breaks=seq(-3,3,by=1),
                     lim=c(-3,3)) +
  geom_point()

## MODELO 4: ARMA(0,14) CON REZAGOS DE ORDEN 8 y 14
modelo_4 <- arima(serie,
                  order = c(0, 0, 14),
                  fixed = c(0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0),
                  method = "ML")

summary(modelo_4)

pnorm(0.1810/0.0965,
      mean = 0,
      sd = 1)

## MODELO 5: ARMA(0,14) SOLO CON REZAGOS DE ORDEN 14
modelo_5 <- arima(serie,
                  order = c(0, 0, 14),
                  fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0),
                  method = "ML")

summary(modelo_5)

pnorm(0.2337/0.0880,
      mean = 0,
      sd = 1)

ggAcf(resid(modelo_5),
      type = "correlation",
      lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM")

Incorrelation(resid(modelo_5),
              type = "Ljung-Box")

Normality_Test(resid(modelo_5),
               type = "SW")

autoplot(resid(modelo_5)) +
  xlab("Tiempo") +
  ylab("") +
  scale_y_continuous(breaks=seq(-3,3,by=1),
                     lim=c(-3,3)) +
  geom_point()

## MODELO 6: ARMA(14,14)
modelo_6 <- arima(serie,
                  order = c(14, 0, 14),
                  fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0),
                  method = "ML")

summary(modelo_6)

pnorm(-0.1731/0.2223,
      mean = 0,
      sd = 1)

## AUTOMÁTICO
auto.arima(y = serie)

## COMPARACIÓN DE MODELOS
AIC(modelo_1,
    modelo_3,
    modelo_5)

BIC(modelo_1,
    modelo_3,
    modelo_5)

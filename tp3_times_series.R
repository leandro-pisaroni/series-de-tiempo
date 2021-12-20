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

data(AirPassengers)
serie <- AirPassengers


##########################
## ANÁLISIS DESCRIPTIVO ##
##########################

## HISTOGRAMA
hist_serie <- ggplot(serie,
                        aes(x=serie)) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 breaks = seq(100,640,
                              by=60)) +
  scale_x_continuous(breaks = seq(100,640,
                                  by=60),
                     lim = c(0,700)) +
  xlab("") +
  ylab("Densidad")

## BOXPLOT
boxplot_serie <- ggplot(serie,
                           aes(x=serie)) + 
  geom_boxplot(color = "darkblue",
               fill = "skyblue3") +
  scale_x_continuous(breaks = seq(100,640,
                                  by=60),
                     lim = c(0,700)) +
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
round(stat.desc(serie),3)
quantile(serie, na.rm = T)
round(skewness(serie),3)
round(kurtosis(serie),3)

## GRÁFICO DE LA SERIE
autoplot(serie) +
  xlab("Tiempo") +
  ylab("Nº de Pasajeros Aéreos") +
  scale_y_continuous(breaks=seq(0,600,by=100),
                     lim=c(50,640)) +
  geom_point()


###############################
## CONJUNTO DE ENTRENAMIENTO ##
###############################

## DEFINICIÓN DEL CONJUNTO DE ENTRENAMIENTO
training <- window(serie,
                   start = c(1949,1), 
                   end = c(1959,12))

## DEFINICIÓN DEL CONJUNTO DE PRUEBA
testing <- window(serie, 
                  start = 1960)

## HISTOGRAMA
hist_training <- ggplot(training,
                     aes(x=training)) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 breaks = seq(100,640,
                              by=60)) +
  scale_x_continuous(breaks = seq(100,640,
                                  by=60),
                     lim = c(0,700)) +
  xlab("") +
  ylab("Densidad")

## BOXPLOT
boxplot_training <- ggplot(training,
                        aes(x=training)) + 
  geom_boxplot(color = "darkblue",
               fill = "skyblue3") +
  scale_x_continuous(breaks = seq(100,640,
                                  by=60),
                     lim = c(0,700)) +
  xlab("Variable de Interés") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## GRÁFICOS JUNTOS
plot_grid(hist_training,
          boxplot_training,
          ncol = 1,
          rel_heights = c(2, 1),
          align = 'v',
          axis = 'lr')

## ESTADÍSTICAS DESCRIPTIVAS
round(stat.desc(training),3)
quantile(training, na.rm = T)
round(skewness(training),3)
round(kurtosis(training),3)

## BOXPLOT POR MES (COMPORTAMIENTO ESTACIONAL)
boxplot(split(training, cycle(training)), 
        names = month.abb, 
        col = "skyblue3",
        ylab = "Nº de Pasajeros Aéreos")

## AUTOCORRELACIONES
FAC_training <- ggAcf(training,
                   type = "correlation",
                   lag.max = 25) +
  ggtitle("") +
  labs(x = "",
       y = "FACM") +
  scale_y_continuous(breaks = seq(0,1,
                                  by = 0.2),
                     lim = c(0,1))

FACP_training <- ggAcf(training,
                    type = "partial",
                    lag.max = 25) + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACPM")

plot_grid(FAC_training,
          FACP_training,
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'v',
          axis = 'lr')

## DESCOMPOSICIÓN
serie_descomposicion <- decompose(serie,
                                  type = "multiplicative")
autoplot(serie_descomposicion) +
  ggtitle("") +
  labs(x="Tiempo")


#####################
## MODELOS SIMPLES ##
#####################

## MODELO INGENUO
ingenuo <- naive(training,
                 h = 12)

summary(ingenuo)

auto_ingenuo <- autoplot(resid(ingenuo)) +
  xlab("Tiempo") +
  ylab("Residuos") +
  scale_y_continuous(breaks=seq(-100,100,by=50),
                     lim=c(-110,110)) +
  geom_point()+
  theme(plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"))

FAC_ingenuo <- ggAcf(resid(ingenuo),
      type = "correlation",
      lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0.5), "cm"))

hist_ingenuo <- ggplot(resid(ingenuo),
       aes(x=resid(ingenuo))) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 bins = 15) +
      xlab("Residuos") +
    scale_x_continuous(lim=c(-120,120)) +
  ylab("Densidad") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0), #arriba, derecha, abajo, izquierda
                           "cm"))

plot_grid(auto_ingenuo,
          plot_grid(FAC_ingenuo,
                    hist_ingenuo,
                    align = 'hv'),
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'h')

Normality_Test(resid(ingenuo),
               type = "SW")

Incorrelation(resid(ingenuo),
              type = "Ljung-Box")

accuracy(ingenuo, testing)

checkresiduals(ingenuo)

## MODELO INGENUO ESTACIONAL
ingenuo_estacional <- snaive(training,
                             h = 12)

summary(ingenuo_estacional)

auto_ingenuo_estacional <- autoplot(resid(ingenuo_estacional)) +
  xlab("Tiempo") +
  ylab("Residuos") +
  scale_y_continuous(breaks=seq(-25,75,by=25),
                     lim=c(-25,75)) +
  geom_point()+
  theme(plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"))

FAC_ingenuo_estacional <- ggAcf(resid(ingenuo_estacional),
                     type = "correlation",
                     lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0.5), "cm"))

hist_ingenuo_estacional <- ggplot(resid(ingenuo_estacional),
                       aes(x=resid(ingenuo_estacional))) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 bins = 10) +
  xlab("Residuos") +
  scale_x_continuous(lim=c(-30,90)) +
  ylab("Densidad") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0), #arriba, derecha, abajo, izquierda
                           "cm"))

plot_grid(auto_ingenuo_estacional,
          plot_grid(FAC_ingenuo_estacional,
                    hist_ingenuo_estacional,
                    align = 'hv'),
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'h')

Normality_Test(resid(ingenuo_estacional),
               type = "SW")

Incorrelation(resid(ingenuo_estacional),
              type = "Ljung-Box")

accuracy(ingenuo_estacional, testing)

checkresiduals(ingenuo_estacional)

## COMPARACIÓN DE LOS MSE PARA DIFERENTES HORIZONTES DE PRONÓSTICO
serie_comparacion <- cbind(serie,
                          testing,
                          ingenuo$mean,
                          ingenuo_estacional$mean)

colnames(serie_comparacion) <- c("Entrenamiento","Prueba","Ingenuo","Ingenuo Estacional")

autoplot(serie_comparacion) +
  xlab("Meses") +
  ylab("Nº de Pasajeros Aéreos") +
  scale_y_continuous(breaks=seq(100,700,by=100),
                     lim=c(100,630)) +
  geom_point() +
  theme(legend.position = c(0.15, 0.83)) #izq = 0 - der = 1; abajo = 0 - arriba = 1

RMSE_ingenuo <- rep(0,12)
pronostico_ingenuo <- as.numeric(ingenuo$model$future)
for (h in 1:12) {
  RMSE_ingenuo[h] <- sqrt(sum((pronostico_ingenuo - testing[1:h])^2)/h)
}

RMSE_ingenuo

RMSE_ingenuo_estacional <- rep(0,12)
pronostico_ingenuo_estacional <- as.numeric(ingenuo_estacional$model$future)
for (h in 1:12) {
  RMSE_ingenuo_estacional[h] <- sqrt(sum((pronostico_ingenuo_estacional[1:h] - testing[1:h])^2)/h)
}

RMSE_ingenuo_estacional


##################
## MODELO ARIMA ##
##################

## DIFERENCIAS
x1 <- diff(training,lag=12)
x2 <- diff(x1,lag=1)

serie_x1 <- autoplot(x1) +
  xlab("Meses") +
  ylab("Residuos") +
  geom_point()

FAC_x1 <- ggAcf(x1,
                type = "correlation",
                lag.max = 25) +
  ggtitle("") +
  labs(x = "",
       y = "FACM")

FACP_x1 <- ggAcf(x1,
                 type = "partial",
                 lag.max = 25) + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACPM")

plot_grid(serie_x1,
          FAC_x1,
          FACP_x1,
          ncol = 1,
          align = 'v',
          axis = 'lr')

serie_x2 <- autoplot(x2) +
  xlab("Meses") +
  ylab("Residuos") +
  geom_point()

FAC_x2 <- ggAcf(x2,
                type = "correlation",
                lag.max = 25) +
  ggtitle("") +
  labs(x = "",
       y = "FACM")

FACP_x2 <- ggAcf(x2,
                 type = "partial",
                 lag.max = 25) + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACPM")

plot_grid(serie_x2,
          FAC_x2,
          FACP_x2,
          ncol = 1,
          align = 'v',
          axis = 'lr')

## DEFINICIÓN DEL MODELO
modelo_arima <- auto.arima(training,
                           method = "ML")

summary(modelo_arima)

pnorm(-0.2431/0.0894,
      mean = 0,
      sd = 1)

## ANÁLISIS DE LOS RESIDUOS
auto_arima <- autoplot(resid(modelo_arima)) +
  xlab("Tiempo") +
  ylab("Residuos") +
  scale_y_continuous(breaks=seq(-40,40,by=20),
                     lim=c(-40,30)) +
  geom_point()+
  theme(plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"))

FAC_arima <- ggAcf(resid(modelo_arima),
                                type = "correlation",
                                lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0.5), "cm"))

hist_arima <- ggplot(resid(modelo_arima),
                                  aes(x=resid(modelo_arima))) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 bins = 12) +
  xlab("Residuos") +
  scale_x_continuous() +
  ylab("Densidad") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0), #arriba, derecha, abajo, izquierda
                           "cm"))

plot_grid(auto_arima,
          plot_grid(FAC_arima,
                    hist_arima,
                    align = 'hv'),
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'h')

Normality_Test(resid(modelo_arima),
               type = "SW")

Incorrelation(resid(modelo_arima),
              type = "Ljung-Box")

checkresiduals(modelo_arima)

## PREDICCIÓN
forecast(modelo_arima,
         h = 12,
         level = 95)

plot(forecast(modelo_arima,
              h = 12,
              level = 95),
     main = "")

## PREDICCIÓN MANUAL
x2.pred.ar1 <- predict(arima(x2,order=c(1,0,0),
                             fixed = c(NA,0)),
                       n.ahead=12)$pred
x2.err.ar1 <- predict(arima(x2,order=c(1,0,0),
                            fixed = c(NA,0)),
                      n.ahead=12)$se

x.completada<-c(x2,
                x2.pred.ar1)

xinv1 <- diffinv(x.completada,
                 lag = 1,
                 xi = x1[1])
xinv2 <- diffinv(xinv1,
                 lag = 12,
                 xi = training[1:12])

x.reconstruida <- ts(xinv2,
                     start=1949,
                     frequency=12)

prediccion_arima <- window(x.reconstruida, 
                           start = 1960)

prediccion_arima

## RECM
RMSE_arima <- rep(0,12)
for (h in 1:12) {
  RMSE_arima[h] <- sqrt(sum((prediccion_arima[1:h] - testing[1:h])^2)/h)
}

RMSE_arima

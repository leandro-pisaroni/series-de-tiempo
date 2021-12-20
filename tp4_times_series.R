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
  library(urca)     #Tests de raíces unitarias
  library(uroot)    #Raíces unitarias estacionales
  library(astsa)    #Applied Statistical Time Series Analysis
  library(nortest)  #Tests de normalidad
})

source(paste0(path,"/Funciones.R"))


##################
## CARGAR DATOS ##
##################

## ARCHIVO CSV
leche <- read.csv2(file = "produccion_leche_mes_2016.csv",
                        sep = ",", 
                        header = T)

produccion_leche <- as.numeric(as.character(leche$produccion_leche))

## SERIE DE TIEMPO
leche_ts <- ts(produccion_leche,
               start = c(2016,1),
               frequency = 12)



## DATA FRAME PARA GRÁFICOS
leche_desc <- as.data.frame(produccion_leche)

leche_desc <- leche_desc %>%
  mutate(año = as.numeric(format(seq(as.Date('2016-01-01'),
                                     by = "months",
                                     length.out = 58),
                                 '%Y')))


##########################
## ANÁLISIS DESCRIPTIVO ##
##########################

## HISTOGRAMA
hist_leche <- ggplot(leche_desc, aes(x=produccion_leche)) + 
  geom_histogram(color = "darkblue",
                 fill = "skyblue3",
                 breaks = seq(600,1100,
                            by = 100)) +
  scale_x_continuous(breaks = seq(600,1100,
                                by = 100),
                     lim = c(600,1100)) +
  xlab("") +
  scale_y_continuous(breaks = seq(0,20,by=5),
                     lim = c(0,20)) +
  ylab("Densidad")

## BOXPLOT
boxplot_leche <- ggplot(leche_desc,
       aes(x = produccion_leche)) + 
  geom_boxplot(color = "darkblue",
               fill = "skyblue3") +
  scale_x_continuous(breaks = seq(600,1100,
                                by=100),
                     lim = c(600,1100)) +
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

## ESTADÍSTICAS DESCRIPTIVAS
round(stat.desc(leche_ts),3)
quantile(leche_ts, na.rm = T)
round(skewness(produccion_leche),3)
round(kurtosis(produccion_leche),3)


#####################
## SERIE DE TIEMPO ##
#####################

## GRÁFICA DE LA SERIE
autoplot(leche_ts) +
  xlab("Tiempo") +
  ylab("Producción de leche [millones de L]") +
  scale_y_continuous(lim=c(600,1100)) +
  geom_point()

## DESCOMPOSICIÓN
autoplot(decompose(leche_ts, type = "additive")) +
  ggtitle("") +
  labs(x = "Tiempo")

## AUTOCORRELACIONES
FAC <- ggAcf(leche_ts,
      type="correlation",
      lag.max = 24) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM")+
  scale_y_continuous(breaks=seq(-0.8,0.8,by=0.2),
                     lim=c(-0.8,0.8))

FACP <- ggAcf(leche_ts,
      type = "partial") + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACPM") +
  scale_y_continuous(breaks=seq(-0.8,0.8,by=0.2),
                     lim=c(-0.8,0.8))

plot_grid(FAC,
          FACP,
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'v',
          axis = 'lr')


###############################
## TESTS DE RAÍCES UNITARIAS ##
###############################

## RAÍCES UNITARIAS ESTACIONALES (HEGY)
test_hegy <- hegy.test(leche_ts,
               deterministic = c(0,0,1)) #Dummies estacionales

## VERIFICACIÓN DE LOS SUPUESTOS DEL MODELO DEL TEST HEGY
checkresiduals(test_hegy)
ad.test(residuals(test_hegy))

## NÚMERO DE DIFERENCIACIONES ESTACIONALES
nsdiffs(leche_ts,
        test = "hegy")


########################
## SERIE DIFERENCIADA ##
########################

## DIFERENCIAS TOMANDO s = 12 Y d = D = 1
serie_Z <- diff(diff(leche_ts,
                     lag = 12),
                lag = 1)

auto_z <- autoplot(serie_Z) +
  xlab("Serie {Z}") +
  ylab("") +
  geom_point()

FAC_z <- ggAcf(serie_Z,
               type="correlation",
               lag.max = 24) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM de {Z}")+
  scale_y_continuous(breaks=seq(-0.8,0.8,by=0.2),
                     lim=c(-0.4,0.4))

FACP_z <- ggAcf(serie_Z,
                type = "partial") + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACPM de {Z}") +
  scale_y_continuous(breaks=seq(-0.8,0.8,by=0.2),
                     lim=c(-0.4,0.4))

plot_grid(auto_z,
          FAC_z,
          FACP_z,
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'v',
          axis = 'lr')


#############
## MODELOS ##
#############

## MODELOS
modelo0 <- arima(leche_ts,
                 order = c(0, 1, 0),
                 seasonal = list(
                   order = c(0, 1,0),
                   period = 12))
modelo1 <- arima(leche_ts,
                 order = c(1, 1, 0),
                 seasonal = list(
                   order = c(0, 1,0),
                   period = 12))
modelo2 <- arima(leche_ts,
                 order = c(0, 1, 1),
                 seasonal = list(
                   order = c(0, 1, 0),
                   period = 12))
modelo3 <- arima(leche_ts,
                 order = c(0, 1, 0),
                 seasonal = list(
                   order = c(1, 1, 0),
                   period = 12))
modelo4 <- arima(leche_ts,
                 order = c(0, 1, 0),
                 seasonal = list(
                   order = c(0, 1, 1),
                   period = 12))
modelo5 <- arima(leche_ts,
                 order = c(1, 1, 1),
                 seasonal = list(
                   order = c(0, 1, 0),
                   period = 12))
modelo6 <- arima(leche_ts,
                 order = c(0, 1, 0),
                 seasonal = list(
                   order = c(1, 1, 1),
                   period = 12))

AIC(modelo0,modelo1,modelo2,modelo3,modelo4,modelo5,modelo6)
BIC(modelo0,modelo1,modelo2,modelo3,modelo4,modelo5,modelo6)

## MODELO PARA LA SERIE ORIGINAL
auto.arima(leche_ts) #ARIMA(0,1,0)(0,1,1)[12]

modelo_sarima <- sarima(leche_ts,
       p = 0,
       d = 1,
       q = 0,
       P = 0,
       D = 1,
       Q = 1,
       S = 12,
       no.constant = TRUE)

modelo_arima <- arima(leche_ts,
                      order = c(0, 1, 0),
                      seasonal = list(
                        order = c(0, 1, 1),
                        period = 12))

checkresiduals(modelo_sarima$fit) #Con ambos códigos se construye el mismo modelo
checkresiduals(modelo_arima)

ad.test(residuals(modelo_sarima$fit))
ad.test(residuals(modelo_arima))

## MODELO PARA LA SERIE Z
modelo_Z <- arima(serie_Z, #MA con rezagos de orden 12 y sin intercepto
          order = c(0, 0, 12),
          fixed = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0),
          method = "ML")

summary(modelo_Z)
pt(-0.5618/0.2215,
   df=57)

checkresiduals(modelo_Z)
ad.test(residuals(modelo_Z))

auto_modelo_z <- autoplot(resid(modelo_Z)) +
  xlab("Tiempo") +
  ylab("Residuos") +
  scale_y_continuous(breaks=seq(-100,100,by=25),
                     lim=c(-50,100)) +
  geom_point()+
  theme(plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"))

FAC_modelo_z <- ggAcf(resid(modelo_Z),
                   type = "correlation",
                   lag.max = 25) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0.5), "cm"))

hist_modelo_z <- ggplot(resid(modelo_Z),
                     aes(x=resid(modelo_Z))) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 bins = 12) +
  xlab("Residuos") +
  scale_x_continuous() +
  ylab("Densidad") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0),
                           "cm"))

## TEST DE LJUNG-BOX
ts <- resid(modelo_Z)
p_ljung_box = NULL
s_ljung_box = NULL
for(i in 0:24){
  p_ljung_box[i] = Box.test(ts,lag = i,type = "Ljung-Box",fitdf = 0)$p.value
  s_ljung_box[i] = Box.test(ts,lag = i,type = "Ljung-Box",fitdf = 0)$statistic
}
table = data.frame(j = 1:24,
                   P_Value = p_ljung_box,
                   Statistic = s_ljung_box)
table

testlb_modelo_z <- ggplot(table, aes(x = j, y = P_Value)) +
  geom_point() +
  geom_line(y = 0.05,
            linetype = "dashed",
            col = "Blue") +
  scale_y_continuous("Test Ljung-Box (Valor p)",
                     limits = c(0,1)) +
  xlab("Rezago") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0.5), #arriba, derecha, abajo, izquierda
                           "cm"))

## GRÁFICOS JUNTOS
plot_grid(auto_modelo_z,
          testlb_modelo_z,
          plot_grid(FAC_modelo_z,
                    hist_modelo_z,
                    align = 'hv'),
          ncol = 1,
          align = 'h')

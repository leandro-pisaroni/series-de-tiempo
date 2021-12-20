rm(list = ls()) #Remueve todos los objetos del entorno de trabajo

######################
## SETEO DE CARPETA ##
######################

path = "C:/Users/leand/Google Drive/Maestría Estadística Aplicada/Series de Tiempo/TPs"
setwd(path)
getwd()
dir()




#######################
## CARGA DE PAQUETES ##
#######################

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(pastecs)
  library(moments)
  library(forecast)
  library(FitAR)
  library(urca)
  library(vars)
  library(uroot)
  library(nortest)
})




####################
## CARGA DE DATOS ##
####################

## ARCHIVO XSLX
datos <- read_xlsx("produccion_mensual.xlsx",
                 sheet = 1,
                 col_types = "numeric")

## VARIABLES SEPARADAS
produccion_leche <- as.numeric(as.character(datos$produccion_leche))
produccion_queso <- as.numeric(as.character(datos$produccion_queso))
produccion_dulce_leche <- as.numeric(as.character(datos$produccion_ddl))

## SERIES DE TIEMPO
leche_ts <- ts(produccion_leche,
               start = c(2016,1),
               frequency = 12)
queso_ts <- ts(produccion_queso,
               start = c(2016,1),
               frequency = 12)
dulce_leche_ts <- ts(produccion_dulce_leche,
                     start = c(2016,1),
                     frequency = 12)

## DATA FRAMES PARA GRÁFICOS
prod_leche <- as.data.frame(produccion_leche)
prod_queso <- as.data.frame(produccion_queso)
prod_dulce <- as.data.frame(produccion_dulce_leche)




##########################
## ANÁLISIS DESCRIPTIVO ##
##########################

## HISTOGRAMAS
hist_leche <- ggplot(prod_leche, aes(x=produccion_leche)) + 
  geom_histogram(color = "darkblue",
                 fill = "skyblue3",
                 breaks = seq(600,1100,
                              by = 100)) +
  scale_x_continuous(breaks = seq(600,1100,
                                  by = 100),
                     lim = c(600,1100)) +
  xlab("Producción de Leche [Ml]") +
  scale_y_continuous(breaks = seq(0,20,
                                  by=5),
                     lim = c(0,20)) +
  ylab("Densidad")

hist_queso <- ggplot(prod_queso, aes(x=produccion_queso)) + 
  geom_histogram(color = "darkblue",
                 fill = "skyblue3",
                 breaks = seq(5,30,
                              by = 5)) +
  scale_x_continuous(breaks = seq(5,30,
                                  by = 5),
                     lim = c(5,30)) +
  xlab("Producción de Quesos [kt]") +
  scale_y_continuous(breaks = seq(0,15,
                                  by=5),
                     lim = c(0,15)) +
  ylab("Densidad")

hist_dulce <- ggplot(prod_dulce, aes(x=produccion_dulce_leche)) + 
  geom_histogram(color = "darkblue",
                 fill = "skyblue3",
                 breaks = seq(1,3.5,
                              by = 0.5)) +
  scale_x_continuous(breaks = seq(1,3.5,
                                  by = 0.5),
                     lim = c(1,3.5)) +
  xlab("Producción de Dulce de Leche [kt]") +
  scale_y_continuous(breaks = seq(0,20,
                                  by=5),
                     lim = c(0,20)) +
  ylab("Densidad")

plot_grid(hist_leche,
          hist_queso,
          hist_dulce,
          ncol = 1,
          align = 'v',
          axis = 'lr')

## ESTADÍSTICAS DESCRIPTIVAS
round(stat.desc(leche_ts),2)
quantile(leche_ts, na.rm = T)
round(skewness(produccion_leche),2)
round(kurtosis(produccion_leche),2)

round(stat.desc(queso_ts),2)
quantile(queso_ts, na.rm = T)
round(skewness(produccion_queso),2)
round(kurtosis(produccion_queso),2)

round(stat.desc(dulce_leche_ts),2)
quantile(dulce_leche_ts, na.rm = T)
round(skewness(produccion_dulce_leche),2)
round(kurtosis(produccion_dulce_leche),2)

## CORRELACIONES LINEALES
cor(datos)




############################
## ANÁLISIS DE LAS SERIES ##
############################

## GRÁFICOS DE LAS SERIES
ggtsdisplay(leche_ts)
ggtsdisplay(quesos_ts)
ggtsdisplay(dulce_leche_ts)

serie_leche <- autoplot(leche_ts) +
  xlab("") +
  scale_y_continuous(breaks = seq(600,1100,
                                  by = 100),
                     lim = c(650,1050)) +
  ylab("Producción Leche [Ml]") +
  geom_point()

serie_queso <- autoplot(queso_ts) +
  xlab("") +
  scale_y_continuous(breaks = seq(5,30,
                                  by = 5),
                     lim = c(7,28)) +
  ylab("Producción Quesos [kt]") +
  geom_point()

serie_dulce <- autoplot(dulce_leche_ts) +
  xlab("Año") +
  scale_y_continuous(breaks = seq(1,3,
                                  by = 0.5),
                     lim = c(1.4,3.4)) +
  ylab("Producción Dulce [kt]") +
  geom_point()

plot_grid(serie_leche,
          serie_queso,
          serie_dulce,
          ncol = 1,
          align = 'v',
          axis = 'lr')

## DESCOMPOSICIÓN
autoplot(decompose(leche_ts, type = "additive")) +
  ggtitle("") +
  labs(x = "Tiempo")
autoplot(decompose(queso_ts, type = "additive")) +
  ggtitle("") +
  labs(x = "Tiempo")
autoplot(decompose(dulce_leche_ts, type = "additive")) +
  ggtitle("") +
  labs(x = "Tiempo")

## COMIENZO A TRABAJAR CON EL CONJUNTO DE DEFINICIÓN
leche_ts <- ts(produccion_leche,
               start = c(2016,1),
               end = c(2019,12),
               frequency = 12)
queso_ts <- ts(produccion_queso,
               start = c(2016,1),
               end = c(2019,12),
               frequency = 12)
dulce_leche_ts <- ts(produccion_dulce_leche,
                     start = c(2016,1),
                     end = c(2019,12),
                     frequency = 12)

## AUTOCORRELACIONES
FAC_leche <- ggAcf(leche_ts,
                  type="correlation",
                  lag.max = 18) +
  ggtitle("") +
  labs(x = "",
       y = "Leche")

FACP_leche <- ggAcf(leche_ts,
                   type = "partial",
                   lag.max = 18) + 
  ggtitle("") +
  labs(x = "",
       y = "")

FAC_queso <- ggAcf(queso_ts,
                  type="correlation",
                  lag.max = 18) +
  ggtitle("") +
  labs(x = "",
       y = "Queso")

FACP_queso <- ggAcf(queso_ts,
                   type = "partial",
                   lag.max = 18) + 
  ggtitle("") +
  labs(x = "",
       y = "")

FAC_dulce <- ggAcf(dulce_leche_ts,
                     type="correlation",
                     lag.max = 18) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "Dulce")

FACP_dulce <- ggAcf(dulce_leche_ts,
                      type = "partial",
                      lag.max = 18) + 
  ggtitle("") +
  labs(x = "Rezago",
       y = "")

plot_grid(FAC_leche,
          FACP_leche,
          FAC_queso,
          FACP_queso,
          FAC_dulce,
          FACP_dulce,
          ncol = 2,
          align = 'v',
          axis = 'lr')




#####################
## ESTACIONARIEDAD ##
#####################

## RAÍCES UNITARIAS ESTACIONALES (HEGY)
test_hegy_leche <- hegy.test(leche_ts,
                             deterministic = c(0,0,1)) #Dummies estacionales
test_hegy_queso <- hegy.test(queso_ts,
                              deterministic = c(0,0,1)) #Dummies estacionales
test_hegy_dulce <- hegy.test(dulce_leche_ts,
                             deterministic = c(0,0,1)) #Dummies estacionales

test_hegy_leche
test_hegy_queso
test_hegy_dulce

## VERIFICACIÓN DE LOS SUPUESTOS DEL MODELO DEL TEST HEGY
checkresiduals(test_hegy_leche)
ad.test(residuals(test_hegy_leche))
checkresiduals(test_hegy_queso)
ad.test(residuals(test_hegy_queso))
checkresiduals(test_hegy_dulce)
ad.test(residuals(test_hegy_dulce))

## NÚMERO DE DIFERENCIACIONES ESTACIONALES
nsdiffs(leche_ts,
        test = "hegy")
nsdiffs(queso_ts,
        test = "hegy")
nsdiffs(dulce_leche_ts,
        test = "hegy")




##########################
## SERIES DIFERENCIADAS ##
##########################

## DIFERENCIACIÓN DE LAS SERIES
leche_dif_ts <- diff(diff(leche_ts,
                          lag = 12),
                     lag = 1)
queso_dif_ts <- diff(diff(queso_ts,
                          lag = 12),
                      lag = 1)
dulce_dif_ts <- diff(diff(dulce_leche_ts,
                          lag = 12),
                     lag = 1)

## GRÁFICOS DE LAS SERIES
ggtsdisplay(leche_dif_ts)
ggtsdisplay(queso_dif_ts)
ggtsdisplay(dulce_dif_ts)

serie_leche_dif <- autoplot(leche_dif_ts) +
  xlab("") +
  scale_y_continuous(breaks = seq(-50,100,
                                  by = 50),
                     lim = c(-50,100)) +
  ylab("Diferencias Leche [Ml]") +
  geom_point()

serie_queso_dif <- autoplot(queso_dif_ts) +
  xlab("") +
  scale_y_continuous(breaks = seq(-10,10,
                                  by = 5),
                     lim = c(-8,8.5)) +
  ylab("Diferencias Queso [kt]") +
  geom_point()

serie_dulce_dif <- autoplot(dulce_dif_ts) +
  xlab("Año") +
  scale_y_continuous(breaks = seq(-2,2,
                                  by = 1),
                     lim = c(-1.5,2)) +
  ylab("Diferencias Dulce [kt]") +
  geom_point()

plot_grid(serie_leche_dif,
          serie_queso_dif,
          serie_dulce_dif,
          ncol = 1,
          align = 'v',
          axis = 'lr')

## ESTACIONARIEDAD
summary(ur.df(leche_dif_ts)) #Estacionaria
summary(ur.df(queso_dif_ts)) #Estacionaria
summary(ur.df(dulce_dif_ts)) #Estacionaria




##############################
## VECTORES AUTORREGRESIVOS ##
##############################

## SELECCIÓN DEL MODELO VAR
series <- data.frame(leche_dif_ts,
                     queso_dif_ts,
                     dulce_dif_ts)

VARselect(series,
          lag.max = 3)

## CONSTRUCCIÓN DEL MODELO VAR(1)
modelo_var <- VAR(series,
                  p = 1,
                  type = "none")

summary(modelo_var)
plot(modelo_var)

## AUTOVALORES A1
A1 <- matrix(rbind(modelo_var[["varresult"]][["leche_dif_ts"]][["coefficients"]],
             modelo_var[["varresult"]][["queso_dif_ts"]][["coefficients"]],
             modelo_var[["varresult"]][["dulce_dif_ts"]][["coefficients"]]),
             nrow = 3)
autovalores_A1 <- eigen(A1)

A1.diag <- diag(diag(A1))
autovalores_A1.diag <- eigen(A1.diag)

## GRÁFICOS
leche_dif_fit <- ts(modelo_var[["varresult"]][["leche_dif_ts"]][["fitted.values"]],
                    start = c(2017,3),
                    frequency = 12)

leche_var <- data.frame(Tiempo = c(time(leche_dif_ts)),
                        leche_dif = c(leche_dif_ts),
                        leche_fit = c(NA,leche_dif_fit))

graf_ajuste_leche <- ggplot(data = leche_var, aes(x = Tiempo,
                             y = value,
                             color = Serie)) +
  ylab("Diferencias Leche [Ml]") +
  geom_line(aes(y = leche_dif ,
                col= "Original"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = leche_fit,
                col = "Ajustada"),
            size = 1,
            alpha = 2) +
  theme(legend.position = "none")

queso_dif_fit <- ts(modelo_var[["varresult"]][["queso_dif_ts"]][["fitted.values"]],
                    start = c(2017,3),
                    frequency = 12)

queso_var <- data.frame(Tiempo = c(time(queso_dif_ts)),
                         queso_dif = c(queso_dif_ts),
                         queso_fit = c(NA,queso_dif_fit))

graf_ajuste_queso <- ggplot(data = queso_var, aes(x = Tiempo,
                                                  y = value,
                                                  color = Serie)) +
  ylab("Diferencias Queso [kt]") +
  geom_line(aes(y = queso_dif ,
                col= "Original"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = queso_fit,
                col = "Ajustada"),
            size = 1,
            alpha = 2) +
  theme(legend.position = "none")

dulce_dif_fit <- ts(modelo_var[["varresult"]][["dulce_dif_ts"]][["fitted.values"]],
                     start = c(2017,3),
                     frequency = 12)

dulce_var <- data.frame(Tiempo = c(time(dulce_dif_ts)),
                        dulce_dif = c(dulce_dif_ts),
                        dulce_fit = c(NA,dulce_dif_fit))

graf_ajuste_dulce <- ggplot(data = dulce_var, aes(x = Tiempo,
                                                 y = value,
                                                 color = Serie)) +
  ylab("Diferencias Dulce [kt]") +
  geom_line(aes(y = dulce_dif ,
                col= "Original"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = dulce_fit,
                col = "Ajustada"),
            size = 1,
            alpha = 2) +
  theme(legend.position = "none")

plot_grid(graf_ajuste_leche,
          graf_ajuste_queso,
          graf_ajuste_dulce,
          ncol = 1,
          align = 'v',
          axis = 'lr')

## ANÁLISIS ESTRUCTURAL
grangertest(leche_dif_ts~queso_dif_ts) #Sí hay causalidad (al 10%)
grangertest(leche_dif_ts~dulce_dif_ts) #No hay causalidad (valor p apenas mayor a 0.10)

grangertest(queso_dif_ts~leche_dif_ts) #No hay causalidad (valor p apenas mayor a 0.10)
grangertest(queso_dif_ts~dulce_dif_ts) #No hay causalidad

grangertest(dulce_dif_ts~leche_dif_ts) #No hay causalidad
grangertest(dulce_dif_ts~queso_dif_ts) #No hay causalidad


irf_leche_queso <- irf(modelo_var,
                       impulse = "queso_dif_ts",
                       response = "leche_dif_ts",
                       n.ahead = 20,
                       ortho = FALSE,
                       cumulative = FALSE)

data_irf_leche_queso <- data.frame(Períodos = c(1:length(irf_leche_queso$irf$queso_dif_ts)),
                                   irf = c(irf_leche_queso$irf$queso_dif_ts),
                                   irf_low = c(irf_leche_queso$Lower$queso_dif_ts),
                                   irf_up = c(irf_leche_queso$Upper$queso_dif_ts))

graf_irf_leche_queso <- ggplot(data = data_irf_leche_queso, aes(x = Períodos,
                                                                y = value,
                                                                color = Serie)) +
  ylab("Diferencias Queso") +
  geom_line(aes(y = irf ,
                col= "IRF"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = irf_low,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  geom_line(aes(y = irf_up,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title="IRF Leche-Queso")

irf_queso_leche <- irf(modelo_var,
                       impulse = "leche_dif_ts",
                       response = "queso_dif_ts",
                       n.ahead = 20,
                       ortho = FALSE,
                       cumulative = FALSE)

data_irf_queso_leche <- data.frame(Períodos = c(1:length(irf_queso_leche$irf$leche_dif_ts)),
                                   irf = c(irf_queso_leche$irf$leche_dif_ts),
                                   irf_low = c(irf_queso_leche$Lower$leche_dif_ts),
                                   irf_up = c(irf_queso_leche$Upper$leche_dif_ts))

graf_irf_queso_leche <- ggplot(data = data_irf_queso_leche, aes(x = Períodos,
                                                                y = value,
                                                                color = Serie)) +
  ylab("Diferencias Leche") +
  geom_line(aes(y = irf ,
                col= "IRF"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = irf_low,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  geom_line(aes(y = irf_up,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title="IRF Queso-Leche")

irf_leche_dulce <- irf(modelo_var,
                       impulse = "dulce_dif_ts",
                       response = "leche_dif_ts",
                       n.ahead = 20,
                       ortho = FALSE,
                       cumulative = FALSE)

data_irf_leche_dulce <- data.frame(Períodos = c(1:length(irf_leche_dulce$irf$dulce_dif_ts)),
                                   irf = c(irf_leche_dulce$irf$dulce_dif_ts),
                                   irf_low = c(irf_leche_dulce$Lower$dulce_dif_ts),
                                   irf_up = c(irf_leche_dulce$Upper$dulce_dif_ts))

graf_irf_leche_dulce <- ggplot(data = data_irf_leche_dulce, aes(x = Períodos,
                                                                y = value,
                                                                color = Serie)) +
  ylab("Diferencias Dulce") +
  geom_line(aes(y = irf ,
                col= "IRF"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = irf_low,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  geom_line(aes(y = irf_up,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title="IRF Leche-Dulce")

irf_dulce_leche <- irf(modelo_var,
                       impulse = "leche_dif_ts",
                       response = "dulce_dif_ts",
                       n.ahead = 20,
                       ortho = FALSE,
                       cumulative = FALSE)

data_irf_dulce_leche <- data.frame(Períodos = c(1:length(irf_dulce_leche$irf$leche_dif_ts)),
                                   irf = c(irf_dulce_leche$irf$leche_dif_ts),
                                   irf_low = c(irf_dulce_leche$Lower$leche_dif_ts),
                                   irf_up = c(irf_dulce_leche$Upper$leche_dif_ts))

graf_irf_dulce_leche <- ggplot(data = data_irf_dulce_leche, aes(x = Períodos,
                                                                y = value,
                                                                color = Serie)) +
  ylab("Diferencias Leche") +
  geom_line(aes(y = irf ,
                col= "IRF"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = irf_low,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  geom_line(aes(y = irf_up,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title="IRF Dulce-Leche")

irf_queso_dulce <- irf(modelo_var,
                       impulse = "dulce_dif_ts",
                       response = "queso_dif_ts",
                       n.ahead = 20,
                       ortho = FALSE,
                       cumulative = FALSE)

data_irf_queso_dulce <- data.frame(Períodos = c(1:length(irf_queso_dulce$irf$dulce_dif_ts)),
                                   irf = c(irf_queso_dulce$irf$dulce_dif_ts),
                                   irf_low = c(irf_queso_dulce$Lower$dulce_dif_ts),
                                   irf_up = c(irf_queso_dulce$Upper$dulce_dif_ts))

graf_irf_queso_dulce <- ggplot(data = data_irf_queso_dulce, aes(x = Períodos,
                                                                y = value,
                                                                color = Serie)) +
  ylab("Diferencias Dulce") +
  geom_line(aes(y = irf ,
                col= "IRF"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = irf_low,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  geom_line(aes(y = irf_up,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title="IRF Queso-Dulce")

irf_dulce_queso <- irf(modelo_var,
                       impulse = "queso_dif_ts",
                       response = "dulce_dif_ts",
                       n.ahead = 20,
                       ortho = FALSE,
                       cumulative = FALSE)

data_irf_dulce_queso <- data.frame(Períodos = c(1:length(irf_dulce_queso$irf$queso_dif_ts)),
                                   irf = c(irf_dulce_queso$irf$queso_dif_ts),
                                   irf_low = c(irf_dulce_queso$Lower$queso_dif_ts),
                                   irf_up = c(irf_dulce_queso$Upper$queso_dif_ts))

graf_irf_dulce_queso <- ggplot(data = data_irf_dulce_queso, aes(x = Períodos,
                                                                y = value,
                                                                color = Serie)) +
  ylab("Diferencias Queso") +
  geom_line(aes(y = irf ,
                col= "IRF"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = irf_low,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  geom_line(aes(y = irf_up,
                col = "IC"),
            size = 1,
            alpha = 2,
            linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title="IRF Dulce-Queso")

plot_grid(graf_irf_leche_queso,
          graf_irf_queso_leche,
          graf_irf_leche_dulce,
          graf_irf_dulce_leche,
          graf_irf_queso_dulce,
          graf_irf_dulce_queso,
          ncol = 2,
          align = 'h')

## ANÁLISIS RESIDUOS
serial.test(modelo_var,
            lags.pt = 11,
            type="PT.adjusted") # Test de Autocorrelación
normality.test(modelo_var) #Test de Normalidad
arch.test(modelo_var,
          lags.multi = 11) #Test para evaluar homocedasticidad


## ANÁLISIS DE LOS RESIDUOS
leche_resid <- ts(modelo_var[["varresult"]][["leche_dif_ts"]][["residuals"]],
                    start = c(2017,3),
                    frequency = 12)
queso_resid <- ts(modelo_var[["varresult"]][["queso_dif_ts"]][["residuals"]],
                  start = c(2017,3),
                  frequency = 12)
dulce_resid <- ts(modelo_var[["varresult"]][["dulce_dif_ts"]][["residuals"]],
                  start = c(2017,3),
                  frequency = 12)

graf_resid_leche <- autoplot(leche_resid) +
  xlab("Tiempo") +
  ylab("Residuos Leche") +
  scale_y_continuous(breaks=seq(-75,100,
                                by=25),
                     lim=c(-55,80)) +
  geom_point()

graf_resid_queso <- autoplot(queso_resid) +
  xlab("Tiempo") +
  ylab("Residuos Queso") +
  scale_y_continuous(breaks=seq(-8,8,
                                by=2),
                     lim=c(-9,6)) +
  geom_point()

graf_resid_dulce <- autoplot(dulce_resid) +
  xlab("Tiempo") +
  ylab("Residuos Dulce") +
  scale_y_continuous(breaks=seq(-1.5,1.5,
                                by=0.5),
                     lim=c(-1.1,1.4)) +
  geom_point()

plot_grid(graf_resid_leche,
          graf_resid_queso,
          graf_resid_dulce,
          ncol = 1,
          align = 'v',
          axis = 'lr')

FAC_resid_leche <- ggAcf(leche_resid,
                   type = "correlation",
                   lag.max = 12) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "Leche")

FAC_resid_queso <- ggAcf(queso_resid,
                         type = "correlation",
                         lag.max = 12) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "Queso")

FAC_resid_dulce <- ggAcf(dulce_resid,
                         type = "correlation",
                         lag.max = 12) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "Dulce")

plot_grid(FAC_resid_leche,
          FAC_resid_queso,
          FAC_resid_dulce,
          ncol = 1,
          align = 'v',
          axis = 'lr')

## PRONÓSTICOS
pronostico_var <- predict(modelo_var,
                          n.ahead = 6)

x1 <- diff(leche_ts,
           lag=12)
x2 <- diff(x1,
           lag=1)
x.completada<-c(x2,
                pronostico_var$fcst$leche_dif_ts[,1])
xinv1 <- diffinv(x.completada,
                 lag = 1,
                 xi = x1[1])
xinv2 <- diffinv(xinv1,
                 lag = 12,
                 xi = leche_ts[1:12])
x.reconstruida <- ts(xinv2,
                     start = 2016,
                     frequency=12)
pronostico_var_leche <- window(x.reconstruida,
                               start = 2020)

x1 <- diff(queso_ts,
           lag=12)
x2 <- diff(x1,
           lag=1)
x.completada<-c(x2,
                pronostico_var$fcst$queso_dif_ts[,1])
xinv1 <- diffinv(x.completada,
                 lag = 1,
                 xi = x1[1])
xinv2 <- diffinv(xinv1,
                 lag = 12,
                 xi = queso_ts[1:12])
x.reconstruida <- ts(xinv2,
                     start = 2016,
                     frequency = 12)
pronostico_var_queso <- window(x.reconstruida,
                               start = 2020)

x1 <- diff(dulce_leche_ts,
           lag=12)
x2 <- diff(x1,
           lag=1)
x.completada<-c(x2,
                pronostico_var$fcst$dulce_dif_ts[,1])
xinv1 <- diffinv(x.completada,
                 lag = 1,
                 xi = x1[1])
xinv2 <- diffinv(xinv1,
                 lag = 12,
                 xi = dulce_leche_ts[1:12])
x.reconstruida <- ts(xinv2,
                     start = 2016,
                     frequency = 12)
pronostico_var_dulce <- window(x.reconstruida,
                               start = 2020)

pronostico_var_leche
pronostico_var_queso
pronostico_var_dulce

## GRÁFICOS DE LOS PRONÓSTICOS
leche_completa_ts <- ts(produccion_leche,
                        start = c(2016,1),
                        frequency = 12)
queso_completa_ts <- ts(produccion_queso,
                        start = c(2016,1),
                        frequency = 12)
dulce_leche_completa_ts <- ts(produccion_dulce_leche,
                              start = c(2016,1),
                              frequency = 12)

ts.plot(leche_completa_ts,pronostico_var_leche,
        col = c("Black", "Red"),
        gpars = list(xlab = "Tiempo",
                     ylab = "Producción de Leche [Ml]",
                     lty = c(1,2))) 
ts.plot(queso_completa_ts,pronostico_var_queso,
        col = c("Black", "Red"),
        gpars = list(xlab = "Tiempo",
                     ylab = "Producción de Queso [Mt]",
                     lty = c(1,2))) 
ts.plot(dulce_leche_completa_ts,pronostico_var_dulce,
        col = c("Black", "Red"),
        gpars = list(xlab = "Tiempo",
                     ylab = "Producción de Dulce de Leche [Mt]",
                     lty = c(1,2)))

leche_pronostico <- data.frame(Tiempo = c(time(leche_completa_ts)),
                        leche_ts = c(leche_completa_ts),
                        leche_pron = c(rep(NA,12*4),pronostico_var_leche))

graf_pron_leche <- ggplot(data = leche_pronostico, aes(x = Tiempo,
                                                  y = value,
                                                  color = Serie)) +
  ylab("Producción Leche [Ml]") +
  geom_line(aes(y = leche_ts ,
                col= "Original"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = leche_pron,
                col = "Ajustada"),
            size = 1,
            alpha = 2) +
  theme(legend.position = "none")

queso_pronostico <- data.frame(Tiempo = c(time(queso_completa_ts)),
                               queso_ts = c(queso_completa_ts),
                               queso_pron = c(rep(NA,12*4),pronostico_var_queso))

graf_pron_queso <- ggplot(data = queso_pronostico, aes(x = Tiempo,
                                                       y = value,
                                                       color = Serie)) +
  ylab("Producción Queso [Mt]") +
  geom_line(aes(y = queso_ts ,
                col= "Original"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = queso_pron,
                col = "Ajustada"),
            size = 1,
            alpha = 2) +
  theme(legend.position = "none")

dulce_pronostico <- data.frame(Tiempo = c(time(dulce_leche_completa_ts)),
                               dulce_ts = c(dulce_leche_completa_ts),
                               dulce_pron = c(rep(NA,12*4),pronostico_var_dulce))

graf_pron_dulce <- ggplot(data = dulce_pronostico, aes(x = Tiempo,
                                                       y = value,
                                                       color = Serie)) +
  ylab("Producción Dulce [kt]") +
  geom_line(aes(y = dulce_ts ,
                col= "Original"),
            size = 1,
            alpha = 2) +
  geom_line(aes(y = dulce_pron,
                col = "Ajustada"),
            size = 1,
            alpha = 2) +
  theme(legend.position = "none")

plot_grid(graf_pron_leche,
          graf_pron_queso,
          graf_pron_dulce,
          ncol = 1,
          align = 'h')

## MAPE PARA LOS PRONÓSTICOS
leche_ts_mape <- leche_completa_ts[49:54]
leche_pron_mape <- as.numeric(pronostico_var_leche)
MAPE_leche <- rep(0,6)
for (h in 1:6) {
  MAPE_leche[h] <- sqrt(sum(abs(leche_ts_mape[1:h] - leche_pron_mape[1:h])/abs(leche_ts_mape[1:h]))/h)
}
MAPE_leche

queso_ts_mape <- queso_completa_ts[49:54]
queso_pron_mape <- as.numeric(pronostico_var_queso)
MAPE_queso <- rep(0,6)
for (h in 1:6) {
  MAPE_queso[h] <- sqrt(sum(abs(queso_ts_mape[1:h] - queso_pron_mape[1:h])/abs(queso_ts_mape[1:h]))/h)
}
MAPE_queso

dulce_ts_mape <- dulce_leche_completa_ts[49:54]
dulce_pron_mape <- as.numeric(pronostico_var_dulce)
MAPE_dulce <- rep(0,6)
for (h in 1:6) {
  MAPE_dulce[h] <- sqrt(sum(abs(dulce_ts_mape[1:h] - dulce_pron_mape[1:h])/abs(dulce_ts_mape[1:h]))/h)
}
MAPE_dulce




######################
## REDES NEURONALES ##
######################

## SELECCIÓN DEL MODELO
leche_ts_training <- head(leche_completa_ts,48)
leche_ts_testing <- tail(leche_completa_ts,6)

RECM_definicion <- matrix(NA, nrow = 5, ncol = 5)
RECM_prueba <- matrix(NA, nrow = 5, ncol = 5)
for (p in 1:5) {
  for (k in 1:5) {
    set.seed(123)
    modelo_NNAR = nnetar(leche_ts_training,
                         p = p,
                         P = 1,
                         size = k,
                         repeats = 50)
    fc1 = forecast(modelo_NNAR,
                   h = 6)
    RECM_definicion[p,k] <- accuracy(fc1,leche_ts_testing)[1,2]
    RECM_prueba[p,k] <- accuracy(fc1,leche_ts_testing)[2,2]
  }
}

round(RECM_definicion,2)
round(RECM_prueba,2)

## SERIE SIN ESTACIONALIDAD
leche_desest <- diff(leche_ts,
                     lag = 12)

FAC_leche_desest <- ggAcf(leche_desest,
                          type="correlation",
                          lag.max = 12) +
  ggtitle("") +
  labs(x = "",
       y = "FACM")

FACP_leche_desest <- ggAcf(leche_desest,
                    type = "partial",
                    lag.max = 12) + 
  ggtitle("") +
  labs(x = "Rezagos",
       y = "FACPM")

plot_grid(FAC_leche_desest,
          FACP_leche_desest,
          ncol = 1,
          align = 'v',
          axis = 'lr')

modelo_AR1 <- arima(leche_desest,
                    order = c(1,0,0),
                    include.mean = F)
modelo_AR2 <- arima(leche_desest,
                    order = c(2,0,0),
                    include.mean = F)
modelo_AR3 <- arima(leche_desest,
                    order = c(3,0,0),
                    include.mean = F)

AIC(modelo_AR1,
    modelo_AR2,
    modelo_AR3)

BIC(modelo_AR1,
    modelo_AR2,
    modelo_AR3)

## ENTRENAR MODELO NNAR
set.seed(123)
modelo_NNAR <- nnetar(y = leche_ts_training,
                      p = 3, 
                      P = 1,
                      size = 3,
                      repeats = 50)
print(modelo_NNAR)
accuracy(modelo_NNAR)

## ESTRUCTURA DE LOS RESIDUOS DEL MODELO
checkresiduals(modelo_NNAR)
LjungBoxTest(na.omit(residuals(modelo_NNAR)),
             k=1,
             lag.max=12)
ad.test(residuals(modelo_NNAR))

auto_NNAR <- autoplot(resid(modelo_NNAR)) +
  xlab("Tiempo") +
  ylab("Residuos") +
  geom_point() +
  theme(plot.margin = unit(c(0.25, 0.5, 0, 0.5), "cm"))

FAC_NNAR <- ggAcf(resid(modelo_NNAR),
                   type = "correlation",
                   lag.max = 15) +
  ggtitle("") +
  labs(x = "Rezago",
       y = "FACM") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0.5), "cm"))

hist_NNAR <- ggplot(resid(modelo_NNAR),
                     aes(x=resid(modelo_NNAR))) + 
  geom_histogram(color="darkblue",
                 fill="skyblue3",
                 bins = 10) +
  xlab("Residuos") +
  scale_x_continuous() +
  ylab("Densidad") +
  theme(plot.margin = unit(c(-1, 0.5, 0, 0), #arriba, derecha, abajo, izquierda
                           "cm"))

plot_grid(auto_NNAR,
          plot_grid(FAC_NNAR,
                    hist_NNAR,
                    align = 'hv'),
          ncol = 1,
          rel_heights = c(1, 1),
          align = 'h')


## PRONÓSTICO
pronostico_nnar <- forecast(modelo_NNAR,
                            h = 6,
                            PI = TRUE)

autoplot(pronostico_nnar) + 
  xlab("Tiempo") + 
  ylab("Producción de Leche [Ml]") + 
  ggtitle("") 

## MAPE PARA LOS PRONÓSTICOS
leche_ts_mape <- leche_completa_ts[49:54]
leche_nnar_mape <- as.numeric(pronostico_nnar$mean)
MAPE_leche_nnar <- rep(0,6)
for (h in 1:6) {
  MAPE_leche_nnar[h] <- sqrt(sum(abs(leche_ts_mape[1:h] - leche_nnar_mape[1:h])/abs(leche_ts_mape[1:h]))/h)
}
MAPE_leche_nnar




#############################
## COMPARACIÓN PRONÓSTICOS ##
#############################

## RECM
modelo_sarima <- arima(leche_ts,
                       order = c(2, 0, 0),
                       seasonal = list(order = c(1, 1, 0),
                                       period = 12))
pronostico_sarima <- forecast(modelo_sarima,
                              h = 6)
leche_sarima <- as.numeric(pronostico_sarima$mean)

RMSE_VAR <- rep(0,6)
RMSE_NNAR <- rep(0,6)
RMSE_SARIMA <- rep(0,6)
for (h in 1:6) {
  RMSE_VAR[h] <- sqrt(sum((leche_pron_mape[1:h] - leche_ts_mape[1:h])^2)/h)
  RMSE_NNAR[h] <- sqrt(sum((leche_nnar_mape[1:h] - leche_ts_mape[1:h])^2)/h)
  RMSE_SARIMA[h] <- sqrt(sum((leche_sarima[1:h] - leche_ts_mape[1:h])^2)/h)
}

RMSE_VAR
RMSE_NNAR
RMSE_SARIMA
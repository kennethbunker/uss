###################################################
# R SCRIPT: SOLEMNE 3
# por       : Kenneth Bunker
# alumnos   : Constanza Ibarra
# ramo      : Estadística II
# trabajo   : 
# permalink : https://github.com/kennethbunker/uss/tree/main/estadistica2/scripts
#
###################################################

## Limpiar el caché (eliminar variables guardadas anteriormente)
rm(list=ls())

###################################################
# BIBLIOTECA
# Cargar e instalar paquetes necesarios
###################################################

#install.packages("pacman")
#pacman::p_load(RCurl, ggplot2, car, dplyr, plyr, readxl, writexl, openxlsx, correlation, see)

library("RCurl")
library("ggplot2")
library("car")
library("dplyr")
library("plyr")
library("readxl")
library("writexl")
library("openxlsx")
library("correlation")
library("see")

###################################################
# CARGA DE DATOS VIA MANUAL
###################################################

## Establecer directorio de trabajo en tu computador
#setwd("")
#data <- read_xlsx("Dropbox/GitHub/uss/estadistica2/data/ibarra.xlsx")

###################################################
# CARGA DE DATOS VIA GITHUB
###################################################

# Importar de Github
github <- "https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/"

###################################################
# ABRIR DATOS (BORRAR SI NO USA)
# Cargar base de datos CSV desde Github
###################################################

data2 <- getURL(paste0(github,"latam.csv")) 
data <- read.csv(text = data2)

###################################################
# DEFINICIÓN DE VARIABLES / HIPOTESIS
# Define aquí tus variables usando formato "base$variable"
# Si las defines correctamente, no es necesario modificar nada después de este item
###################################################

## Variables para análisis (ajustar si es necesario)
vd  <- data$enpv_bn           # número de partidos políticos (Y)
vi1 <- data$dep_dm_1tier      # número de diputados - H1: a medida que aumenta el número de diputados (X1), aumenta el número de partidos políticos (Y)
vi2 <- data$pres_term         # años de mandato presidencial - H2: a medida que aumenta el número de años de mandato (X2), aumenta el número de partidos políticos (Y)
vi3 <- data$pres_power        # poder presidencial - H3: a medida que aumenta el poder presidencial (X3), disminuye el número de partidos políticos (Y)
vi4 <- data$const_instability # inestabilidad constitucional - H4: a medida que aumenta la inestabilidad constitucional (X4), aumenta el número de partidos (Y)

###################################################
# EXPLORAR DATOS (TABLAS)
# Estadísticos descriptivos (media, desviación estándar, mínimo y máximo)
###################################################

## Estadísticas variable dependiente
mean(vd, na.rm = T)
sd(vd, na.rm = T)
min(vd, na.rm = T)
max(vd, na.rm = T)

## Estadísticas variable independiente 1
mean(vi1, na.rm = T)
sd(vi1, na.rm = T)
min(vi1, na.rm = T)
max(vi1, na.rm = T)

## Estadísticas variable independiente 2
mean(vi2, na.rm = T)
sd(vi2, na.rm = T)
min(vi2, na.rm = T)
max(vi2, na.rm = T)

## Estadísticas variable independiente 3
mean(vi3, na.rm = T)
sd(vi3, na.rm = T)
min(vi3, na.rm = T)
max(vi3, na.rm = T)

## Estadísticas variable independiente 4
mean(vi4, na.rm = T)
sd(vi4, na.rm = T)
min(vi4, na.rm = T)
max(vi4, na.rm = T)

###################################################
# EXPLORAR DATOS (BOXPLOT)
# Gráficos tipo boxplot para identificar valores extremos (solo sirve con valores continuos--no dummy)
###################################################

boxplot(vd, main="Variable Dependiente")
boxplot(vi1, main="Variable Independiente 1")
boxplot(vi2, main="Variable Independiente 2")
boxplot(vi3, main="Variable Independiente 3")

###################################################
# EXPLORAR DATOS (DENSIDADES)
# Gráficos de densidad para evaluar la distribución
###################################################

dens1 <- density(vd, na.rm = T)
plot(dens1, main="Densidad: Variable Dependiente")

dens2 <- density(vi1, na.rm = T)
plot(dens2, main="Densidad: Variable Independiente 1")

dens3 <- density(vi2, na.rm = T)
plot(dens3, main="Densidad: Variable Independiente 2")

dens4 <- density(vi3, na.rm = T)
plot(dens4, main="Densidad: Variable Independiente 3")

###################################################
# CORRELACIÓN
# Calcular correlación y graficar relación entre variables
###################################################

# Correlación entre variables
cor(vd, vi1, use="pairwise.complete.obs")

# Gráfico de dispersión entre variables
plot(vi1, vd, 
     main="Relación entre Variables",
     ylab="Variable Dependiente",
     xlab="Variable Independiente")

# Gráfico de dispersión entre variables
plot(vi2, vd, 
     main="Relación entre Variables",
     ylab="Variable Dependiente",
     xlab="Variable Independiente")

# Gráfico de dispersión entre variables
plot(vi3, vd, 
     main="Relación entre Variables",
     ylab="Variable Dependiente",
     xlab="Variable Independiente")

###################################################
# REGRESIÓN: TABLA 2
# Modelos de regresión lineal simple y múltiple
###################################################

## Modelo 1: Regresión simple (vd ~ vi1)
modelo1 <- lm(vd ~ vi1)
summary(modelo1)
nobs(modelo1)

## Modelo 2: Regresión simple (vd ~ vi2)
modelo2 <- lm(vd ~ vi2)
summary(modelo2)
nobs(modelo2)

## Modelo 3: Regresión simple (vd ~ vi3)
modelo3 <- lm(vd ~ vi3)
summary(modelo3)
nobs(modelo3)

## Modelo 4: Regresión múltiple (vd ~ vi1 + vi3)
modelo4 <- lm(vd ~ vi4)
summary(modelo4)
nobs(modelo4)

###################################################
# REGRESIÓN: TABLA 3
# Modelos de regresión lineal simple y múltiple
###################################################

## Modelo 5: Regresión simple
modelo5 <- lm(vd ~ vi1 + vi2)
summary(modelo5)
nobs(modelo5)

## Modelo 6: Regresión múltiple
modelo6 <- lm(vd ~ vi1 + vi3)
summary(modelo6)
nobs(modelo6)

## Modelo 7: Regresión múltiple
modelo7 <- lm(vd ~ vi1 + vi2 + vi3)
summary(modelo7)
nobs(modelo7)

## Modelo 8: Regresión múltiple (
modelo8 <- lm(vd ~ vi1 + vi2 + vi3 + vi4)
summary(modelo8)
nobs(modelo8)

###################################################
# REGRESIÓN + LINEA DE TENDENCIA
# Gráficos con líneas de regresión ajustadas
###################################################

plot(vd ~ vi1, 
     main="Variable Dependiente vs Independiente 1",
     ylab="Variable Dependiente",
     xlab="Variable Independiente 1")
abline(modelo1, col="red")

plot(vd ~ vi2, 
     main="Variable Dependiente vs Independiente 2",
     ylab="Variable Dependiente",
     xlab="Variable Independiente 2")
abline(modelo2, col="red")

plot(vd ~ vi3, 
     main="Variable Dependiente vs Independiente 3",
     ylab="Variable Dependiente",
     xlab="Variable Independiente 3")
abline(modelo3, col="red")

# ###################################################
# OTROS EJEMPLOS: 1
# Gráficos de valores añadidos (partial regression plots)
###################################################

# car::avPlots(modelo1)

# ###################################################
# OTROS EJEMPLOS: 2
# Gráfico avanzado de correlación con paquete see
###################################################
# result <- cor_test(byn, "dep_dm", "enpv_bn")
# plot(result,
#      point = list(
#        aes = list(color = "dep_dm", size = "enpv_bn"),
#        alpha = 0.66
#      ),
#      smooth = list(color = "black", se = FALSE)
# ) +
#   see::theme_modern() +
#   see::scale_color_material_c(palette = "rainbow", guide = "none") +
#   scale_size_continuous(guide = "none")

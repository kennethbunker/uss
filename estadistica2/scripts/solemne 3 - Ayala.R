###################################################
# R SCRIPT: SOLEMNE 3
# por       : Kenneth Bunker
# contacto  : kenneth.bunker@uss.cl
# ramo      : Estadística II
# trabajo   : Martin Ayala
# script    : https://github.com/kennethbunker/uss/tree/main/estadistica2/scripts
# data      : https://github.com/kennethbunker/uss/tree/main/estadistica2/data
# nota      : datos fabricados para demostración de ejercicio estadístico
#           : no usar en otro contexto
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
#setwd("/ruta/a/tu/directorio/")
#data <- read_xlsx("Dropbox/GitHub/uss/estadistica2/data/droguett_sepulveda.xlsx")

###################################################
# CARGA DE DATOS VIA GITHUB
###################################################

# Importar de Github
github <- "https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/"

###################################################
# ABRIR DATOS 2025-1
# Cargar base de datos apellido.csv desde Github
###################################################

data2 <- getURL(paste0(github,"ayala.csv")) 
data <- read.csv(text = data2)

names(data)[1] <- "Country"

###################################################
# DEFINICIÓN DE VARIABLES + HIPOTESIS
# Define aquí tus variables usando formato "base$variable"
# Si las defines correctamente, no es necesario modificar nada después de este item
###################################################

## Variables para análisis (ajustar si es necesario)
vd  <- data$GDP_Growth_Percent        # variable dependiente. Crecimiento economico.
vi1 <- data$Gini_Index                # variable independiente 1. Índice de Gini (desigualdad). H1: A medida que aumenta el Gini, aumenta el crecimiento.
vi2 <- data$Access_to_Water_Percent   # variable independiente 2. Porcentaje acceso agua. H2. A medida que aumenta Acceso a Agua, aumenta el crecimiento.
vi3 <- data$Employment_Percent        # variable independiente 3. Tasa de empleo. H3. A medida que aumenta la tasa de empleo, aumenta el crecimiento.
vi4 <- data$Left_President            # variable independiente 4. Presidente de Izquierda. H4. Hay mayor crecimiento cuando presidente es de izquierda (Left_President=1), que cuando es de oto sector (Left_Izquierda=0).

###################################################
# EXPLORAR DATOS (TABLA 1)
# Estadísticos descriptivos (promedio, desviación estándar, mínimo y máximo)
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

# dens5 <- density(vi4, na.rm = T)
# plot(dens5, main="Densidad: Variable Independiente 3")

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

## Modelo 4: Regresión múltiple (vd ~ vi4)
modelo4 <- lm(vd ~ vi4)
summary(modelo4)
nobs(modelo4)

###################################################
# REGRESIÓN: TABLA 3
# Modelos de regresión lineal simple y múltiple
###################################################

## Modelo 5: Regresión simple
modelo5 <- lm(vd ~ vi1 + v2)
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

###################################################
# R SCRIPT: SOLEMNE 3
# por       : Kenneth Bunker
# alumnos   : Millaray Herrera - Barbara Navarro
# ramo      : Estadística II
# trabajo   : ¿Qué factores determinan la rotación ministerial en Chile desde 1990?
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
setwd("/Dropbox/GitHub/uss/estadistica2/data/")
data <- read_xlsx("../data/herrera_navarro.xlsx")

###################################################
# CARGA DE DATOS VIA GITHUB
###################################################

# Importar de Github
github <- "https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/"

###################################################
# ABRIR DATOS 2025-1
# Cargar base de datos apellido.csv desde Github
###################################################

data2 <- getURL(paste0(github,"herrera_navarro.csv")) 
data <- read.csv(text = data2)

###################################################
# DEFINICIÓN DE VARIABLES
# Define aquí tus variables usando formato "base$variable"
# Si las defines correctamente, no es necesario modificar nada después de este item
###################################################

## Variables para análisis (ajustar si es necesario)
vd  <- data$días              # total de días en el poder
vi1 <- data$edad              # años. H1: a medida que aumenta el número de años del ministro, aumenta el número de días en el poder
vi2 <- as.numeric(data$debut) # año de nombramiento del ministro. H2: a medida que aumenta el año de nombramiento, disminuye el númerod e días en el poder
vi3 <- data$experiencia       # experiencia política. H3: a medida que aumenta la experiencia política, aumenta el número de días en el poder
vi4 <- data$sexo              # sexo H4: los hombres durán más que las mujeres en el poder

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

boxplot(vd, main="Número de Días en el Poder")
boxplot(vi1, main="Variable Independiente 1")
boxplot(vi2, main="Variable Independiente 2")
boxplot(vi3, main="Variable Independiente 3")
#boxplot(vi4, main="Variable Independiente 4") # con dummy no va boxplot

###################################################
# EXPLORAR DATOS (DENSIDADES)
# Gráficos de densidad para evaluar la distribución
###################################################

dens1 <- density(vd, na.rm = T)
plot(dens1, main="Densidad: Número de días en el poder")

dens2 <- density(vi1, na.rm = T)
plot(dens2, main="Densidad: Variable Independiente 1")

dens3 <- density(vi2, na.rm = T)
plot(dens3, main="Densidad: Variable Independiente 2")

dens4 <- density(vi3, na.rm = T)
plot(dens4, main="Densidad: Variable Independiente 3")

#dens5 <- density(vi4, na.rm = T)
#plot(dens5, main="Densidad: Variable Independiente 3")

###################################################
# CORRELACIÓN
# Calcular correlación y graficar relación entre variables
###################################################

# Correlación entre variables
cor(vd, vi3, use="pairwise.complete.obs")

# Gráfico de dispersión entre variables
plot(vi1, vd, 
     main="Relación entre Variables",
     ylab="Variable Dependiente",
     xlab="Variable Independiente")

###################################################
# REGRESIÓN
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

## Modelo 4: Regresión simple (vd ~ vi4)
modelo4 <- lm(vd ~ vi4)
summary(modelo4)
nobs(modelo4)

## Modelo 5: Regresión múltiple (vd ~ vi1 + vi2)
modelo5 <- lm(vd ~ vi1 + vi2)
summary(modelo5)
nobs(modelo5)

## Modelo 5: Regresión múltiple (vd ~ vi1 + vi2)
modelo5 <- lm(vd ~ vi1 + vi2)
summary(modelo5)
nobs(modelo5)

## Modelo 6: Regresión múltiple (vd ~ vi1 + vi2 + vi3)
modelo6 <- lm(vd ~ vi1 + vi2 + vi3)
summary(modelo6)
nobs(modelo6)

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

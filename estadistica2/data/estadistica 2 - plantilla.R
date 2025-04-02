###################################################
# R SCRIPT: SOLEMNE 3
# por       : Kenneth Bunker
# contacto  : kenneth.bunker@uss.cl
# ramo      : Estadística II
# trabajo   : Nombre de Trabajo
#           
#           
#
#
#
###################################################

## Limpiar el caché
rm(list=ls())

Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

###################################################
# DIRECTORIO DE TRABAJO
###################################################

## CAMBIAR A TU COMPUTADOR
#setwd("/Users/kennethbunker/Dropbox/USS/clases/2025-01/estadistica 2/solemne 3/")

###################################################
# BIBLIOTECA
###################################################

install.packages("pacman")
pacman::p_load(RCurl, ggplot2, ggplot2, car,
               dplyr, plyr,readxl,writexl,openxlsx,
               correlation,see,ggplot2)

###################################################
# ABRIR DATOS BUNKER Y NEGRETTO
#
#
###################################################

x <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/latam.csv") 
byn <- read.csv(text = x)

###################################################
# ABRIR DATOS V-DEM
#
#
###################################################

xx <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/vdem.csv") 
vdem <- read.csv(text = xx)

###################################################
# ABRIR DATOS POLARIZACION
#
#
###################################################

xxx <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/polarizacion.csv") 
polarizacion <- read.csv(text = xxx, encoding="UTF-8")

###################################################
# LIMPIAR BASE
#   # sacar filas que tienen NA
#   # 
#
###################################################

polarizacion <- drop_na(polarizacion)

###################################################
# DEFINICIÓN DE VARIABLES <------------------------------------------- ingresa tus variables aquí!!!
#   # elegir base y variables [ejemplo: "base$variable"]
#   # si están bien ingresadas, no hay que tocar nada abajo
#
###################################################

## asignar nombre genérico a mis variables
vd  <- polarizacion$boric_1v2021
vi1 <- polarizacion$coord1D_normal_all
vi2 <- polarizacion$edad
vi3 <- polarizacion$magnitud_distrital

###################################################
# EXPLORAR DATOS (TABLAS)
#
#
###################################################

## mi variable dependiente
mean(vd, na.rm = T)
sd(vd, na.rm = T)
min(vd, na.rm = T)
max(vd, na.rm = T)

## mi variable independiente 1
mean(vi1, na.rm = T)
sd(vi1, na.rm = T)
min(vi1, na.rm = T)
max(vi1, na.rm = T)

## mi variable independiente 2
mean(vi2, na.rm = T)
sd(vi2, na.rm = T)
min(vi2, na.rm = T)
max(vi2, na.rm = T)

## mi variable independiente 3
mean(vi3, na.rm = T)
sd(vi3, na.rm = T)
min(vi3, na.rm = T)
max(vi3, na.rm = T)

###################################################
# EXPLORAR DATOS (BOXPLOT)
#
#
###################################################

## boxplot
boxplot(vd, main="Variable Dependiente")
boxplot(vi1, main="Variable Independiente 1")
boxplot(vi2, main="Variable Independiente 2")
boxplot(vi3, main="Variable Independiente 3")

###################################################
# EXPLORAR DATOS (DENSIDADES)
#
#
###################################################

## distribuciones de densidades
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
#
#
###################################################

# esta es mi primera correlación
cor(vd, vi1, use="pairwise.complete.obs")

# este es mi primer gráfico de asociación
plot(vi1, vd, 
     main="Relación entre Variables",
     ylab="Nombre Variable Dependiente",
     xlab="Nombre Variable Independiente")

###################################################
# REGRESIÓN
#
#
###################################################

## este es mi modelo 1 (tiene la principal variable independiente)
modelo1 <- lm(vd ~ vi1)
summary(modelo1)
nobs(modelo1) # nobs = numero de observaciones = N

## este es mi modelo 2 (tiene una variable independiente)
modelo2 <- lm(vd ~ vi2)
summary(modelo2)
nobs(modelo2) # nobs = numero de observaciones = N

## este es mi modelo 3 (tiene otra variable independiente)
modelo3 <- lm(vd ~ vi3)
summary(modelo3)
nobs(modelo3) # nobs = numero de observaciones = N

## este es mi modelo 4 (tiene dos variables independientes)
modelo4 <- lm(vd ~ vi1 + vi3)
summary(modelo4)
nobs(modelo4) # nobs = numero de observaciones = N

## este es mi modelo 5 (tiene dos variables independientes)
modelo5 <- lm(vd ~ vi1 + vi2)
summary(modelo5)
nobs(modelo5) # nobs = numero de observaciones = N

## este es mi modelo 6 (tiene tres variables independientes)
modelo6 <- lm(vd ~ vi1 + vi2 + vi3)
summary(modelo6)
nobs(modelo6) # nobs = numero de observaciones = N

###################################################
# REGRESIÓN + LINEA DE TENDENCIA
#
#
###################################################

plot(vd ~ vi1, 
     main="Título del Gráfico",
     ylab="Nombre Variable Dependiente",
     xlab="Nombre Variable Independiente")
abline(modelo1, col="red")

plot(vd ~ vi2, 
     main="Título del Gráfico",
     ylab="Nombre Variable Dependiente",
     xlab="Nombre Variable Independiente")
abline(modelo2, col="red")

plot(vd ~ vi3, 
     main="Título del Gráfico",
     ylab="Nombre Variable Dependiente",
     xlab="Nombre Variable Independiente")
abline(modelo3, col="red")


# ###################################################
# # OTROS EJEMPLOS: 1
# #
# #
# ###################################################
# 
# car::avPlots(modelo1)
# 
# ###################################################
# # OTROS EJEMPLOS: 2
# # # hay que definir base y variables
# #
# ###################################################
# 
# ## una correlación de arcoiris
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
# 
# ###################################################
# # OTROS EJEMPLOS: 3
# #
# #
# ###################################################
# 
# 
# 
# 

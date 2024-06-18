###################################################
# R SCRIPT: SOLEMNE 3
# por       : Kenneth Bunker
# contacto  : kenneth.bunker@uss.cl
# trabajo   : Nombre de Trabajo
#           
#           
#
#
#
###################################################

## Limpiar el caché
rm(list=ls())

###################################################
# DIRECTORIO DE TRABAJO
###################################################

## CAMBIAR A TU COMPUTADOR
#setwd("/Users/kennethbunker/Dropbox/USS/clases/2024-1/estadistica 2/solemne 3/")

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
polarizacion <- read.csv(text = xxx)

###################################################
# DEFINICIÓN DE VARIABLES <------------------------------------------- ingresa tus variables aquí!!!
#   # elegir base y variables [ejemplo: "base$variable"]
#   # si están bien ingresadas, no hay que tocar nada abajo
#
###################################################

## asignar nombre genérico a mis variables
vd  <- byn$enpv
vi1 <- byn$pres_power
vi2 <- byn$time
vi3 <- byn$number

###################################################
# EXPLORAR DATOS (TABLAS)
#
#
###################################################

## mi variable dependiente
mean(vd)
sd(vd)
min(vd)
max(vd)

## mi variable independiente 1
mean(vi1)
sd(vi1)
min(vi1)
max(vi1)

## mi variable independiente 2
mean(vi2)
sd(vi2)
min(vi2)
max(vi2)

## mi variable independiente 3
mean(vi3)
sd(vi3)
min(vi3)
max(vi3)

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
dens1 <- density(vd)
plot(dens1, main="Densidad: Variable Dependiente")

dens2 <- density(vi1)
plot(dens2, main="Densidad: Variable Independiente 1")

dens3 <- density(vi2)
plot(dens3, main="Densidad: Variable Independiente 2")

dens4 <- density(vi3)
plot(dens4, main="Densidad: Variable Independiente 3")

###################################################
# CORRELACIÓN
#
#
###################################################

# esta es mi primera correlación
cor(vd, vi1)

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

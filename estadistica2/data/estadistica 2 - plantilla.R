###################################################
# R SCRIPT: SOLEMNE 3
# por       : Kenneth Bunker
# contacto  : kenneth.bunker@uss.cl
# ramo      : Estadística II
# trabajo   : Nombre de Trabajo
# permalink : https://github.com/kennethbunker/uss/tree/main/estadistica2/
###################################################

## Limpiar el caché (eliminar variables guardadas anteriormente)
rm(list=ls())

###################################################
# DIRECTORIO DE TRABAJO
###################################################

## Establecer directorio de trabajo en tu computador
#setwd("/ruta/a/tu/directorio/")

###################################################
# BIBLIOTECA
# Cargar e instalar paquetes necesarios
###################################################

install.packages("pacman")
pacman::p_load(RCurl, ggplot2, car,
               dplyr, plyr, readxl, writexl, openxlsx,
               correlation, see)

###################################################
# ABRIR DATOS BUNKER Y NEGRETTO
# Cargar base de datos latam.csv desde Github
###################################################

data <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/latam.csv") 
byn <- read.csv(text = data)

###################################################
# ABRIR DATOS V-DEM
# Cargar base de datos vdem.csv desde Github
###################################################

data <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/vdem.csv") 
vdem <- read.csv(text = data)

###################################################
# ABRIR DATOS POLARIZACION
# Cargar base de datos polarizacion.csv desde Github
###################################################

data <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/polarizacion.csv") 
polarizacion <- read.csv(text = data, encoding="UTF-8")

###################################################
# ABRIR DATOS 2025-1
# Cargar base de datos apellido.csv desde Github
###################################################

data <- getURL("https://raw.githubusercontent.com/kennethbunker/uss/main/estadistica2/data/perez_rojas.csv") 
perez_rojas <- read.csv(text = data)

###################################################
# LIMPIAR BASE
# Remover filas que tienen valores faltantes (NA)
###################################################

#polarizacion <- drop_na(polarizacion)

###################################################
# DEFINICIÓN DE VARIABLES
# Define aquí tus variables usando formato "base$variable"
# Si las defines correctamente, no es necesario modificar abajo
###################################################

## Variables para análisis (ajustar si es necesario)
vd  <- perez_rojas$homicide                 # variable dependiente
vi1 <- perez_rojas$govstab           # variable independiente 1
vi2 <- perez_rojas$subexp                         # variable independiente 2
vi3 <- perez_rojas$corrup           # variable independiente 3

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

###################################################
# EXPLORAR DATOS (BOXPLOT)
# Gráficos tipo boxplot para identificar valores extremos
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

## Modelo 4: Regresión múltiple (vd ~ vi1 + vi3)
modelo4 <- lm(vd ~ vi1 + vi3)
summary(modelo4)
nobs(modelo4)

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

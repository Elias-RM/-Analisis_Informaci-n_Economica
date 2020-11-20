# Curso de analisis de informacion economica: 
# Replica 1: lectura de base de datos PEA (2005-2019)
# Nombre: Reyes Martinez Elias
# Fecha: 15/10/2020

# elegir subdirectirio de trabajo
setwd("~/analisis de informacion economica/replicas/replica 1")

#Activar librerias
library(stats)

#lectura de archivos csv
read.csv("PEA.csv")

#lectura de base de datos

PEA <- read.csv("PEA.csv")

# vizualizar objeto con base de datos
View(PEA)

# graficas de variables

# pea total por trimestre
plot(PEA$pea_total)
plot(PEA$pea_total, type = "line")


# pea ocupada trimestral
plot(PEA$pea_ocup)# con puntos
plot(PEA$pea_ocup, type = "line") # con linea

# pea desocupada trimestral
plot(PEA$pea_desocup)
plot(PEA$pea_desocup, type = "line")

# Identificar variables de series de tiempo
pea_total <- ts (PEA$pea_total, frequency = 4, start = c(2005,1))
pea_ocupada <- ts(PEA$pea_ocup, frequency = 4, start = c(2005,1))
pea_desocupada <- ts(PEA$pea_desocup, frequency = 4, start = c(2005,1))


# grafica de serie de tiempo
plot(pea_total, type="line", main="PEA total", col="darkblue")
plot(pea_ocupada, type="line", main="PEA ocupada", col="violet")
plot(pea_desocupada, type="line", main="PEA desocupada", col="purple")

# poner en una grafica mas variables
plot(pea_total, type="line", main="PEA Total ocupada", col="green")
lines(pea_ocupada, col="red")








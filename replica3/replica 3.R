# Curso de analisis de Informacion Economica
# Reyes Martinez Elias
# 29/10/2020
# Replica sesion 3

# Activar libreria
library(stats)
library(readxl)

# Elegir directorio de trabajo
setwd("~/analisis de informacion economica/replicas/replica 3")

# Lectura de base de datos
Bdatos <- read_xls("igae.xls", sheet = "Bdatos")

# vizualizar datos
View(Bdatos)

# memoria de base de datos
attach(Bdatos)

# Identificar series de tiempo
igae_total <- ts(igae_tot, frequency = 12, start = c(2003,1))
igae_primario <- ts(igae_prim, frequency = 12, start = c(2003,1))
igae_secundario <- ts(igae_secu, frequency = 12, start = c(2003,1))
igae_terciario <- ts(igae_terc, frequency = 12, start = c(2003,1))

# tasas de crecimiento
# igae total
igae_mens <- diff(igae_total,1)/lag(igae_total,1)*100
igae_anual <- diff(igae_total,12)/lag(igae_total,12)*100

# igae primario
igae_prim_mens <- diff(igae_primario,1)/lag(igae_primario,1)*100
igae_prim_anual <- diff(igae_primario,12)/lag(igae_primario,12)*100

# igae secundario
igae_secu_mens <- diff(igae_secundario,1)/lag(igae_secundario,1)*100
igae_secu_anual <- diff(igae_secundario,12)/lag(igae_secundario,12)*100

# igae terciario
igae_terc_mens <- diff(igae_terciario,1)/lag(igae_terciario,1)*100
igae_terc_anual <- diff(igae_terciario,12)/lag(igae_terciario,12)*100

# grafica de igae mensual
plot(igae_mens, type="line", main="IGAE Mensual", col="black")
lines(igae_prim_mens, col="blue")
lines(igae_secu_mens, col="red")
lines(igae_terc_mens, col="green")


# grafica de igae interanual
plot(igae_anual, type="line", main="IGAE Anual", col="black")
lines(igae_prim_anual, col="orange")
lines(igae_secu_anual, col="red")
lines(igae_terc_anual, col="darkblue")


# limites en las graficas
summary(igae_prim_mens)
summary(igae_prim_anual)

# grafica con cambios de limites igae mensual
#lty son opciones de linea de la grafica
plot(igae_mens, type="line", main="Igae mensual", col="black", ylim=c(-50,50))
lines(igae_prim_mens, col="blue", lty=2)
lines(igae_secu_mens, col="red", lty=3)
lines(igae_terc_mens, col="green", lty=1)

# grafica con cambios de limites igae mensual inter-anual
plot(igae_anual, type="line", main="IGAE anual", col="darkblue", ylim=c(-14,20))
lines(igae_prim_anual, col="red", lty=2)
lines(igae_secu_anual, col="green", lty=3)
lines(igae_terc_anual, col="gray", lty=1)


# le agregamos titulo al eje y
plot(igae_anual, type="line", main="Igae anual 2003-2020", 
     col="blue", ylim= c(-12,8), ylab="tasas en tanto por ciento")
lines(igae_prim_anual, col="blue", lty=4)
lines(igae_secu_anual, col="blue", lty=5)
lines(igae_terc_anual, col="blue", lty=6)

# Le agregamos titulo al eje X
plot(igae_anual, type="line", main="Igae mensual inter-anual 2003-2020", 
     col="blue", ylim= c(-12,12), ylab="tasas en tanto por ciento",
     xlab="Periodo 2003-2020")
lines(igae_prim_anual, col="blue", lty=4)
lines(igae_secu_anual, col="blue", lty=5)
lines(igae_terc_anual, col="blue", lty=6)


# le agregamos dos lineas horizontales con el comando abline
plot(igae_anual, type="line", main="Igae mensual inter-anual 2003-2020", 
     col="blue", ylim= c(-12,12), ylab="tasas en tanto por ciento",
     xlab="Periodo 2003-2020")
lines(igae_prim_anual, col="blue", lty=4)
lines(igae_secu_anual, col="blue", lty=5)
lines(igae_terc_anual, col="blue", lty=6)
abline(h=4, col="red")
abline(h=2, col="red")













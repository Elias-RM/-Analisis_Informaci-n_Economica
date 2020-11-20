# Curso de "Analisis de Indicadores Economicos"
# Replica 5: APIs y Tendencias de Poblaciom Ocupada
# Facultad de Economia, UNAM
# Reyes Martinez Elias, UNAM (2020) 

# Activar librerias
library(stats)
library(inegiR)

# Directorio de trabajo 
getwd()

# token Indicadores
token_indica <- "ee89beaf-6633-b24b-8746-9f6372674102" # sustituir tu token por la clave que recibiste 

# Poblacion Ocupada
pobocup_id <- "289245"
BDatos <- inegi_series(pobocup_id,token_indica)
View(BDatos)
# Grafica de tendencia de la poblacion ocupada
plot(BDatos$values, type="l")
# la grafica nos diria que la pendiente es negativa
# visto economicamente la grafica dice que a traves del tiempo el numero de personas ocupadas va decreciendo 
# sin embargo Inegi acomodo los datos del mas reciente al mas antiguo por lo que, 
# hay que aplicar un ordenamineto a la serie 

# identificar y ordenar la series 
# [with(BDatos, order(BDatos$date)),] lo esta condicionando
pobocu <- ts(BDatos[with(BDatos, order(BDatos$date)),]$values, frequency=4, start=c(2005,1))

# Grafica de tendencia CORRECTA del iage
plot(pobocu)

# Transformaci??n de indicadores
lnpobocu <- log(pobocu)

# Variable de tiempo y al cuadrado
ten <- time(BDatos$date)
ten2 <- ten^2
View(ten2)

# Grafica de indicadores 
plot(pobocu, type="l",main = "Poblacion Ocupada", col="black") 

plot(lnpobocu, type="l",main = "Poblacion Ocupada", col="black") 

# Tendencia lineal
# lm es para aplicar minimos cuadrados ordinarios
# ~ indica cual es la variable exgone la cual es el tiempo en este ejemplo
mod_lin <- lm(pobocu ~ ten)
summary(mod_lin)
# B1 es estadisticamente diferente de cero



# Curso de "Analisis de Indicadores Economicos"
# Replica 6(5): APIs y Tendencias de Poblaciom Ocupada continuacion
# Facultad de Economia, UNAM
# Reyes Martinez Elias, UNAM (2020) 
# 19/11/2020

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
# Grafica de tendencia de la Poblacion ocupada
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

# Tendencia cuadratica
# + añade una variable exogena
mod_cua <- lm(pobocu ~ ten+ten2)
summary(mod_cua)
# B1 y B2 estadisticamente diferente de cero
# la especificacion cauadratica tambien se utiliza para establecer tendencia
# Signo positivo, lo cual es un minimo 

# Tendencia exponencial
mod_exp <- lm(log(pobocu) ~ ten)
summary(mod_exp)
#lnBo estadisticamente diferente de cero
# Yt tambien puede ser representado por una especificacion exponencial

# si tuviesemos que elgir un modelo para este ejemplo seria el cuadratico
# porque tiene un R2 mal alto

# Prediccion de tendencias 
#transformacion a series de tiempo
# R utiliza la parte estructural del modelo con la funcion predict
ten_lin <- ts(predict(mod_lin), frequency= 4, start=c(2005,1))
ten_cua <- ts(predict(mod_cua), frequency= 4, start=c(2005,1))
ten_exp <- ts(exp(predict(mod_exp)), frequency= 4, start=c(2005,1))

plot(pobocu, main = "Tendencias de la poblacion Ocupada", col="darkgoldenrod")
lines(ten_lin, col="purple4")
lines(ten_cua, col = "skyblue")
lines(ten_exp, col = "red")
colors()
options(scipen = 999)


# Residuales de los modelos
# resid= errores de la funcion
u_lin <- ts(resid(mod_lin), frequency= 4, start=c(2005,1))
u_cua <- ts(resid(mod_cua), frequency= 4, start=c(2005,1))
u_exp <- ts(resid(mod_exp), frequency= 4, start=c(2005,1))

# Graficas individuales de variable sin tendencias
plot(u_lin, main = "Variable sin tendencia lineal", type="l", col="violetred3")
# alrededor de un valor constante 
plot(u_cua, main = "Variable sin tendencia cuadratica", type="l", col="sienna2")
plot(u_exp, main = "Variable sin tendencia exponencial", type="l", col= "orangered3")

# Grafica con la comparacion de variable sin tendencias
plot(u_lin, main = "Variables sin tendencias", col="darkviolet")
lines(u_cua, col ="green")
lines(u_exp, col ="red")

# Normalizar las series sin tendencias para comparar
u_lin_nor <- (u_lin-mean(u_lin))/sd(u_lin)
u_cua_nor <- (u_cua-mean(u_cua))/sd(u_cua)
u_exp_nor <- (u_exp-mean(u_exp))/sd(u_exp)

# Grafica con la compraci??n de variables sin tendencias normalizadas
plot(u_lin_nor, main = "Variables sin tendencias normalizadas", col="darkviolet")
lines(u_cua_nor, col ="green")
lines(u_exp_nor, col ="red")
# los errores de la tres estimaciones me dan comportamientos muy parecidos
# una variable sin tendencia, la variable esta alrededor de una constante



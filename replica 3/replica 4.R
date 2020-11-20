# Curso de "Analisis de Indicadores Económicos"
# Replica 4: Ejemplo Micro "Gasolina Magna"
# Facultad de Economia, UNAM
# Reyes Martinez Elias, UNAM (2020) 

# Activar librerias
library(stats)
library(readxl)

# Directorio de trabajo 

getwd()

# Lectura de la base de datos
BDatos <- read_xls("BDatos_r4.xls", sheet="Bdatos")

.# Vizualizar datos 
View(BDatos)

# Activar en memoria la base de datos
attach(BDatos)

# identificar series de tiempo
vol <- ts(volumen, frequency=12, start=c(2013,1))
valor <- ts(valor, frequency=12, start=c(2013,1))
inpc <- ts(inpc, frequency=12, start=c(2013,1))
igae <- ts(igae, frequency=12, start=c(2013,1))

# Construccion de indicadores
# valor = P*Q
# volumen= Q
# precio = (Q*P)/Q
p <- valor/vol
q <- vol
# relacion entre cantidad y precio del producto
View(q)
View(p)
# Grafica de series   
plot(q, type="l")
plot(p, col="red")
plot(inpc, col="green")
plot(igae, col="blue")

# Diagramas de dispersion 
# precio relativo = p/inpc

plot(q,p)
plot(q, p/inpc)
plot(q,igae)

# Relaciones no-lineales (aplicacion de logaritmos)
plot(log(q),log(p))
plot(log(q), log(p/inpc))
plot(log(q),log(igae))

# Transformación de variables originales
pr <- (p/inpc)
lnpr <- log(pr)
lnq <- log(q)
lny <- log(igae)

# la info de cantidad esta en miles de unidades


# Grafica de indicadores 
plot(q/10000, type="l",
     main = "Cantidad y precio relativo de Gasolina Magna",
     col="red", ylim=c(0,400)) 
lines(pr*100, col="black")

summary(log(q))
summary(log(pr))
plot(log(q), type="l",
     main = "logaritmos de cantidad y precio relativo de Gasolina magna",
     col="red", ylim=c(-3,15)) 
lines(log(pr), col="black")


# Diagramas de dispersion 
# Relación cantidad y precio
scatter.smooth(q,pr)
scatter.smooth(log(q),log(pr))

# Relacion cantidad e ingreso (igae)
# suaviza el movimiento
scatter.smooth(igae, q)
scatter.smooth(log(igae),log(q))

# valores de referencia
View(pr)
View(i_pr)
View(q)
View(i_q)
View(igae)
View(i_y)

# construccion de indice
i_pr <- (pr/0.093621160)*100
i_q <- (q/825239)*100
i_y <- (igae/111.43486)*100

# grafica de indices
plot(i_q, type="l",
     main = "Cantidad y precio relativo de gasolina magna e ingreo nacional
     indices enero de 2020= 100",
     col="red", ylim=c(0,150), xlim=c(2019,2020)) 
lines(i_pr, col="black")
lines(i_y, col="green")
# caida del ingreso, caida del consumo de gasolina magna y caida del precio relativo
# la demada cayo porque hubo confinamiento parcial en casi todo el pais


# gaussian = distribucion normal
# se utilizan otras como poisson, bernoulli, exponencial
# grafica de dispersion en terminos de indices
scatter.smooth(log(i_q), log(i_pr), 
               main = "Diagrama de dispersion entre cantidad y precio relativo de gasolina magna",
               col="red", xlab="logartimo natural de cantidad (lnq)",
               ylab="logaritmo natural  del precio relativo (lnpr)", family="gaussian")


scatter.smooth(log(i_y), log(i_q), 
               main = "Diagrama de dispersion entre cantidad de demanda de gasolina magna
               e ingreso",
               col="blue", xlab="logartimo natural del ingreso (lny)",
               ylab="logaritmo natural  de la cantidad demandada (lnq)", family="gaussian")
# no nos dice que tan fuerte es la relacion

# Pruebas de hipótesis coeficentes de correlación
# Para saber si la relacion es estadisticamente igaul a cero o diferente de cero
# Q, Pr, Q, Y
# Hn: p=0 no existe relacion entre las variables
# Ha:p/= 0 existe relacion entre las variables
# queremos que el coeficiente p sea estadisticamente diferente de cero
# a< 0.05 ha
# a> 0.05 hn
cor.test(q, pr)
cor.test(log(q),log(pr))
# no correlacion entre la cantidad demandada de gasolina magna y el precio
cor.test(q,igae)
cor.test(log(q),log(igae))
# Hay relacion negativa entre la cantidad y el ingreaso


# pruebas con indices
cor.test(i_q,i_pr)
cor.test(log(i_q),log(i_pr))


cor.test(i_q, i_y)
cor.test(log(i_q), log(i_y))

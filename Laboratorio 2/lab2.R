# Laboratorio 2 - CO3321
# Sección 1
# Andrea Reyes 15-11201
# Javier Medina 12-10400

carne = read.table("zonas_carne.txt", header=T)

zona1 = carne$Zona1
zona1_guisar = zona1[carne$Tipo1 == "Guisar"]
zona2 = carne$Zona2
zona2_guisar = zona2[carne$Tipo2 == "Guisar"]
zona3 = carne$Zona3
zona3_guisar = zona3[carne$Tipo3 == "Guisar"]
zona4 = carne$Zona4
zona4_guisar = zona4[carne$Tipo4 == "Guisar"]

# PREGUNTA 1
# Realice un análisis descriptivo de los precios de las zonas del tipo de carne de guisar. 
# En el estudio considere histograma, boxplot y una tabla con información 
# del mı́nimo, cuartiles, media y desviación estándar de cada zona (10 puntos).

hist(zona1_guisar, main = "Histograma de precios de carne guisar en Zona 1", ylab = "Abastecimientos", xlab = "Precios en Zona 1")
hist(zona2_guisar, main = "Histograma de precios de carne guisar en Zona 2", ylab = "Abastecimientos", xlab = "Precios en Zona 2")
hist(zona3_guisar, main = "Histograma de precios de carne guisar en Zona 3", ylab = "Abastecimientos", xlab = "Precios en Zona 3")
hist(zona4_guisar, main = "Histograma de precios de carne guisar en Zona 4", ylab = "Abastecimientos", xlab = "Precios en Zona 4")

# Boxplot con las 4 Zonas
boxplot(zona1_guisar,zona2_guisar,zona3_guisar,zona4_guisar, ylab="Precios", xlab="Zonas",col=c("red","pink","gray","yellow"))

# Boxplot ZONA 1

boxplot(zona1_guisar, ylab="Precios", xlab="Zona 1", col=c("red"))

# Boxplot ZONA 2

boxplot(zona2_guisar, ylab="Precios", xlab="Zona 2", col=c("pink"))

# Boxplot ZONA 3

boxplot(zona3_guisar, ylab="Precios", xlab="Zona 3", col=c("gray"))

# Boxplot ZONA 4

boxplot(zona4_guisar, ylab="Precios", xlab="Zona 4", col=c("yellow"))

# Tabla

D = data.frame(Zonas =c('Zona1','Zona2','Zona3','Zona4') ,
               Minimos =c(min(zona1),min(zona2),min(zona3),min(zona4)) , 
               Q1 =c(quantile(zona1,0.25),quantile(zona2,0.25),quantile(zona3,0.25),quantile(zona4,0.25)) ,
               Q2 =c(quantile(zona1,0.50),quantile(zona2,0.50),quantile(zona3,0.50),quantile(zona4,0.50)) ,
               Q3 =c(quantile(zona1,0.75),quantile(zona2,0.75),quantile(zona3,0.75),quantile(zona4,0.75)) ,
               Media =c(mean(zona1),mean(zona2),mean(zona3),mean(zona4)),
               Desviacion =c(sd(zona1),sd(zona2),sd(zona3),sd(zona4)),
               stringsAsFactors = FALSE )

######################

# PREGUNTA 2
# Determine un intervalo de confianza para la media de los precios de la carne de guisar 
# para la Zona 1 y así mismo, para los precios de la Zona 3. (confianza de 99 %).

# Intervalo de confianza Zona 1
mean(zona1_guisar)
t.test(zona1_guisar, conf.level = 0.99 )$conf.int

# Intervalo de confianza Zona 2
mean(zona3_guisar)
t.test(zona3_guisar, conf.level = 0.99 )$conf.int

######################

# PREGUNTA 3
# ¿Cuál de los dos alimentos logra mayor precio en la carne de guisar, la Zona 2 o la 
# Zona 4? Suponga 1 − α = 0,99

# Verificamos que las varianzas son iguales
var.test(zona2_guisar, zona4_guisar)$conf.int

# Intervalo de diferencia de medias
t.test (zona4_guisar, zona2_guisar, var.equal = T, conf.level = 0.99 )$conf.int

######################

# PREGUNTA 4
# Los investigadores presumen que la diferencia del gasto promedio en carne de la zona 2
# y 4 es menor a 1$. ¿Es esta afirmación cierta?

# H0: mu4 == mu2 vs. Ha: mu4 - mu2 < 1
# alpha = 0.01
t.test(zona4, zona2, alternative = "less", mu = 1, conf.level = 0.99 )

######################

# PREGUNTA 5
# Calcule un intervalo de confianza al 90% para estimar la proporción de precios mayores
# a 300$ de la Zona 3

size_z3 = length(zona3) # Tamaño del vector Zona3
zona3_precios = zona3[carne$Zona3 > 300]
size_z3p = length(zona3_precios) # Tamaño del vector Zona3 filtrado por precios mayores a 300$
binom.test(size_z3p, size_z3, conf.level = 0.90)$conf.int

######################

# PREGUNTA 6
# Una semana después se volvió a realizar la encuesta en la Zona 1, en los mismos 
# abastecimientos. La información se encuentra en la columna Zona1B. El investigador 
# presume que los precios han aumentado en dicha zona con relación a la semana anterior. 
# ¿Es esta información correcta?

p1b = mean(carne$Zona3B)
p1 = mean(zona1)
#Tomamos como hipotesis nula que p1b = p1
#Tomamos como hipotesis alternativa que p1b > p1

#Tomando alfa como 5%, calculamos

t.test( carne$Zona3B, alternative = "greater", mu = p1, conf.level = 0.95 )



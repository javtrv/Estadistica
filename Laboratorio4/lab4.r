# Laboratorio 4 - CO3321
# Sección 1
# Andrea Reyes 15-11201
# Javier Medina 12-10400

#####################################################################################

# 1.1 (20 puntos) Realice un análisis descriptivo de los datos (histograma, gráfico de cajas, número de la
# muestra, mínimo, cuartiles, media y desviación).

# Se cargan los datos
data(state)
edo.77<- as.data.frame(state.x77)
names(edo.77)[4] <- "Life.Exp"
names(edo.77)[6] <- "HS.Grad"

# Mostramos los datos
edo.77
View(edo.77)

#• Population: estimación de población al 1 de julio de 1975.
#• Income: renta per capita (1974).
#• Illiteracy: analfabetismo (1970, porcentaje de la población).
#• Life.Exp: esperanza de vida en años (1969–71)
#• Murder: asesinato y tasa de homicidio no negligente por cada 100.000 habitantes (1976)
#• HS Grad: porcentaje de graduados de escuela secundaria (1970).
#• Frost: número promedio de días con temperatura mínima por debajo del punto de congelación
#(1931–1960) en la capital o ciudad grande.
#• Area: área de tierra en millas cuadrad

# Realizamos los histogramas de las variables
hist(edo.77$Population, main = "Poblacion estimada al 01/07/1975", ylab = "Estados", xlab = "Cantidad de personas")
hist(edo.77$Income, main = "Renta per capita (1974)", ylab = "Estados", xlab = "Renta")
hist(edo.77$Illiteracy, main = "Analfabetismo (1970, porcentaje de la población)", ylab = "Estados", xlab = "Porcentaje de Poblacion Analfabeta")
hist(edo.77$Life.Exp, main = "Esperanza de vida en años (1969–71)", ylab = "Estados", xlab = "Años")
hist(edo.77$Murder, main = "Asesinato y tasa de homicidio no negligente por cada 100.000 habitantes (1976)", ylab = "Estados", xlab = "Tasa")
hist(edo.77$HS.Grad, main = "Porcentaje de graduados de escuela secundaria (1970)", ylab = "Estados", xlab = "Graduados")
hist(edo.77$Frost, main = "Número promedio de días con temperatura mínima por debajo del punto de congelación(1931–1960) en la capital o ciudad grande", ylab = "Estados", xlab = "Días")
hist(edo.77$Area, main = "Area de tierra en millas cuadrad", ylab = "Estados", xlab = "Area")

# Hacemos el Summary para extraer los datos descriptivos
summary(edo.77)

# Realizamos los boxsplot

boxplot(edo.77$Population, ylab="Poblacion")
boxplot(edo.77$Income, ylab="Renta")
boxplot(edo.77$Illiteracy, ylab="Porcentaje Poblacion Analfabeta")
boxplot(edo.77$Life.Exp, ylab="Espezanda de Vida")
boxplot(edo.77$Murder, ylab="Asesinato y Tasa de Homicidios")
boxplot(edo.77$HS.Grad, ylab="Pocentaje de Graduados")
boxplot(edo.77$Frost, ylab="Dias con temperatura minima")
boxplot(edo.77$Area, ylab="Area")

# Desviacion estandar

sd(edo.77$Population)
sd(edo.77$Income)
sd(edo.77$Illiteracy)
sd(edo.77$Life.Exp)
sd(edo.77$Murder)
sd(edo.77$HS.Grad)
sd(edo.77$Frost)
sd(edo.77$Area)

#####################################################################################

# 1.2 (10 puntos) Realice un gráfico de dispersión y una matriz de correlación de las
# variables independientes respecto a Life Exp. Interprete los resultados.

# Gráfico de dispersión
pairs(edo.77)

# Matriz de correlación
cor(edo.77)

#####################################################################################

# 1.3 (30 puntos) ¿Cuál es el modelo que explica mejor la variabilidad de Life Exp? 
# Incluya todas las pruebas necesarias para llegar a este modelo. Utilice un nivel
# de significancia de 0.001.

attach(edo.77)
# Definimos el modelo lineal
mod1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = edo.77)
summary(mod1)
# Graficamos el modelo para analizarlo
plot(mod1)

# Eliminamos las variables que no son significativas hasta obtener el mejor modelo
mod2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = edo.77)
summary(mod2)

mod3 = lm(Life.Exp ~ Murder + HS.Grad + Frost, data = edo.77)
summary(mod3)
plot(mod3)

#####################################################################################

# 1.4 (20 puntos) Realice un análisis de residuos al modelo ganador.
# VER RESPUESTA EN EL INFORME

#####################################################################################



#1.5 (20 puntos) Un químico quiere probar el efecto de 4 agentes químicos sobre la resistencia de un tipo
#particular de tela. Debido a que podría haber variabilidad de un rollo de tela a otro, el químico decide usar
#un diseño de bloques aleatorizados, con los rollos de tela considerados como bloques. Selecciona 5 rollos y
#aplica los 4 agentes químicos de manera aleatoria a cada rollo. A continuación se presentan las resistencias a
#la tención resultantes. Analizar los datos de este experimento (utilizar α = 0.05) y sacar las conclusiones
#apropiadas.


# Construimos los vectores
Rollo_1= c(73,73,75,73)
Rollo_2= c(68,67,68,71)
Rollo_3= c(74,74,78,75)
Rollo_4= c(71,72,73,75)
Rollo_5= c(67,70,68,69)

dat = c(Rollo_1,Rollo_2,Rollo_3,Rollo_4,Rollo_5)
fac = c(replicate(4,"Rollo_1"),replicate(4,"Rollo_2"),replicate(4,"Rollo_3")
        ,replicate(4,"Rollo_4"),replicate(4,"Rollo_5"))
fact = factor(fac)

tapply(dat,fact,mean)

boxplot(dat~fact)

mod.lm = lm(dat~fact)
anova(mod.lm)

# PARTE2

Agente_Quimico_1= c(73,68,74,71,67)
Agente_Quimico_2= c(73,67,74,72,70)
Agente_Quimico_3= c(75,68,78,73,68)
Agente_Quimico_4= c(73,71,75,75,69)

dat = c(Agente_Quimico_1,Agente_Quimico_2,Agente_Quimico_3,Agente_Quimico_4)
fac = c(replicate(5,"Agente_Quimico_1"),replicate(5,"Agente_Quimico_2"),replicate(5,"Agente_Quimico_3")
        ,replicate(5,"Agente_Quimico_4"))
fact = factor(fac)

tapply(dat,fact,mean)

boxplot(dat~fact)

mod.lm = lm(dat~fact)
anova(mod.lm)



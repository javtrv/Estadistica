# 1.1 (20 puntos) Realice un análisis descriptivo de los datos (histograma, gráfico de cajas, número de la
#muestra, mínimo, cuartiles, media y desviación).


#Se cargan los datos
data(state)
edo.77<- as.data.frame(state.x77)
names(edo.77)[4] <- "Life.Exp"
names(edo.77)[6] <- "HS.Grad"

#Mostramos los datos
edo.77

#• Population: estimación de población al 1 de julio de 1975.
#• Income: renta per capita (1974).
#• Illiteracy: analfabetismo (1970, porcentaje de la población).
#• Life.Exp: esperanza de vida en años (1969–71)
#• Murder: asesinato y tasa de homicidio no negligente por cada 100.000 habitantes (1976)
#• HS Grad: porcentaje de graduados de escuela secundaria (1970).
#• Frost: número promedio de días con temperatura mínima por debajo del punto de congelación
#(1931–1960) en la capital o ciudad grande.
#• Area: área de tierra en millas cuadrad

#Realizamos los histogramas de las variables
hist(edo.77$Population, main = "Poblacion estimada al 01/07/1975", ylab = "Estados", xlab = "Cantidad de personas")
hist(edo.77$Income, main = "Renta per capita (1974)", ylab = "Estados", xlab = "Renta")
hist(edo.77$Illiteracy, main = "Analfabetismo (1970, porcentaje de la población)", ylab = "Estados", xlab = "Porcentaje de Problacion Analfabeta")
hist(edo.77$Life.Exp, main = "Esperanza de vida en años (1969–71)", ylab = "Estados", xlab = "Años")
hist(edo.77$Murder, main = "Asesinato y tasa de homicidio no negligente por cada 100.000 habitantes (1976)", ylab = "Estados", xlab = "Tasa")
hist(edo.77$HS.Grad, main = "Porcentaje de graduados de escuela secundaria (1970)", ylab = "Estados", xlab = "Graduados")
hist(edo.77$Frost, main = "Número promedio de días con temperatura mínima por debajo del punto de congelación(1931–1960) en la capital o ciudad grande", ylab = "Estados", xlab = "Días")
hist(edo.77$Area, main = "Area de tierra en millas cuadrad", ylab = "Estados", xlab = "Area")

## Hacemos el Summary para extraer los datos descriptivos
summary(edo.77)


#Realizamos los boxsplot

boxplot(edo.77$Population, ylab="Poblacion")
boxplot(edo.77$Income, ylab="Renta")
boxplot(edo.77$Illiteracy, ylab="Porcentaje Poblacion Analfabeta")
boxplot(edo.77$Life.Exp, ylab="Espezanda de Vida")
boxplot(edo.77$Murder, ylab="Asesinato y Tasa de Homicidios")
boxplot(edo.77$HS.Grad, ylab="Pocentaje de Graduados")
boxplot(edo.77$Frost, ylab="Dias con temperatura minima")
boxplot(edo.77$Area, ylab="Area")

#Desviacion estandar

sd(edo.77$Population)
sd(edo.77$Income)
sd(edo.77$Illiteracy)
sd(edo.77$Life.Exp)
sd(edo.77$Murder)
sd(edo.77$HS.Grad)
sd(edo.77$Frost)
sd(edo.77$Area)

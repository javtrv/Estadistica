mercado = read.table("Mercado.txt", header=T)

edad = mercado$Edad
sexo = mercado$Sexo
fruta = mercado$Fruta
vegetales = mercado$Vegetales
carnico = mercado$Carnico
consumo = mercado$Consumo

################################################
# PREGUNTA 1

#Grafica Vegetales/Consumo
boxplot(split(consumo,vegetales),ylab="Consumo",col=c("purple","green","brown","red"))
hist(consumo, main = "Histograma de consumo por persona", ylab = "Personas")
#  SUMMARY

summaryVegetales = summary(vegetales)
summaryConsumo = summary(consumo)


# Varianza de consumo

varianzaConsumo = var(consumo)
varianzaConsumo

# Desviacion estandar
desviacionConsumo = sd(consumo)
desviacionConsumo

################################################
# PREGUNTA 2. Analice la relación en el dinero 
#gastado en las mujeres que compraron cambur según el tipo de cárnico

mujeresCamburCarnico = carnico[ sexo=="F" & fruta == "Cambur"]

boxplot(split(consumo,mujeresCamburCarnico), ylab="Consumo",col=c("red","pink","gray","yellow"))

##################################################

# PREGUNTA 3: Hombres mayores de 60 compraron carne

hombres60Carne = sexo[sexo == "M" & edad > 60 &  carnico == "Carne"]
cantidadHombres = length(hombres60Carne)
cantidadHombres




carne = read.table("zonas_carne.txt", header=T)

zona1 = carne$Zona1
zona1_guisar = zona1[carne$Tipo1 == "Guisar"]
zona2 = carne$Zona2
zona2_guisar = zona2[carne$Tipo2 == "Guisar"]
zona3 = carne$Zona3
zona3_guisar = zona3[carne$Tipo3 == "Guisar"]
zona4 = carne$Zona4
zona4_guisar = zona4[carne$Tipo4 == "Guisar"]

hist(zona1_guisar, main = "Histograma de precios de carne en Zona 1", ylab = "Abastecimientos")
hist(zona2_guisar, main = "Histograma de precios de carne en Zona 2", ylab = "Abastecimientos")
hist(zona3_guisar, main = "Histograma de precios de carne en Zona 3", ylab = "Abastecimientos")
hist(zona4_guisar, main = "Histograma de precios de carne en Zona 4", ylab = "Abastecimientos")

boxplot(zona1_guisar, ylab="Zona1")
# Laboratorio 3 - CO3321
# Sección 1
# Andrea Reyes 15-11201
# Javier Medina 12-10400


## 1. Se desea conocer si existe alguna relación entre el nivel socioeconómico (pobre, medio y rico) y la
## zona donde vive (rural y urbano) 505 grupos de familias:
##  X/Y      Rural(1) Urbano(2)
## Pobre(1)    249    139
## Medio(2)    80     20
## Rico(3)      2     15

columnas = 2
filas = 3

rural <- c(249, 80, 2)
urbano <- c(139, 20, 15)

(matriz <- cbind(rural, urbano))

chisq.test(matriz)


## 2. Se efectuó un estudio para determinar si los conductores preferı́an los carriles centrales de una vı́a
## rápida con cuatro carriles para cada sentido. Se observó un total de mil autos a las horas picos
## de la mañana y se registraron los carriles por los que éstos circulaban. La siguiente tabla contiene
## los resultados. Proporcionan los datos suficiente evidencia para concluir que los conductores tienen
## preferencia por algunos carriles? (Pruebe la hipótesis que afirma que p 1 = p 2 = p 3 = p 4 = 1/4
## utilizando un nivel de significancia de α = 0,05). Establezca lı́mites para el p-valor asociado.
## Carril   Número de carros observados
##   1               294
##   2               276
##   3               238
##   4               192

alpha = 0.05
n = 1000
k = 4
r = 0

p = c(1/4,1/4,1/4,1/4)
fi = c(294,276,238,192)
chi2_obs = sum((fi-n*p)^2/(n*p))
chi2_obs
chi2.alpha = qchisq(1-alpha,k-1-r)
chi2.alpha
p_valor = 1- pchisq(chi2_obs, k-1-r)
p_valor

prop.test(fi, c(1000,1000,1000,1000),p)

## 3. Los siguientes datos son las edades de una muestra de personas seleccionadas entre los visitantes de
## un Bingo.
## 32, 23, 64, 31, 74, 44, 61, 33, 66, 73,
## 27, 65, 40, 54, 23, 43, 58, 87, 58, 62,
## 68, 89, 93, 24, 73, 42, 33, 63, 36, 48,
## 77, 75, 37, 59, 70, 61, 43, 68, 54, 29,
## 48, 81, 57, 97, 35, 58, 56, 58, 57, 45
## Realizar una prueba Chi-cuadrado de bondad de ajuste para decidir si puede aceptarse que las
## edades sigan una distribución normal.

edades = c(32, 23, 64, 31, 74, 44, 61, 33, 66, 73,
           27, 65, 40, 54, 23, 43, 58, 87, 58, 62,
           68, 89, 93, 24, 73, 42, 33, 63, 36, 48,
           77, 75, 37, 59, 70, 61, 43, 68, 54, 29,
           48, 81, 57, 97, 35, 58, 56, 58, 57, 45)

# Tabla de frecuencia de las edades
hist(edades, plot = F)

r = 2
fi = c(5, 8, 7, 10, 10, 5, 3, 2)

# Gráficas para observar el comportamiento de los datos
qqnorm(fi)
qqline(fi)

k = length(fi)
n = sum(fi)

# Media de las clases
mi = c(25, 35, 45, 55, 65, 75, 85, 95)

xbarra = sum(fi*mi)/n
xbarra

x_barra = rep(xbarra, k)
x_barra

S_cuadrado = sum( fi*(mi-x_barra)^2 )/(n-1)
S_cuadrado

S = sqrt(S_cuadrado)
S

pi = pnorm(4 : 11* 10, xbarra, S) - pnorm(3 : 10 *10, xbarra, S)
pi

chi2_obs = sum((fi-n*pi)^2/(n*pi))
chi2_obs

chi2_alpha <- qchisq(1 - alpha, k - 1 -r)
chi2_alpha

p_valor = 1 - pchisq(chi2_obs, k - 1 - r)
p_valor

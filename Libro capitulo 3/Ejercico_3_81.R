# Ejericio 3.81 Libro Estadistica Descriptiva Campo Elias - UNAL

# Se define la matriz de datos

Y <- matrix(c( 9,  7, 8, 3, 1,  3, 4, 7, 2,  6, 
               9, 13, 6, 1, 5, 11, 4, 3, 8, 10), nrow=10, byrow=FALSE)

# Se dibuja la matriz de datos utilizando las coordenadas
# de la columna 1 como el eje "x" y la columna 2 como eje"y"

plot(Y[,1],Y[,2], main = "Grafico de la matriz de datos", xlab = "coordenadas x" , ylab = "coordenadas y")

# Se calcula el centro de gravedad para las coordenadas "x" y "y"
# para esto se calcula el promedio (mean) de la coordenadas

centro_gravedad <- c(mean(Y[,1]),mean(Y[,2]))

# se genera un vector vacio sobre el que operar

centrados_X <- c()

# se calcula las coordenadas "x" de la matriz de centrados"
# restando cada coordenada "x" con el promedio en "x" (centro_gravedad[1])
# se asignan al vector centrados_x 

for (i in 1:10){
  
  
  centrados_X <- append(centrados_X, Y[i]- centro_gravedad[1])
  
}

# se calcula las coordenadas "y" de la matriz de centrados"
# restando cada coordenada "y" con el promedio en "y" (centro_gravedad[2])
# se asignan al vector centrados_y 

# se genera un vector "vacio" sobre el que operar

centrados_Y <- c()

for (i in 11:20){
  
  centrados_Y <- append(centrados_Y, Y[i]- centro_gravedad[2])
  
}

# se genera la matriz de centrados con los centrados "x" y "y"

matriz_centrados <- matrix(c(centrados_X, centrados_Y), nrow=10, byrow=FALSE)

# se imprime la matriz de centrados

plot(matriz_centrados[,1],matriz_centrados[,2], main="Matriz de centrados", xlab = "centrados x" , ylab = "centrados y")


# se genera un vector "vacio" sobre el que operar

varianza_x<- 0

# Se calcula la varianza de los datos en x, 
# sumando  el cuadrado de los datos en "x" de la matriz de centrado

for (val in centrados_X){
  
  varianza_x <- varianza_x + val ^ 2
  
}

varianza_x <- varianza_x / 10

# se genera un vector "vacio" sobre el que operar

varianza_y <- 0

# Se calcula la varianza de los datos en y, 
# sumando  el cuadrado de los datos en "y" de la matriz de centrado

for (val in centrados_Y){
  
  varianza_y <- varianza_y + val ^ 2
  
}

varianza_y <- varianza_y / 10


# se genera un vector "vacio" sobre el que operar

covarianza <- 0

# Se calcula la covarianza de los datos, 
# sumando  las multiplicaciones de las coordenzadas (x,y) de la matriz de centrado

for(i in 1:10){
  
  covarianza <- covarianza + (centrados_X[i]*centrados_Y[i])

  
}

covarianza <- covarianza / 10

# Se genera la matriz de covarianzas
# cuya diagonal principal son las varianzas de los ejes(x,y)
# el resto de los datos son la covarianza entre variables

Matriz_de_convarianzas <- matrix(c(varianza_x,covarianza,covarianza,varianza_y), nrow=2, byrow=TRUE)

# se calcula los vectores y valores propios de la matriz de covarianza

valores_y_vectores_propios <- eigen(Matriz_de_convarianzas)

# se determina sobre que que eje se realizara la reduccion

if (valores_y_vectores_propios$values[1] > valores_y_vectores_propios$values[2]){
  
  print("Los valores pierden menos informacion proyectandolos hacia en eje x")
  
}else{
  
  print("Los valores pierden menos informacion proyectandolos hacia en eje y")
  
}

# Reduciendo la dimencionalidad
# como se reduce una dimension lo unico que hace falta es proyectar los valores X
# sobre el eje x, con Valores en Y iguales a 0 para cada coordenada en X

Vector_de_ceros <- numeric(10)

# se imprime la reduccion de la informacion
# proyectadndo los valores de la tabala inicial al eje x

plot(Y[,1],Vector_de_ceros, main = "Dimensionalidad reducida", xlab = " Datos proyectados al eje x", ylab = "")


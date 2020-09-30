library(FactoClass)
library(FactoMineR)
data(ColorAdjective)

# Calculo de la tabla de contingecia

suma_col = as.vector(colSums(ColorAdjective))

suma_row = as.vector(rowSums(ColorAdjective))

# Generacion matrix K

K <- ColorAdjective
K$sumaF <- suma_row

suma_col <- append(suma_col, sum(suma_col))

K <- rbind(K,suma_col)

# Creacion de la tabla en latex
# xtable(K)

ACS <- CA(ColorAdjective, ncp = 12)

plot(ACS, cex = 0.7, title = "Mapa del Analisis de Correspondencias")

plot(ACS, cex= 0.7, selectRow = "cos2 0.7", selectCol = "cos2 0.5", title = "Variables mejor representadas")

plot(ACS, cex= 0.7, selectRow = "contrib 4", selectCol = "contrib 4", title = "Variables mas contribuyen")

# Graficas entre otras dim 

plot(ACS, cex=0.7, axes=c(1,3), title="ACS DIM 1 y DIM 3", selectRow = "cos2 0.7", selectCol = "cos2 0.5")

# Calculo de representacion de colores por dim

# matrix con la reprsentacion dada por los cosenos cuadrados
R <- ACS[["col"]][["cos2"]]

# matrix vacia

M <- c()

for ( i in 1:10){
  
  j <- i + 1
  
  for ( j in j:10){
      
    M <- cbind(M, (R[,i] + R[,j]))
      
  }
    
}

# nombre de las columnas de la nueva matrix
dim_names <- c("Dim1-2","Dim1-3","Dim1-4","Dim1-5","Dim1-6","Dim1-7","Dim1-8","Dim1-9","Dim1-10",
               "Dim2-3","Dim2-4","Dim2-5","Dim2-6","Dim2-7","Dim2-8","Dim2-9","Dim2-10",	
               "Dim3-4","Dim3-5","Dim3-6","Dim3-7","Dim3-8","Dim3-9","Dim3-10",			
               "Dim4-5","Dim4-6","Dim4-7","Dim4-8","Dim4-9","Dim4-10",					
               "Dim5-6","Dim5-7","Dim5-8","Dim5-9","Dim5-10",								
               "Dim6-7","Dim6-8","Dim6-9","Dim6-10",										
               "Dim7-8","Dim7-9","Dim7-10","Dim8-9","Dim8-10","Dim9-10")

# asignacion de los nombre de la nueva matrix
colnames(M) <- dim_names



# Dimension de mejor representacion de la matrix

Blue <- sort(M[1,], decreasing=TRUE);Blue
Red <- sort(M[2,], decreasing=TRUE);Red
Yellow <- sort(M[3,], decreasing=TRUE);Yellow
White <- sort(M[4,], decreasing=TRUE);White
Grey <- sort(M[5,], decreasing=TRUE);Grey
Pink <- sort(M[6,], decreasing=TRUE);Pink
Brown <- sort(M[7,], decreasing=TRUE);Brown
Purple <- sort(M[9,], decreasing=TRUE);Purple
Black <- sort(M[9,], decreasing=TRUE);Black
Orange <- sort(M[10,], decreasing=TRUE);Orange


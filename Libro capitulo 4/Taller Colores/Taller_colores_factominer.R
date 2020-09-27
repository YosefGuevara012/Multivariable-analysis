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

ACS <- CA(ColorAdjective)

plot(ACS, cex = 0.7, title = "Mapa del Analisis de Correspondencias")

plot(ACS, cex= 0.7, selectRow = "cos2 0.7", selectCol = "cos2 0.7", title = "Variables mejor representadas")

plot(ACS, cex= 0.7, selectRow = "contrib 4", selectCol = "contrib 4", title = "Variables mas contribuyen")

# Graficas entre otras dim 

plot(ACS, cex=0.7, axes=c(1,3), title="ACS DIM 1 y DIM 3")

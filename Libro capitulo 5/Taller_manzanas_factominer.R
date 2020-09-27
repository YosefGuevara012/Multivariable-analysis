library(FactoClass)
library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
data(Bogota)


# Se asigna la data de Bogota inluyendo los la col No_STR
#

bta_NSTR <- as.matrix(Bogota)


# Tabla de contingencia K

ni <- colSums(bta_NSTR)
nj <- rowSums(bta_NSTR)

total_nij <- sum(ni)


# Calculo de frecuencias relativas F

F <- round((bta_NSTR/total_nij) * 100, 2)

fi <- round(rowSums(F), 2)

fj <- round(colSums(F), 2)

total_fij <- round(sum(fi),0)

# Perfil fila

Dn <- diag(fi)


PF <- t(F) %*% solve(Dn)

PF <- t(PF) * 100

# Marginal_C perfil fila

MC_PF <- colSums(PF)


MC_PF <- (MC_PF / sum(MC_PF))*100

# Perfil Columna

Dp <- diag(fj)


PC <- F %*% solve(Dp)

PC <- PC * 100

# Marginal_C perfil Columna

MC_PC <- rowSums(PC)

MC_PC <- (MC_PC / sum(MC_PC))*100

# Histograma perfil columna

# Create the input vectors.
colors = c("antiquewhite1","aquamarine1","azure1","burlywood3","cadetblue1","coral1",
          "cornflowerblue","darkolivegreen1","darkorchid1","goldenrod2","hotpink1","lightgoldenrod4",
           "lightgreen", "lightskyblue1","slateblue2","seagreen1","mistyrose3","gray42","darkorange")


estratos <- colnames(Bogota)
localidades <- rownames(Bogota)

# Create the matrix of the values.

Values <- PC/100

# Create the bar chart
barplot(Values, main = "Perfiles Columna", names.arg = estratos, xlab = "Localidades", ylab = "Estratos", col = colors, cex.axis=0.7, cex.names=0.5, horiz = TRUE, las=2 )

# Add the legend to the chart
legend("topright", localidades, cex = 0.5, fill = colors)

# MC_PC 

barplot(MC_PC/100, main = "Marginal C" , xlab = "",  ylab = "Porcentaje de manzanas", col = colors, cex.axis=0.7, cex.names=0.5, las=2)

# Histograma perfil fila

# Create the input vectors.
# colors = c("antiquewhite1","aquamarine1","azure1","burlywood3","cadetblue1","coral1",
#           "cornflowerblue","darkolivegreen1","darkorchid1","goldenrod2","hotpink1","lightgoldenrod4",
#           "lightgreen", "lightskyblue1","slateblue2","seagreen1","mistyrose3","gray42","darkorange")

colors <- brewer.pal(n = 6, name = 'Dark2')
estratos <- colnames(Bogota)
localidades <- rownames(Bogota)

# Create the matrix of the values.

Values <- PF/100

# Create the bar chart
barplot(t(Values), main = "Perfiles fila", names.arg = localidades, xlab = "Estratos", ylab = "Localidades", col = colors, cex.axis=0.7, cex.names=0.5, horiz = TRUE, las=2,
)

# Add the legend to the chart
# legend("topright", estratos, cex = 0.5, fill = colors)

# histograma margin_C pF

barplot(MC_PF/100, main = "Marginal C" , xlab = "",  ylab = "Localidades", col = colors, cex.axis=0.7, cex.names=0.5, las=2)

ACS_NSTR <- CA(Bogota, ncp = 5, row.sup = NULL, col.sup = NULL,quanti.sup=NULL, quali.sup = NULL, graph = TRUE,axes = c(1,2), row.w = NULL, excl=NULL)

ACS <- CA(Bogota[,-1])

plot(ACS, cex = 0.7, title = "Mapa del Analisis de Correspondencias")

plot(ACS, cex= 0.7, selectRow = "cos2 0.7", selectCol = "cos2 0.7", title = "Variables mejor representadas")

plot(ACS, cex= 0.7, selectRow = "contrib 4", selectCol = "contrib 4", title = "Variables mas contribuyen")




# Graficas entre otras dim 

plot(ACS, cex=0.7, axes=c(1,3), title="ACS DIM 1 y DIM 3")

# Agrupamiento de por estratos

ACS.hcpc <- HCPC(ACS, cluster.CA = "columns") 

# Agrupamiento de por localidaes

ACS.hcpc <- HCPC(ACS, cluster.CA = "rows") 


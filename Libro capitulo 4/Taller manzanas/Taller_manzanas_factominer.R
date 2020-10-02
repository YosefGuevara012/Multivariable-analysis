library(FactoClass)
library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
data(Bogota)


# Se asigna la data de Bogota inluyendo los la col No_STR
#

bta <- as.matrix(Bogota[,-1])


# Tabla de contingencia K

ni <- colSums(bta)
nj <- rowSums(bta)

total_nij <- sum(ni)


# Generacion matrix K

K <- Bogota[,-1]
K$Total_R <- nj

suma_col <- append(suma_col, sum(ni))

K <- rbind(K,suma_col)

rownames(K) <- c(rownames(Bogota[,-1]),"Total_C")

# Calculo del marginal de la distribución


# Histograma de la reparticion de manzanas por estratos

colors <- brewer.pal(n = 6, name = 'Dark2')
estratos <- colnames(bta)
localidades <- rownames(bta)

# Create the matrix of the values.

# Create the bar chart
barplot(ni, main = "Repartición de las manzanas según el estrato", names.arg = estratos, xlab = "Estratos", ylab = " No. de manzanas por estrato ", col = colors, cex.axis=0.7, cex.names=0.7, las=2)

mazanas_x_str = ni/total_nij


# Histograma de la reparticion de manzanas por localidades


colors = c("antiquewhite1","aquamarine1","azure1","burlywood3","cadetblue1","coral1",
           "cornflowerblue","darkolivegreen1","darkorchid1","goldenrod2","hotpink1","lightgoldenrod4",
           "lightgreen", "lightskyblue1","slateblue2","seagreen1","mistyrose3","darkorange")



barplot(nj, main = "Repartición de las manzanas según la localidad", names.arg = localidades, xlab = "Localidad", ylab = " No. de manzanas por localidad ", col = colors, cex.axis=0.7, cex.names=0.7, las=2)

mazanas_x_localidades = nj/total_nij

# Calculo de frecuencias relativas F

F <- round((bta/total_nij) * 100, 2)

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
           "lightgreen", "lightskyblue1","slateblue2","seagreen1","mistyrose3","darkorange")


estratos <- colnames(Bogota[,-1])
localidades <- rownames(Bogota[,-1])

# Create the matrix of the values.

Values <- PC/100

# Create the bar chart
barplot(Values, main = "Perfiles Columna", names.arg = estratos, xlab = "Localidades", ylab = "Estratos", col = colors, cex.axis=0.7, cex.names=0.5, horiz = TRUE, las=2 )

# Add the legend to the chart
legend("topright", localidades, cex = 0.5, fill = colors)


# Histograma perfil fila

# Create the input vectors.

colors <- brewer.pal(n = 6, name = 'Dark2')
estratos <- colnames(Bogota[,-1])
localidades <- rownames(Bogota[ordep,])

# Create the matrix of the values.

Values <- PF/100

# Creacion de grafica del promedio de los perfiles ordenados

acs_facto <-dudi.coa (Bogota[,-1], scannf =FALSE ,nf =3)
ordep <-order (acs_facto$li[ ,1]);
# Create the bar chart
barplot(t(Values[ordep,]), main = "Perfiles fila", names.arg = localidades, xlab = "Estratos", ylab = "Localidades", col = colors, cex.axis=0.5, cex.names=0.5, horiz = TRUE, las=2)

# Add the legend to the chart
legend("topright", estratos, cex = 0.5, fill = colors)

# Analisis de correspondencia simple usando FactoMiner

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




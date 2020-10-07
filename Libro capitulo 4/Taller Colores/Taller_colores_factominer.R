library(FactoClass)
library(FactoMineR)
library(ggplot2)
library(RColorBrewer)
data(ColorAdjective)

# Calculo de la tabla de contingecia

suma_col = as.vector(colSums(ColorAdjective))

suma_row = as.vector(rowSums(ColorAdjective))

# Generacion matrix K

K <- ColorAdjective
K$sumaF <- suma_row

suma_col <- append(suma_col, sum(suma_col))

K <- rbind(K,suma_col)

rownames(K) <- c(rownames(ColorAdjective), "Total_C")

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

Blue <- round(sort(M[1,], decreasing=TRUE),2);Blue
plot(ACS, cex=0.7, axes=c(3,8), title="ACS - Color Azul DIM 3 y DIM 8", selectRow = "cos2 0.32", selectCol = "cos2 0.64")

Red <- round(sort(M[2,], decreasing=TRUE),2);Red
plot(ACS, cex=0.7, axes=c(1,6), title="ACS - Color Rojo DIM 1 y DIM 6", selectRow = "cos2 0.6", selectCol = "cos2 8.926268e-01")


Yellow <- round(sort(M[3,], decreasing=TRUE),2);Yellow
plot(ACS, cex=0.7, axes=c(1,5), title="ACS - Color Amarillo DIM 1 y DIM 5", selectRow = "cos2 0.6", selectCol = "cos2 0.91")

White <- round(sort(M[4,], decreasing=TRUE),2);White
Pink <- round(sort(M[6,], decreasing=TRUE),2);Pink
plot(ACS, cex=0.7, axes=c(3,4), title="ACS - Color Rosado y Blanco, DIM 3 y DIM 4", selectRow = "cos2 0.6", selectCol = "cos2 0.70")

Grey <- round(sort(M[5,], decreasing=TRUE),2);Grey
plot(ACS, cex=0.7, axes=c(2,10), title="ACS - Color Gris DIM 2 y DIM 10", selectRow = "cos2 0.3", selectCol = "cos2 0.56")

Brown <- round(sort(M[7,], decreasing=TRUE),2);Brown
plot(ACS, cex=0.7, axes=c(7,9), title="ACS - Color Café DIM 7 y DIM 9", selectRow = "cos2 0.3", selectCol = "cos2 0.50")

Purple <- round(sort(M[8,], decreasing=TRUE),2);Purple
plot(ACS, cex=0.7, axes=c(2,7), title="ACS - Color Purpura DIM 2 y DIM 7", selectRow = "cos2 0.5", selectCol = "cos2 0.65")

Black <- round(sort(M[9,], decreasing=TRUE),2);Black
plot(ACS, cex=0.7, axes=c(2,9), title="ACS - Color Negro DIM 2 y DIM 9", selectRow = "cos2 0.6", selectCol = "cos2 0.65")

Orange <- round(sort(M[10,], decreasing=TRUE),2);Orange
plot(ACS, cex=0.7, axes=c(1,6), title="ACS - Color Naranja DIM 1 y DIM 6", selectRow = "cos2 0.6", selectCol = "cos2 0.94")

Verde <- round(sort(M[11,], decreasing=TRUE),2);Verde
plot(ACS, cex=0.7, axes=c(2,4), title="ACS - Color Verde DIM 2 y DIM 4", selectRow = "cos2 0.45", selectCol = "cos2 0.57")

# punto 7

# Agrupamiento de por colores

ACS.hcpc <- HCPC(ACS, cluster.CA = "columns", description = F) 

#Azul

colors <- brewer.pal(n = 12, name = 'Paired')
temp <- sort(ColorAdjective$BLUE[ColorAdjective$BLUE>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$BLUE, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:8])
temp <- c(sum(temp[9:length(temp)]),temp[1:8])

# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Azul", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)


#Rojo

temp <- sort(ColorAdjective$RED[ColorAdjective$RED>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$RED, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:7])
temp <- c(sum(temp[8:length(temp)]),temp[1:7])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Rojo", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)

#Amarillo

temp <- sort(ColorAdjective$YELL[ColorAdjective$YELL>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$YELL, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:8])
temp <- c(sum(temp[9:length(temp)]),temp[1:8])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Amarillo", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)

#Blanco

temp <- sort(ColorAdjective$WHIT[ColorAdjective$WHIT>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$WHIT, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:8])
temp <- c(sum(temp[9:length(temp)]),temp[1:8])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Blanco", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)



#Gris

temp <- sort(ColorAdjective$GREY[ColorAdjective$GREY>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$GREY, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:8])
temp <- c(sum(temp[9:length(temp)]),temp[1:8])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Gris", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)


#Rosado

temp <- sort(ColorAdjective$PINK[ColorAdjective$PINK>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$PINK, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:9])
temp <- c(sum(temp[10:length(temp)]),temp[1:9])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Rosado", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)


#Cafe

temp <- sort(ColorAdjective$BROW[ColorAdjective$BROW>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$BROW, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:12])
temp <- c(sum(temp[13:length(temp)]),temp[1:12])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Café", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)

#Purpura

temp <- sort(ColorAdjective$PURP[ColorAdjective$PURP>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$PURP, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:10])
temp <- c(sum(temp[11:length(temp)]),temp[1:10])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Purpura", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)

#Negro

temp <- sort(ColorAdjective$BLAC[ColorAdjective$BLAC>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$BLAC, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:6])
temp <- c(sum(temp[7:length(temp)]),temp[1:6])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Negro", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)


#Naraja

temp <- sort(ColorAdjective$ORAN[ColorAdjective$ORAN>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$ORAN, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:9])
temp <- c(sum(temp[9:length(temp)]),temp[1:9])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Naranja", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)


#Verde

temp <- sort(ColorAdjective$GREE[ColorAdjective$GREE>0], decreasing = T)
adjetivos <- rownames(ColorAdjective[order(ColorAdjective$GREE, decreasing = T),])[1:length(temp)]
adjetivos <- c( "otros",adjetivos[1:8])
temp <- c(sum(temp[9:length(temp)]),temp[1:8])


# Create the bar chart
barplot(temp, main = "Perfil de frecuencia adjetivos asociados al color Verde", names.arg = adjetivos, xlab = "Adjetivos", ylab = "Frecuencia del Adjetivo", col = colors, cex.axis=0.7, cex.names=0.7, las=2)


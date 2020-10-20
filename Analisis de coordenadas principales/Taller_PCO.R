library(FactoClass)
library(RColorBrewer)
library(ggplot2)
load("~/Esp_Estadistica/Analisis multivariado/Multivariable-analysis/Analisis de coordenadas principales/disCultuLatino.Rda")
ACO <- dudi.pco(d = dis, scannf = FALSE, nf = 4)

Mdis <- as.matrix(dis)

# Graficas de los valores propios
colors <- brewer.pal(n = 8, name = 'Dark2')
xval <- c("1","2","3","4","5","6","7","8")
Ev <- ACO[["eig"]]
barplot(Ev, main = "Representación de valores propios", names.arg = xval, xlab = "Valores propios", ylab = "% del valor propio", col = colors, cex.axis=0.7, cex.names=0.7)

# Rango del analisis 

rango <- ACO[["rank"]];rango

# Represntacion en plano 1-2

plot.dudi(ACO,ex=1,ey=2,infaxes="out",main="Analisis de coordenadas principales Dim 1 - Dim 2", Tcol= FALSE)

# Represntacion en plano 1-3

plot.dudi(ACO,ex=1,ey=3,infaxes="out",main="Analisis de coordenadas principales Dim 1 - Dim 3", Tcol= FALSE)

# Represntacion en plano 1-4

plot.dudi(ACO,ex=1,ey=4,infaxes="out",main="Analisis de coordenadas principales Dim 1 - Dim 4", Tcol= FALSE)

# Represntacion en plano 2-3

plot.dudi(ACO,ex=2,ey=3,infaxes="out",main="Analisis de coordenadas principales Dim 2 - Dim 3", Tcol= FALSE)

# Represntacion en plano 2-4

plot.dudi(ACO,ex=2,ey=4,infaxes="out",main="Analisis de coordenadas principales Dim 2 - Dim 4", Tcol= FALSE)

# Represntacion en plano 3-5

plot.dudi(ACO,ex=3,ey=4,infaxes="out",main="Analisis de coordenadas principales Dim 3 - Dim 4", Tcol= FALSE)

# AMA 2

ma_dis<-as.matrix(dis)
ma_dis

n<-nrow(ma_dis)
n

c<-ncol(ma_dis)
c

e<-dudi.pco(dis,scannf = F,full = T)
data.frame(e$eig)

barplot(e$eig)

data.frame(e$co)

data.frame(e$tab)

data.frame(e$l1)

w<-inertia.dudi(e,row.inertia = T,col.inertia = T)
data.frame(w$row.abs)

summary(w$row.abs)

plot.dudi(e)

plot.dudi(e,1,3)

plot.dudi(e,2,3)

library ( FactoClass ) # cargar FactoClass ;
data ( admi ) # cargar la tabla ;
# 6 barplots y etiquetas de las categorias en forma horizontal

# manualmente cuadrar la ventana Plots
# dev . print ( device = pdf) # grabar la grafica como Rplots . pdf

# capitulo 2
xtable(cor(admi$exam,admi[,2:6]),digits=rep(3,6))
plotpairs(admi[,2:6],col=gray.colors(10,0.8,0.4,2.2));
n<-nrow(admi);
V<-(n-1)/n*var(admi[,2:6]);
xtable(V,digits=rep(2,6));

boxplot(admi$exam~admi$carr,las=1)
xtable(centroids(admi[,2:7],
                 admi$carr)$cr*100,digits = rep(2,7))

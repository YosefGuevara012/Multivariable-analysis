## Se Carga la libreria necesaria
library ( FactoClass )
## Se leen los datos necesarios
data ( Whisky )


# Selecciona las columnas columnas continuas  del df del whisky


datos <- as.matrix(Whisky[, -3])

#para imprimir estad´ısticas b´asicas
cov(datos)
#matriz de correlaciones
cor(datos)
#vector de medias
mean(datos)
#desviaciones est´andar
sapply(datos,sd)

#se carga la librer´ıa

#an´alisis de componentes principales
acp<-dudi.pca(datos,scannf = FALSE,nf=ncol(datos))
#valores propio?
inertia.dudi(acp)
#vectores propios
acp$c1

#gr´afico scree
plot(acp$eig,type="b") # gr´afica de valores propios
#componentes principales
acp$li
#individuos sobre el primer plano factorial
s.label(acp$li,clabel=0.7,grid = FALSE)
#c´ırculo de c?rrelaciones
s.corcirc?e(acp$co,grid=FALSE)
#individuos sobre el primer plano factorial con biplot
s.label(acp$li,clabel=0.7,grid = FALSE,boxes =FALSE)
s.corcircle(acp$co,grid=FALSE,add=TRUE,clabel = 0.7)


#an´alisis de componentes principales desde la ?atriz
#de correlaciones?acp<-princomp(datos,cor=TRUE)
summary(acp)
# gr´afico scree
plot(acp)
# la desviaci´on est´andar de cada componente principal
# es decir la raiz de los valores propios de la matriz
acp$sdev
# Matriz con los vectores propios?acp$loadings
# la media de la? variables originales con la que se centran
# las observaciones
acp$center
# n´umero de observaciones
acp$n.obs
# las coordenadas factoriales
acp$scores
#biplots
par(mfrow=c(2,2))
# primer plano factorial
biplot(acp)
#segun?o plano factorial
biplot(acp,ch?ices = c(1,3))
# otro plano factorial
biplot(acp,choices = c(2,3))
#acp desde la matriz de covarianzas
#(opci´on por defecto )
acpCov<-princomp(datos)




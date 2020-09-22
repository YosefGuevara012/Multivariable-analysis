
## Se Carga la libreria necesaria
library ( FactoClass )

## Se leen los datos necesarios
data ( Whisky )


# Asigna las columnas numericas del DF a una lista "Y"

Y <- as.matrix(Whisky[, -3])


### Creacion del  ACP_NO_NORMADO

## Calculo del centro de Gravedad

g <- colMeans(Y)

## Centrado de datos

# Genera un vecotor de unos segun la cantidad de variables
n <- nrow(Y) 
unos <- rep (1,n);

# Cacula la matriz centrada

Yc <- Y - unos%*%t(g)


## Calculo de la Matriz de Varianzas

V <- t(Yc) %*% as.matrix(Yc)
Dsigma <-diag ( sqrt ( diag (V ))); round ( diag ( Dsigma ) ,1);
X <-as.matrix(Yc) %*% solve ( Dsigma ); 
colnames (X) <- colnames (Y);
## suma las varianzas de la matriz de varianzas

suma_var <- sum(diag(V))

contrib_var <- diag(V)/suma_var
## Obtencion de los Valores y Vectores Propios

des <-eigen (V);

lambda <-des$values
U <- des$vectors
rownames(U) <- rownames (V)
colnames(U) <- c(" Eje1 "," Eje2 "," Eje3 ", " Eje4 " ); 
round (U ,3)
# Sentido de los ejes?

F <- X %*% U
round ( sort (F[ ,1]) ,2)

round (F ,2) # coordenadas sobre los nuevos ejes
plot (F[ ,1:2] , las =1, asp =1) # plano 12
text (F[ ,1:2] , label = rownames (F), col=" black ",pos =2) # etiquetas
abline (h=0,v=0, col =" darkgrey ") # ejes
rowSums (F^2) -> d2;d2 # distancias
1/n*F^2 %*%diag (1/ lambda )*100 -> cont # contribuciones
F^2/d2*100 -> cos2 # cosenos cuadrados
Ayu <-cbind ( dis2 =d2 ,F1=F[ ,1] , F2=F[ ,2] , cont1 = cont [,1] , cont2 =
                cont [,2] , cos21 = cos2 [ ,1] , cos22 = cos2 [,2] ,
              cosp = rowSums ( cos2 [ ,1:2]))
round (Ayu ,2)

xtable (Ayu , digits = rep (2 ,9))


comer <-as.matrix ( Y); comer
comc <- comer - rep(1,2) %*%(g); comc # centrado
comcr <- comc %*%solve ( Dsigma ) # reducido
colnames ( comcr ) <- colnames ( comer ); comcr
Fsup <- comcr %*%U; Fsup
# primer plano factorial
plot (F[ ,1:2] , las =1, asp =1)
text (F[ ,1:2] , label = rownames (F), col=" black ",pos =1)
abline (h=0,v=0, col=" darkgrey ")
points (Fsup ,col =" black ",pch =20) # cafes comerciales
text (Fsup , labels =c(" Com1 "," Com2 "), col=" darkgreen ",pos =2)


conta <-factor (c(" exce "," maiz "," ceba "," maiz "," ceba "," exce ",
                  " maiz "," ceba "," maiz "," ceba "))
centroids (F, conta )$ centroids -> Fconta ; Fconta
points ( Fconta ,col =" brown ",pch =20)
text ( Fconta , col=" brown ",labels = rownames ( Fconta ),pos =2)

# tabla de ayudas para la interpretacion
# 2 Analice la matriz de varianzas y covarianzas con la ayuda del primer
# plano factorial de las variables. Haga un resumen (interpretación del
# primer plano factorial de las variables).


# 3. Realice el ACP normado y justifique por qué es el que conviene para los objetivos de este taller.

# Calculo del PCA

pca_normado <- prcomp(t(Y), scale = TRUE)


# Principal Components

pca_normado$x

# Grafico de los 2 ejees principales

plot(pca_normado$x[,1],pca_normado$x[,2])

pca_normado.var <- pca_normado$sdev^2

pca_normad.var.per <- round(pca_normado.var/sum(pca_normado.var)*100 , 1)

barplot(pca_normad.var.per, main = "Scree Plot", xlab = "Componente Principal", ylab = "Porcentaje de la desviacion")


# Grafico del PCA

pca_normado.data <- data.frame(Sample = row.names(pca_normado$x),
                               X= pca_normado$x[,1],Y= pca_normado$x[,2])

pca_normado.data

ggplot(data = pca_normado.data, aes(x=X, y=Y, label= Sample)) +
  geom_text() +
  xlab(paste("PCA1 - ", pca_normad.var.per[1], "%", sep="")) +
  ylab(paste("PCA2 - ", pca_normad.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("Grafico del PCA")

loading_scores <- pca_normado$rotation[,1]
whisky_score <- abs(loading_scores)
whisky_score_ranking <- sort(whisky_score, decreasing = TRUE)

top_10_whisky <- names(whisky_score_ranking[1:10])

top_10_whisky

pca_normado$rotation[top_10_whisky,1]
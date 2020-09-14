
## Se Carga la libreria necesaria
library ( FactoClass )

## Se leen los datos necesarios
data ( Whisky )

## Se crea una lista que unicamente contiene las variables Continuas


# Selecciona las columnas numericas del DF

numric <- unlist(lapply(Whisky, is.numeric));

# Asigna las columnas numericas del DF a una lista "Y"

Y <- as.matrix(Whisky[, numric])


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

## Obtencion de los Valores y Vectores Propios

des <-eigen (V);

lambda <-des$values
U <- des$vectors
rownames(U) <- rownames (V)
colnames(U) <- c(" Eje1 "," Eje2 "," Eje3 ", " Eje4 " ); 
round (U ,3)

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





## Se Carga la libreria necesaria
library ( FactoClass )

## Se leen los datos necesarios
data ( Whisky )

## Se crea una lista que unicamente contiene las variables Continuas


# Selecciona las columnas numericas del DF

numric <- unlist(lapply(Whisky, is.numeric));

# Asigna las columnas numericas del DF a una lista "Y"

Y <- as.matrix(Whisky[, numric])

# PCA

pca <- prcomp(Y, scale = TRUE)
pca
summary(pca)
plot(pca, type="l")
biplot(pca, scale = 0)


# PC components





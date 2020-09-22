## Se Carga la libreria necesaria
library ( FactoClass )
library ( factoextra )
## Se leen los datos necesarios
data ( Whisky )


# Selecciona las columnas columnas continuas  del df del whisky


Y <- as.matrix(Whisky[, -3])

# ACP

res.pca <- prcomp(Y,  scale = T)

# Extrae los resultados delos individuos

individuos <- get_pca_ind(res.pca)
print(individuos)

head(individuos$coord) # coordinates of individuals
head(individuos$cos2) # cos2 of individuals
head(individuos$contrib) # contributions of individuals

# Extract the results for variables

var <- get_pca_var(res.pca)
print(var)
head(var$coord) # coordinates of variables
head(var$cos2) # cos2 of variables

head(var$contrib) # contributions of variables

# You can also use the function get_pca()
get_pca(res.pca, "ind") # Results for individuals
get_pca(res.pca, "var") # Results for variable categories


library(FactoClass)
library(factoextra)
library(FactoMineR)

data(Bogota)

res.ca<-CA(Bogota, graph = FALSE)

get_ca_row(res.ca)

fviz_contrib(res.ca, choice = "row", axes = 1)

fviz_contrib(res.ca, choice = "col", axes = 1)

fviz_ca_row(res.ca, repel = TRUE)

fviz_ca_col(res.ca)

fviz_ca_biplot(res.ca, repel = TRUE)


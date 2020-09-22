## Se Carga la libreria necesaria
library ( FactoClass )

## Se leen los datos necesarios
data ( Whisky )


# Selecciona las columnas columnas continuas  del df del whisky


Y <- as.matrix(Whisky[, -3])

# Estadisticas basicas y boxplot
cat( "\n Estadosticas Basicas \n")

print(summary(Y))

cat( "\n Desviaciones Estandar \n")

print(apply(Y,2,sd))

boxplot(Y)

# ACP_NORMADO

acp <- dudi.pca(df = Y, nf = 2, scannf = FALSE)

acpI <- inertia.dudi(acp, row.inertia = TRUE, col.inertia = TRUE)

# Impresion del ACP y el ACPI con tiutlos 

cat("\n Valores propios \n")

print(acpI$tot,2)

plot(acp$eig)

cat("\n Vectores propios \n")

print(acp$c1)

cat("\n Coordenadas de las columnas \n")

print(acp$co)

# contribucion de cada invididuo a la inercia proyectada sobre un eje

cat("\n Contribuciones de las columnas a los ejes \n")


print(acpI$col.abs/100)

cat("\n Calidad de representacion de las columnas \n")

print(acpI$col.rel/100)

cat("\n Calidad de representacíon de las columnas en el plano \n")

print(acpI$col.cum/100)

("\n Coordenadas de las filas \n")

print(acp$li)

cat("\n Contribuciones de las filas a los ejes \n")

print(acpI$row.abs/100)

cat("\n Calidad de representacíon de las filas en los ejes \n")

print(acpI$row.rel/100)

cat("\n Calidad de representaci ́on de las filas en el plano \n")

print(acpI$row.cum/100)


s.corcircle(acp$co,sub="whiskey - circulo de correlaciones")


library(FactoClass)
library(FactoMineR)
library(MASS)
library(ade4)
library(ca)
library(GDAtools)
library(writexl)

data(DogBreeds)


# 1b
m <- as.matrix(DogBreeds)
compare = t(combn(nrow(m),2,FUN=function(x)m[x[1],]==m[x[2],]))
rownames(compare) = combn(nrow(m),2,FUN=function(x)paste0("Raza",x[1],"_raza",x[2]))
compare


for(i in 1:nrow(compare)){
 
  for(j in 1:7){
    
    if(compare[i,j]==TRUE){
      compare[i,j] = 1
    }else{
      compare[i,j] = 0
    }
    
  } 
  
}
compare <- as.matrix(compare)

k <- rowSums(compare);k

for(i in 1:length(k)){
  
  if(k[i]== 7){
    print(k[i])
  }
}


#3 ACM Burt

ACMB <- mjca(DogBreeds[,-7], nd = NA)

burt <- as.data.frame(ACMB$Burt)

write_xlsx(burt,"C:/Users/yosef/OneDrive/Escritorio/Ruptela/burt.xlsx")

pesos <- c(5, 8, 14)
barplot(pesos, main="Distribución de razas de perro según su categoría de peso", ylab= "Cantidad de razas", xlab="Categorias de pesos de perros", names.arg = c("Pesado", "Mediano", "Ligero"), col=c(5,4,3), cex.main=0.7, cex.lab=1, cex.sub=1)

# ACM

ACM <- MCA(DogBreeds, quali.sup=c(7))
plot(ACM, col.ind = "blue", col.var = "red", title = "Analisis de correspondencias múltiples")
summary(ACM, npc=3)


# 5 Categorias que tienen una mayor contribucion al primer eje y que signo son

contrib_Dim1 <- as.matrix(ACM[["var"]][["contrib"]])

sort(contrib_Dim1[,1], decreasing = T)
mean(contrib_Dim1[,1])


# 6 Categorias que tienen un coordenadas más importantes al primer eje y que signo son

cos2_var <- as.matrix(ACM[["var"]][["cos2"]])
coord_var<- as.matrix(ACM[["var"]][["coord"]])

# 7  ¿Cu ́ales son las razas que se encuentran m ́as alejadas del origen?
# ¿Cu ́ales sonsus coordenadas sobre el primer eje

coord_ind<- as.matrix(ACM[["ind"]][["coord"]])


distancias <-c()

for (i in 1:nrow(coord_ind)){
  
  distancias[i] <- sqrt(coord_ind[i,1]^2 +coord_ind[i,2]^2)
  
}

distancias <- as.matrix(distancias)

rownames(distancias)<- rownames(coord_ind)




# 10

plotellipses(ACM)


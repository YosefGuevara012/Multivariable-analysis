library(FactoClass)
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

write_xlsx(burt,"C:/Users/yosef/OneDrive/Escritorio/Ruptela/burt_2.xlsx")



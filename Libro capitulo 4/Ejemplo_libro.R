library ( FactoClass );
data ( admi );

# Tabla de contingencias

K <- unclass ( table ( admi $carr , admi $ estr ));

# Tabla de frecuencias relativas

F <-K/ sum (K)* 100; # o F <-prop . table (K)*100 , en porcentaje

Dn <-diag ( rowSums (F));

Dp <-diag ( colSums (F));

# Perfil fila

solve(Dn) %*%F

# Perfil columna

F %*% solve(Dp)


# Calculo de la inercia

inercia <- sum((F-A)^2/A)


# Calculo de X^2

chi_square <- n * inercia


curve ( dchisq (x ,12) , xlim =c(0 ,30) , las =1)
curve ( dnorm (x ,12 ,4.9) , col =" blue ",add = TRUE )
abline (v=c (26.7 ,29.19) , col =" orange ")
chisq.test (K) # prueba de independencia


A <- matrix ( c(1, 2, -1, 
               -1, 3, -1,
                2, 2, 4), nrow=3, byrow=TRUE)
B <- matrix ( c(3, 2, -1, 
                2, 3, 1, 
                -1, 1,3),nrow=3, byrow=TRUE)
C <- matrix ( c(2, 0,
               -1, 1,
                3, 2), nrow=3, byrow=TRUE)

# Matrix A
A

#Matrix B
B

# Matrix C
C


#1a

A + B

#1b

A - 2*B

#1c

t(A) + B

# 1d

A + C

# 1e

t(A + B)

#1f

t(3*t(A) - 2*B)

#2

P <- matrix ( c(3, 2, 1,
                2, 5, -1,
                1, -1, 3), nrow=3, byrow = TRUE)
Q <- matrix ( c(1, 2, 2, 1,
                1, 1, 1, 4,
                1, 1, 2, 1), nrow=3,byrow = TRUE)

R <- matrix( c(1, 2,
                -5, 2,
                3, -1,
                -2, 2), nrow=4, byrow = TRUE)



x <- c(1, 0, -1)

y <- c(2, 3, 2)

z <- c(-1, -2, -3, -4)

#2a

P*Q

#2b

P*Q*R

#2c

Q*t(R)

#2d

y*t(x)

#2e

t(x)*y

#2f

t(x)*P*y

#2h

P*(x + y)


#5

a <- matrix ( c(4, 2, 0,
                5, 3, 0,
                6, 9, 2), nrow=3, byrow=TRUE)

b <- matrix ( c(  1, 0.8, 0.5,
                0.8,   1, 0.6,
                0.5, 0.6,   1), nrow=3, byrow=TRUE)

c <- matrix ( c(5, 0, 0,
                0, 3, 0,
                0, 0, 1), nrow=3, byrow=TRUE)

d <- matrix ( c(1,  4, -1,
                3, 12, -3,
                0,  35, 7), nrow=3, byrow=TRUE)

e <- matrix ( c(2, 0, 4, 0,
                0, 3, 0, 5,
                5, 0, 1, 0,
                0, 4, 0, 1), nrow=4, byrow=TRUE)

f <- matrix ( c(2, 0, 1, 1, 1,
                0, 2, 3, 3, 3,
                1, 3, 1, 0, 0,
                1, 3, 0, 1, 0,
                1, 3, 0, 0, 1), nrow=5, byrow=TRUE)

#5a

det(a)

#5b

det(b)

#5c

det(c)

#5d

det(d)

#5e

det(e)

#5f

det(f)

#6

seis_a <- matrix ( c(5,  1, -2,
                     2,  6,  3,
                     -1, 0,  3), nrow=3, byrow=TRUE)

seis_b <- matrix ( c("a", "b", "b",
                     "b", "a", "b",
                     "b", "b", "a"), nrow=3, byrow=TRUE)

seis_c <- matrix ( c(5, 0, 0,
                     0, 8, 6,
                     0, 6, 5), nrow=3, byrow=TRUE)

seis_d <- matrix ( c(4, 3, 2, 1,
                     0, 3, 2, 1,
                     0, 0, 2, 1,
                     0, 0, 0, 1), nrow=4, byrow=TRUE)


#6a

solve(seis_a)

#6b

solve(seis_b)

#6c

solve(seis_c)

#6d

solve(seis_d)

#7

siete_a <- matrix ( c(1,  0,  2,  1,
                      1,  1,  2,  0,
                      1, -1,  2,  2,
                      1,  1,  2,  0), nrow=4, byrow=TRUE)

siete_b <- matrix ( c( 1, 2,  3,  4, 5,
                       1, 0, -1,  3, 1,
                       2, 1,  1,  0, 1,
                       0, 3,  8, -5, 3,
                      -1, 2,  6, -2, 3,
                       1, 1,  2, -3, 0), nrow=6, byrow=TRUE)


#7a

siete_a <- qr(siete_a)

siete_a$rank

#7b

siete_b <- qr(siete_b)

siete_b$rank


#12

magnitude.vector <- function (vector){
  
  magnitude <- sqrt(vector[1]**2 + vector[2]**2 + vector[3]**2 + vector[4]**2)
  
  return (magnitude)
  
}

t <- c(0.5, 0.5, 0.5, 0.5)

u <- c(1, 0, -1, 0)

v <- c(sqrt(2)/2, 0, sqrt (2)/2, 0)

if (t%*%u == 0){
  
  magnitude_t <- magnitude.vector(t)
  magnitude_u <- magnitude.vector(u)
  

  
  normal_t <- t/magnitude_t
  normal_u <- u/magnitude_u 
  
  cat("t y u son ortogonales y su vectores ortonormales son normal_t " , normal_t ," y normal_u", normal_u)
  
}else{
  "t y u NO son ortogonales"
  
}

if (t%*%v == 0){
  "t y v son ortogonales"
  2+2
}else{
  "t y v NO son ortogonales"
  
}


if (u%*%v == 0){
  "u y v son ortogonales"
}else{
  "t y u NO son ortogonales"
  
}

#13

Trece <- matrix ( c( sqrt(3)/2, 1/2, 0,
                     -sqrt(2)/2, sqrt(6)/4, -sqrt(2)/2,
                     -sqrt(2)/2, sqrt(6)/4,  sqrt(2)/2), nrow=3, byrow=TRUE)

Trece


library('Ryacas')
library('rootSolve')

hermite<- function(x,y,dy) {
  n <- length(x)
  RE <- matrix(0,2*n,2*n)
  
  for (i in 1:n) {
    RE[1,i*2-1] <- y[i]
    RE[1,i*2] <- y[i]
    RE[2,i*2-1] <- dy[i]
  }
  for (i in 1:(n-1)) {
    RE[2,i*2] <- (RE[1,i*2+1] - RE[1,i*2])/(x[i+1] - x[i])
  }
  
  for (i in 3:(n*2)) {
    for (j in 1:(n*2-i+1)) {
      RE[i,j] <- (RE[i-1,j+1] - RE[i-1,j])/( x[ceiling((i+j-1)/2)] - x[ceiling(j/2)] )
    }
  }
  x2<- Sym('x')
  polinom_hermite = y[1]
  multipliexp = expression(1)
  for (i in 2:(n*2)) {
    multipliexp = multipliexp*(x2- x[ceiling((i-1)/2)])
    polinom_hermite = polinom_hermite + RE[i,1]*multipliexp
  } 
  polinom_hermite = Simplify(polinom_hermite)
}

x <- c(0, 1, 2)
y <- c(10, 15, 5)
dy <- c(1,0,0)

her_resul = hermite(x,y,dy)
print(her_resul)
library('Ryacas')
library('rootSolve')
hermite <- function(x,y,dy) {
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
	x2 <- Sym('x')
	polinom_hermite= y[1]
	multipliexp = expression(1)
	for (i in 2:(n*2)) {
		multipliexp = multipliexp*(x2 - x[ceiling((i-1)/2)])
		polinom_hermite= polinom_hermite + RE[i,1]*multipliexp
	} 
	polinom_hermite = Simplify(polinom_hermite)
}


x2 <- Sym('x')
f <- log(x2)
df <- deriv(f)
x <- c(1,2) 
y <- Eval(f, list(x = x))
dy <- Eval(df, list(x = x))

her_resul = hermite(x,y,dy)
print(her_resul)

puntos_x <- seq(0.5, 4, 0.05)
puntos_y <- Eval(f,list(x = puntos_x))

herresul_puntos_y <- Eval(her_resul,list(x = puntos_x))


png('interpolacion.png')
plot(puntos_x,puntos_y,type='l',xlab='x',ylab='y')
lines(puntos_x,herresul_puntos_y,col='red')
points(x,y)
legend('topleft',legend=c('ln(x)','P(x)'),col=c('blue','red'), lty=c(1,1), cex=0.8)

f_error <- deriv(((her_resul) - f)^2)
zero <- uniroot.all(function(x) {Eval(f_error,list(x=x))},c(1,2))[1]
error <- abs(Eval(her_resul- f,list(x=zero)))
print(error)
library(tseries)
set.seed(123)
Tt=500
gamma = 0.5
a = lapply(1:10000, function(x)
{
  rw= cumsum(rnorm(Tt))
  y = gamma*rw + rnorm(Tt)
  
  ts(cbind('y'=y, 'x' = rw),start=1)
}
  )

slope_reg = sapply(a, function(d) coefficients(lm(y~x, data = d))[2])

plot(a[[3]],plot.type='single',col = c('red', 'blue'), ylab = '')
legend('topright',legend = c('y', 'x'), col = c('red','blue'), lty = c(1,1))
plot(density(slope_reg),main = expression('Distribuição de '~hat(gamma)~' em amostras repetidas'), xlab='')

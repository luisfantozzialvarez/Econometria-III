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

t_stat = sapply(a, function(d) {mod = summary(lm(y~x, data = d)); mod$coefficients[2,3] - gamma/mod$coefficients[2,2]})

plot(a[[3]],plot.type='single',col = c('red', 'blue'), ylab = '')
legend('topright',legend = c('y', 'x'), col = c('red','blue'), lty = c(1,1))
plot(density(slope_reg),main = expression('Distribuição de '~over(hat(gamma)-gamma,se(hat(gamma)))~' em amostras repetidas'), xlab='', cex.main=0.9)

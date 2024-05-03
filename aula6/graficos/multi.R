set.seed(123)

v = matrix(c(1,1,1,2),nrow=2)

a = t(chol(v))

pd = a%*%matrix(rnorm(1000), nrow=2)

pd = t(pd)

rb = ts(pd)

plot(rb)

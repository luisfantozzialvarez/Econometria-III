#Fixando semente para permitir a replicação
set.seed(123)

#Modelo aditivo da forma
#a*sin(2*pi*t/12)  + u_t, onde u_t é passeio aleatório
add_model = ts(sin(2*pi*1:500/12) + cumsum(rnorm(500)),start= 0)
plot(add_model)
plot(diff(add_model))

#Modelo multiplicativo da forma
#a*t*u_t, onde u_t é passeio aleatório
mult_model = ts(1:500*cumsum(rnorm(500)),start= 0)
plot(mult_model)
plot(diff(mult_model))
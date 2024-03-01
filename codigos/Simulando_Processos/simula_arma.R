#Função para simular um processo da forma
#Y_t = alpha + rho*y_{t-1} + epsilon_t
#onde epsilon_t ~ N(0, sigma^2) iid
#Se |rho| < 1, processo é inicializado como y_0 ~ N(alpha/(1-rho), sigma^2/(1-rho^2))
#do contrário, processo inicia em alpha.
ar.sim <- function(Nperiods, rho, alpha = 0, sigma = 1)
{
  if(abs(rho)<1)
    y = rnorm(1, alpha/(1-rho), sd = sigma/sqrt(1-rho^2)) else y = alpha
    
    epsilon = rnorm(Nperiods, mean = 0, sd = sigma)
    
    for(j in 1:Nperiods)
      y = c(y, alpha + rho*y[j] + epsilon[j])
    
    return(y)
}

#Função para simular um MA(q) Gaussiano. O comprimento do vetor q determina a ordem do MA
ma.sim <- function(Nperiods, psi = c(), alpha = 0, sigma = 1)
{
  noise = rnorm(Nperiods+length(psi), sd = sigma)
  
  vec_lg = sapply(0:length(psi), function(x){
    noise[(length(psi)-x+1):(length(noise)-x)]
  })
  
  mov = as.numeric(vec_lg%*%c(1,psi))
}

#Fixa semente do simulador - para replicar resultados
set.seed(13579)

#Simulando ruído branco
rb = ts(ar.sim(500, rho = 0), start=0)
plot(rb)

#Simulando AR1
ar1 = ts(ar.sim(500, rho = 0.7), start=0)
plot(ar1)

#Simulando MA2
#Vamos usar a função arima.sim, do pacote stats (carregando, por padrão, no R)
ma2 = ts(ma.sim( 500, psi = c(1,0.5) ),start = 0)
plot(ma2)

#Simulando ARMA(1,2)
#usamos função arima.sim do pacote stats
arma12 = ts(arima.sim(list(ar=0.7, ma =c(1,0.5) ),n = 500),start= 0)
plot(arma12)

#Processo determinístico
det = 0.01*0:500 + rb
plot(ts(det,start=0))

#Passeio aleatorio
rw = ts(ar.sim(500, rho=1), start = 0)
plot(rw)

#Quebra de nível
break_mean = ts(c(ar.sim(250, rho=0),2+ar.sim(250,rho=0)), start = 0)
plot(break_mean)

#Quebra de variância
break_sd = ts(c(ar.sim(250, rho=0),2*ar.sim(250,rho=0)), start = 0)
plot(break_sd)

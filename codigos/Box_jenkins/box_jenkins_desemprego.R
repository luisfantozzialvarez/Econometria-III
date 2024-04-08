#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Documents/GitHub/Econometria-III/codigos/")

#Carregando pacotes necessários. Instale-os se não os possuir
library(forecast) #Pacote forecast para métodos de previsão
library(sandwich) #Erros padrão HAC
library(lmtest) #Testes com erros padrão robustos

#Carregando dados
data = read.csv2("Raiz_Unitaria/desemprego.csv")

desemprego = ts(data = data$Desemprego.PNADC., start = c(2012,03), frequency = 12)

plot(desemprego)


#Da aula anterior, sabemos que a série apresenta raiz unitária, e de:
trend = 1:(length(desemprego)-1)
regg = lm(diff(desemprego)~trend)
coeftest(regg, vcov. = vcovHAC)
#Série não apresenta componentes determinísticos na primeira diferença.
#Dessa forma, estimaremos o ARMA em primeiras diferenças (ARIMA(p,1,q)) sem intercepto ou tendência.

#FAC e FACP do processo estacionarizado (no nosso caso, basta tirar as diferenças, 
#pois n há não estacionariedade determinística uma vez que o processo é diferenciado)
acf((diff(desemprego)), lag.max = 60)
pacf((diff(desemprego)), lag.max = 60)

#Sendo conservador, da inspeção visual da FAC e FACP, incluímos, na parte não sazonal:
pmax = 13
qmax = 16

#A parte sazonal parece compatível com, no máximo, um SAR(2). Vamos considerar:
spmax = 2

#Vamos usar uma função auxiliar, que incluí no arquivo box_jenkins_parallel.R
source("Box_jenkins/box_jenkins_parallel.R")
tabela = arima.est.parallel(desemprego, pmax, qmax, d=1, pseas_max = spmax, include.constant = F, include.trend = F,
                   signif = 0.1, lags.lbox = c(20,30))

#Removendo modelos que não convergiram
tabela = tabela[tabela$Converged,]

#ARIMA(1,1,0)(2,0,0)_12 passa em nosso diagnóstico Vamos estimá-lo:
modelo = Arima(desemprego, order =c(1,1,0), seasonal = c(2,0,0), include.constant = F, include.drift = F)
summary(modelo)

checkresiduals(modelo)

forecast(modelo)
plot(forecast(modelo))




#Vamos aproveitar e calcular a decomposição de Beveridge-Nelson
lp =c()
for(year in 2013:2021)
  for(month in 1:12)
  {
    valor = forecast(Arima(window(desemprego,end=c(year,month)), model = modelo), h=1000)
    lp = c(lp,valor$mean[1000])
  }

lp = ts(lp, start = c(2013,1),frequency=12)

cycle_bn = desemprego - lp

plot(cycle_bn)



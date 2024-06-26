#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Documents/GitHub/Econometria-III/codigos/")

#Carregando pacotes necessários. Instale-os se não os possuir
library(forecast) #Pacote forecast para métodos de previsão
library(sandwich) #Erros padrão HAC
library(lmtest) #Testes com erros padrão robustos

#Carregando dados
data = read.csv2("Raiz_Unitaria/divida_liquida.csv")

divida = ts(data = data$Divida.liquida.do.Setor.publico, start = c(2001,12), frequency = 12)

plot(divida)


#Da aula anterior, sabemos que a série apresenta raiz unitária, e de:
trend = 1:(length(divida)-1)
regg = lm(diff(divida)~trend)
coeftest(regg, vcov. = vcovHAC)
#Série não apresenta componentes determinísticos na primeira diferença.
#Dessa forma, estimaremos o ARMA em primeiras diferenças (ARIMA(p,1,q)) sem intercepto ou tendência.

#FAC e FACP do processo estacionarizado (no nosso caso, basta tirar as diferenças, 
#pois n há não estacionariedade determinística uma vez que o processo é diferenciado)
acf(diff(divida), lag.max = 40)
pacf(diff(divida), lag.max = 40)

#Da inspeção visual da FAC e FACP, o processo parece ser compatível com MA(2)
#No entanto, FACP, apesar de aparentemente exibir decaimento, poderia ser compatível com um componente
#AR(12)

#Vamos ser conservadores e considerar pmax = 12 e qmax = 2, e calcular todos os modelos:
pmax = 12
qmax = 2

#Vamos usar uma função auxiliar, que incluí no arquivo box_jenkins_parallel.R
source("Box_jenkins/box_jenkins_parallel.R")
tabela = arima.est.parallel(divida, pmax, qmax, d=1, include.constant = F, include.trend = F,
                   signif = 0.1, lags.lbox = c(20,30))

#Removendo modelos que não convergiram
tabela = tabela[tabela$Converged,]

#MA(2) e ARMA(5,2) parecem razoáveis. Vamos estimá-lo:
modelo = Arima(divida, order =c(0,1,2), include.constant = F, include.drift = F)
summary(modelo)

checkresiduals(modelo)

forecast(modelo)
plot(forecast(modelo))

# ARMA(5,2)
modelo2 = Arima(divida, order =c(5,1,2), include.constant = F, include.drift = F)
summary(modelo2)

checkresiduals(modelo2)

forecast(modelo2)
plot(forecast(modelo2))

#Resultados parecidos: vamos ficar com o modelo mais parcimonioso

#Vamos usar o modelo para calcular a decomposição de beveridge Nelson
#Vamos fazer a decomposição a partir de Jan/2003
lp =c()
for(year in 2003:2023)
  for(month in 1:12)
  {
    valor = forecast(Arima(window(divida,end=c(year,month)), model = modelo), h=1000)
    lp = c(lp,valor$mean[1000])
  }

lp = ts(lp, start = c(2003,1),frequency=12)

cycle_bn = divida - lp

plot(cycle_bn)



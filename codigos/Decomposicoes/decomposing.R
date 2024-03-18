#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Documentos/GitHub/Econometria-III/codigos/Decomposicoes/")

#Carregando pacotes necessários. Instale-os se não os possuir
library("seasonal") #Arima X13-Seats
library("mFilter") #Filtro HP

data = read.csv2("desemprego.csv")

desemprego_brasil = ts(data = data$Desemprego.PNADC., start = c(2012,03), frequency = 12)

plot(desemprego_brasil) #Série em nível
plot(diff(desemprego_brasil)) #Gráfico da primeira diferença: modelo aditivo parece razoável

#1. Ajuste de média móvel 
decomposicao = decompose(desemprego_brasil, type = "additive")
desemprego_ajustado_cma = desemprego_brasil - decomposicao$seasonal

plot(desemprego_brasil)
lines(desemprego_ajustado_cma, col = 'blue')

#2. Ajuste via ARIMA X13-Seats
modelo_sazonal = seas(desemprego_brasil)

plot(modelo_sazonal)

desemprego_x13 = predict(modelo_sazonal)

#Filtro HP
filtrado = hpfilter(desemprego_x13,129600,type = "lambda")

plot(desemprego_x13, col='red')
lines(filtrado$trend,col='blue')


#Exemplificando instabilidade de ponta com dados mais recentes
data = read.csv("desemprego_atualizado.csv")

desemp_atualizado = ts(data$Desemprego.PNADC., start = c(2012,03), frequency = 12)
desemp_x13 = predict(seas(desemp_atualizado))

desemp_menor = window(desemp_x13, end = c(2021,12))

filtrado_menor =  hpfilter(desemp_menor,129600,type = "lambda")$trend
filtrado_cheio =  hpfilter(desemp_x13,129600,type = "lambda")$trend

plot(desemp_x13)
lines(filtrado_menor, col = 'red')
lines(filtrado_cheio, col = 'blue')

legend('topleft', c('Tendência HP (estimação até dez/2021)', 'Tendência HP (estimação até jan/2024)'),
       col = c('red','blue'),lty = c(1,1), cex = 0.7)


#Proposta de Hamilton
base = cbind(desemp_x13,do.call(cbind, lapply(-24-0:3, function(x) lag(desemp_x13, x))))
colnames(base) = c('desemp', paste('L',0:3,sep=''))

model = lm(desemp~., data = base)

base = cbind(base, 'hamilton_cycle' =base[,1]- predict(model, base))

plot(base[,"hamilton_cycle"], col = 'blue')
abline(h=0)
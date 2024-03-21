#Decompondo séries de tempo


#Carregando pacotes necessários. Instale-os se não os possuir
library("seasonal") #Arima X13-Seats
library("mFilter") #Filtro HP

air = AirPassengers

plot(air) #Série em nível
plot(diff(air)) #Gráfico da primeira diferença: modelo multiplicativo parece o mais apropriado

#1. Ajuste de média móvel 
decomposicao = decompose(air, type = "multiplicative")
air_ajustado_cma = air/decomposicao$seasonal

plot(air)
lines(air_ajustado_cma, col = 'blue')

#2. Ajuste via ARIMA X13-Seats
modelo_sazonal = seas(air)

plot(modelo_sazonal)

air_x13 = predict(modelo_sazonal)

#Filtro HP: note que vamos usar log, pois modelo é multiplicativo
filtrado = hpfilter(log(air_x13),129600,type = "lambda")

plot(log(air_x13), col='red')
lines(filtrado$trend,col='blue')


#Proposta de Hamilton
base = cbind(air_x13,do.call(cbind, lapply(-24-0:3, function(x) lag(air_x13, x))))
colnames(base) = c('l_air', paste('L',0:3,sep=''))

#Passando para log (modelo multiplicativo!)
base = log(base)

model = lm(l_air~., data = base)

base = cbind(base, 'hamilton_cycle' =base[,1]- predict(model, base))

plot(base[,"hamilton_cycle"], col = 'blue')
abline(h=0)
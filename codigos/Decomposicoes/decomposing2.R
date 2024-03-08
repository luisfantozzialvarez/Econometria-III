#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Dropbox/Ensino/Applied Econometrics/SeriesdeTempo1/codigos")

#Carregando pacotes necessários. Instale-os se não os possuir
library("seasonal") #Arima X13-Seats
library("mFilter") #Filtro HP

data = read.csv2("pib_tri.csv")

pib_brasil = ts(data = data$X22099...Quarterly.GDP...observed.data...GDP.at.market.prices...Index, start = c(1995,01), frequency = 4)

plot(pib_brasil) #Série em nível
plot(diff(pib_brasil)) 
#Gráfico da primeira diferença: modelo multiplicativo parece ok, variabilidademuda a partir de 2015
#Vale checar também com modelo aditivo.

#1. Ajuste de média móvel 
decomposicao = decompose(pib_brasil, type = "multiplicative")
pib_ajustado_cma = pib_brasil/decomposicao$seasonal

plot(pib_brasil)
lines(pib_ajustado_cma, col = 'blue')

#2. Ajuste via ARIMA X13-Seats
modelo_sazonal = seas(pib_brasil)

plot(modelo_sazonal)

pib_x13 = predict(modelo_sazonal)

#Filtro HP
filtrado = hpfilter(log(pib_x13),1600,type = "lambda")

plot(log(pib_x13), col='red')
lines(filtrado$trend,col='blue')
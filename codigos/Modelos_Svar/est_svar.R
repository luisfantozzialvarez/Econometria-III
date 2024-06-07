setwd("~/Documents/GitHub/Econometria-III/codigos/Modelos_Svar")

library(vars)

for(name in c("expectativa_6","expectativa_12", "selic", "ipca", "desemprego", "ibc",
              "cambio_nominal"))
{
  print(name)
  ddd = read.csv(paste(name,".csv",sep=""))
  
  serie = assign(name, ts(ddd[,2], start = strsplit(as.character(ddd[1,1]),"[.]")[[1]], frequency = 12 ))
  
  serie = window(serie, start = c(2003,01), end = c(2024,03))
  assign(name, serie)
}

#Anualizando IPCA
ipca = 100*((1+ipca/100)^(12) -1)

#Trabalhando com log(IBC)
ibc = log(ibc)

#Trabalhando com log(cambio)
cambio_nominal  = log(cambio_nominal)

#De aulas anteriores, sabemos que IBC e câmbio apresentam raiz unitária e que
#Selic e expectativa são trend-stationary.
#Além disso, note que
acf(diff(ibc),lag.max=40)
# ibc apresenta sazonalidade.

# Ajustamos um VAR com variação do ibc e as demais variáveis em nível, tendência linear e 
# dummies sazonais
dados = cbind(diff(ibc),  ipca,selic,expectativa_12,expectativa_6,diff(cambio_nominal))
dados = window(dados, start = c(2003,02))

VARselect(dados, lag.max =ceiling(12*(nrow(dados)/100)^(1/4)), type = "both", season=12)


#Vamos trabalhar com a defasagem 4, escolhida pelo AIC (vale fazer testes)
#Vamos identificar a FRI do choque monetário, sob identificação recursiva.
#No nosso caso, pol monetária reage contemporaneamente a choques na IS e na PC,
# mas atividade e inflação não respondem a choques monetários contemporaneamente
#Além disso, expectativas e câmbiorespondem contemporaneamente a choques monetários, mas
#a política monetária só reage de forma defasada às expectativas e ao câmbio

var_reduzido = VAR(dados, 4, type = "both",season=12)

#Fixando semente para as simulações usadas no cálculo dos intervalos de confiança
set.seed(123)
fri = irf(var_reduzido, impulse = "selic", n.ahead = 36, ci = 0.95, runs = 1000 )

plot(fri)


#Blanchard Quah brasileiro
dados_bq = cbind(diff(ibc), diff(desemprego))
dados_bq = window(dados_bq, start= c(2012,04))

VARselect(dados_bq, lag.max=ceiling(12*(nrow(dados_bq)/100)^(1/4)), type='none',season=12)
var_bq = VAR(dados_bq,4,season=12)

mod = BQ(var_bq)

fri = irf(mod, n.ahead = 48, ci = 0.95, runs = 1000, cumulative = T )
plot(fri)



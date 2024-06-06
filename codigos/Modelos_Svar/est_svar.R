setwd("~/Documentos/GitHub/Econometria-III/codigos/Modelos_Svar")

library(vars)

for(name in c("expectativa", "selic", "ipca", "desemprego", "ibc"))
{
  print(name)
  ddd = read.csv(paste(name,".csv",sep=""))
  
  serie = assign(name, ts(ddd[,2], start = strsplit(as.character(ddd[1,1]),"[.]")[[1]], frequency = 12 ))
  
  serie = window(serie, start = c(2012,03), end = c(2024,03))
  assign(name, serie)
}

#Anualizando IPCA
ipca = 100*((1+ipca/100)^(12) -1)

#Trabalhando com log(IBC)
ibc = log(ibc)

#De aulas anteriores, sabemos que  IBC desemprego apresenta raiz unitária, e que
#Selic e expectativa são trend-stationary.
#Além disso, note que
acf(diff(desemprego))
# desemprego apresenta sazonalidade.

# Ajustamos um VAR com variação do desemprego e as demais variáveis em nível, tendência linear e 
# dummies sazonais
dados = cbind( diff(ibc), diff(desemprego),expectativa, ipca, selic)
dados = window(dados, start = c(2012,04))

VARselect(dados, lag.max =20, type = "both", season=12)


#Vamos trabalhar com a defasagem 6, escolhida pelo AIC (vale fazer testes)
#Vamos identificar a FRI do choque monetário, sob identificação recursiva.
#No nosso caso, pol monetária reage contemporaneamente a choques na IS e na PC,
# mas atividade e inflação não respondem a choques monetários contemporaneamente
#Além disso, expectativas respondem contemporaneamente a choques monetários, mas
#a política monetária só reage de forma defasada às expectativas

var_reduzido = VAR(dados, 6, type = "both",season=12)

#Fixando semente para as simulações usadas no cálculo dos intervalos de confiança
set.seed(123)
fri = irf(var_reduzido, impulse = "selic", n.ahead = 36, ci = 0.95, runs = 1000 )

plot(fri)


#Blanchard Khan



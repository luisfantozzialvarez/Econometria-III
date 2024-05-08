#Fixando ambiente
setwd("~/Documents/GitHub/Econometria-III/codigos/cointegracao")

library("CADFtest")
library("car")
library("lmtest")
library("sandwich")
library("vars")

#Pacote para testes de cointegração
library('urca')


ibc = read.csv2('ibc.csv')
ibc = ts(log(ibc[,2]), start=c(substring(ibc[1,1],1,4),substring(ibc[1,1],6,7)),frequency=12)

m1 = read.csv2('m1.csv')
m1 = ts(log(m1[,2]), start=c(substring(m1[1,1],1,4),substring(m1[1,1],6,7)),frequency=12)

igp = read.csv2('igp.csv')
igp = ts(log(igp[,2]), start=c(substring(igp[1,1],1,4),substring(igp[1,1],6,7)),frequency=12)


dados = cbind(ibc,m1,igp)
dados = window(dados, start = c(2003,1), end = c(2024,2))

#Ao rodar o procedimento sequencial, notamos que todas as séries são I(1),
#e que m1 e igp exibem drift (intercepto na primeira diferença)


#Vamos testar se há uma relação de longo prazo entre as variáveis 
#Vamos supor que igp participa da relação, visto o que esperamos de neutralidade do
#longo prazo
teste = lm(igp~m1+ibc, data=dados)
res= ts(residuals(teste),start = c(2003,1),frequency=12)
ur.pp(res, type = c("Z-tau"))

#Olhando a tabela dos slides, valor crítico para n=3, com drift do lado direito (Caso 3)
#a 1% é: -4.36. Logo, como -4.66 < -4.36, rejeitamos a nula -> há cointegração.


VARselect(dados, lag.max = 20, type = c("const"),season = 12)
summary(ca.jo(dados, type = c('eigen'), ecdet = c('none'),spec='transitory', K=5, season = 12))




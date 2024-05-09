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


###############################
#Procedimento de Engle Granger#
###############################

#Vamos testar se há uma relação de longo prazo entre as variáveis 
#Vamos considerar o caso em que igp deve participar da relação (caso ela exista), 
#visto o que esperamos da neutralidade do longo prazo e da teoria quantitativa
teste = lm(igp~m1+ibc, data=dados)
res= ts(residuals(teste),start = c(2003,1),frequency=12)
ur.pp(res, type = c("Z-tau"))

#Olhando a tabela dos slides, valor crítico para n=3, com drift do lado direito (Caso 3)
#a 1% é: -4.36. Logo, como -4.66 < -4.36, rejeitamos a nula -> há cointegração.

#Para os testes de hipótese, sabemos que é curcial que (Delta m1 e Delta ibc) não sejam
#correlacionados, contemporanea ou extemporaneamente, com o erro da relação de longo prazo

#Vamos checar isso
est = cbind(diff(dados[,c("ibc","m1")]), res)
colnames(est) = c('d_ibc','d_m1', 'err')
est= est[-1,]
acf(est)

#Alguma evidência pontual de correlação, o que sugere que devemos tomar cuidado com a 
#inferência no vetor de cointegração. 

#Intervalos de confiança para os coeficientes da equaçào
coefci(teste, vcov. = vcovHAC)

#Intervalos compatíveis com a equação quantitativa




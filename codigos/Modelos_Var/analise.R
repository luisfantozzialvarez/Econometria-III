setwd("~/Documentos/GitHub/Econometria-III/codigos/Modelos_Var")

library("CADFtest")
library("car")
library("lmtest")
library("sandwich")

#Pacote para VAR
library("vars")

exp = read.csv("expectativas.csv")
exp = ts(exp[,2], start = c(exp[1,1]%/%1,exp[1,1]%%1*100), frequency = 12)

juros = read.csv("selic.csv")
juros = ts(juros[,2], start = c(juros[1,1]%/%1,juros[1,1]%%1*100), frequency = 12)

infl = read.csv("nucleo_ipca.csv")
infl = ts(infl[,2], start = c(infl[1,1]%/%1,infl[1,1]%%1*100), frequency = 12)

#Anualizando variação mensal
infl=100*((1+infl/100)^(12)-1)


dados = cbind(exp, juros, infl)
dados = window(dados, start = c(2001,07), end = c(2024,03))

plot(dados)

#Vamos verificar a fonte de não estacionariedade dos dados 

##########################
#Expectativas de inflação#
##########################

#TESTE DE RAIZ UNITÁRIA
teste = CADFtest(dados[,1], type = "trend", max.lag.y = ceiling(12*(length(dados[,1])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
#Rejeitamos a nula de raiz unitária, aos níveis de significância convencionais. 

#TESTE DE COMPONENTE DETERMINÍSTICO
trnd = 1:nrow(dados)
coeftest(lm(dados[,1]~ trnd), vcov = vcovHAC)
#Evidência de tendência determinística na série em nível, a 5%


#######
#SELIC#
#######

#TESTE DE RAIZ UNITÁRIA
teste = CADFtest(dados[,2], type = "trend", max.lag.y = ceiling(12*(length(dados[,2])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
#Não rejeitamos a nula, nesta primeira etapa, nos níveis convencionais.

#Vamos fazer o teste F
print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))
#Não rejeitamos H0, do teste F, a 5%, olhando na tabela dos slides

#Rodando agora no modelo com drift somente
teste = CADFtest(dados[,2], type = "drift", max.lag.y = ceiling(12*(length(dados[,2])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
#Concluímos que não há raiz unitária

#TESTE DE COMPONENTE DETERMINÍSTICO
trnd = 1:nrow(dados)
coeftest(lm(dados[,2]~ trnd), vcov = vcovHAC)
#Evidência de tendência determinística na série em nível aos níveis de significância convencionais



##################
#Taxa de inflação#
##################

#TESTE DE RAIZ UNITÁRIA
teste = CADFtest(dados[,3], type = "trend", max.lag.y = ceiling(12*(length(dados[,3])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
#Rejeitamos a nula de raiz unitária, a 5 % 

#TESTE DE COMPONENTE DETERMINÍSTICO
trnd = 1:nrow(dados)
coeftest(lm(dados[,3]~ trnd), vcov = vcovHAC)
#Não  há evidência de tendência determinística na série em nível, a 5%


#Vamos analisar as FACs dos processos, para detectar a presença de sazonalidade.
#Note que os dois primeiros processos devem ser detrended
resid = residuals(lm(dados[,1:2]~ trnd))
dados = cbind(dados,resid)
colnames(dados)[4:5] = paste("r", c("exp","juros"), sep="_")


acf(dados[,3:5],lag.max=40)
#Não parece haver evidência de componentes sazonais no processo

#Vamos estimar um modelo VAR para as séries.

#Há duas alternativas aqui: ou fazemos com inflação + juros e selic detrended,
#ou usamos as séries originais e incluímos uma tendência linear diretamente no modelo vetorial.

#Desvantagem do segundo método é que sabemos que tendência linear na inflação é desnecessária
#No entanto, estimação conjunta dos parâmetros pode ser vantajosa para as duas outras séries.
#Logo, vantagens relativas não são claras.

#Vamos incluir a tendência linear, pois facilita o cálculo de previsões via R (vale testar a outra alternativa).

#Selecionando ordem do VAR inicial
VARselect(dados[,1:3], type = "both", lag.max = ceiling(12*(nrow(dados)/100)^(1/4)))

#Vamos verificar o modelo do VAR com SC
modelo = VAR(dados[,1:3],type = 'both', p = 2)

summary(modelo)
help(roots)
any(roots(modelo)>1)
#Processo estimado não apresenta raiz unitária

#Vamos fazer o teste de razão de verossimilhança
stat = 2*(logLik(modelo) - logLik(VAR(dados[,1:3],type = 'both', p = 1)))
print(stat)
#Valor crítico é, a 5%
qchisq(0.95, 9)
#Rejeitamos nula de que segunda defasagem é inócua.

#Vamos verificar o teste de Portmanteau para autocorrelação dos resíduos
serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))
#Rejeitamos a hipótese nula: VAR(2) joga muita informação fora!!!

#Vamos considerar o VAR com base no AIC
modelo = VAR(dados[,1:3],type = 'both', p = 14)

summary(modelo)
help(roots)
any(roots(modelo)>1)
#Processo estimado não apresenta raiz unitária

#Vamos fazer o teste de razão de verossimilhança
stat = 2*(logLik(modelo) - logLik(VAR(dados[,1:3],type = 'both', p = 13)))
print(stat)
#Valor crítico é, a 5%
qchisq(0.95, 9)
#Evidência de que precisamos desta defasagem

serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))
#Parecemos melhorar algo em relação ao modelo menor

normality.test(modelo)
#Rejeitamos claramente normalidade dos erros

#Podemos procurar especificações com ordem maiores, mas a dimensionalidade vai tender a piorar
#Vamos ficar com o modelo do AIC (vale comparar os resultados com o do BIC e HQ também)

predicoes = predict(modelo, ci = 0.95)

fanchart(predict(modelo, ci = 0.95), plot.type = 'single')


### VAMOS INCORPORAR OS DADOS DE ABRIL PARA A SELIC
dado_up = read.csv('selic_atualizado.csv')
at= dado_up[nrow(dado_up),2]
eup = summary(modelo)$covres[2,]%*%solve(summary(modelo)$covres[2,2])%*%(at - predicoes$fcst$dados.juros[1,1])

vlh =t(do.call(c,lapply(predicoes$fcst, function(x) x[1,1])) + eup)

mod_up = predict(VAR(ts(rbind(dados[,1:3],vlh),end=c(2024,04),frequency=12) ,type = 'both', p = 14), ci = 0.95)


#Testes de causalidade de Granger e contemporânea
causality(modelo,cause = 'dados.juros')


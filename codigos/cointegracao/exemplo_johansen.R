#Fixando ambiente
setwd("~/Documentos/GitHub/Econometria-III/codigos/cointegracao")

library("CADFtest")
library("car")
library("lmtest")
library("sandwich")
library("vars")

#Pacote para testes de cointegração
library('urca')
library('vars')

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

#Evidência de sazonalidade
acf(diff(dados),lag.max=40)

#Selecionando coeficientes com base no VAR em nível. Note que, como há drift,
#devemos incluir uma constante no VAR em nível (random walk + drift => tendencia estocastica e linear no nível)
criterios = VARselect(dados, lag.max = 20, type = 'const', season=12)

#Selecionando com base no BIC
modelo_nivel = VAR(dados, type = 'const', p=2, season=12)
summary(modelo_nivel)
#Obs: NÃO PODEMOS OLHAR OS VALORES CRÍTICOS do summary acima, pois há risco de inferência espúria

#Teste da nula de que p-ésima defasagem é zero
LM = 2*(logLik(modelo_nivel)- logLik(VAR(dados, type = 'const', p=1, season=12)))
print(LM>qchisq(0.95,9))
#Rejeitamos a nula a 5%

#Vamos olhar o teste de Breusch-Godfrey de não correlação serial
serial.test(modelo_nivel, type = 'BG', lags.bg =5)
#Muita evidência contra não autocorrelação dos erros

#Aumentando defasagem para ver se melhoramos
modelo_nivel = VAR(dados, type = 'const', p=3, season=12)
LM = 2*(logLik(modelo_nivel)- logLik(VAR(dados, type = 'const', p=2, season=12)))

print(LM>qchisq(0.95,9))
print(LM>qchisq(0.9,9))
#Teste LM não rejeita a nula a 5%, mas rejeita a 10%, o que sugere alguma evidência
#da necessidade da terceira defasagem

serial.test(modelo_nivel, type = 'BG', lags.bg =5)
#Teste BG não rejeita a nula a 5%

#p=3 parece ok, visto que apresenta bom balanço entre parcimônia e não autocorrelação dos erros

#Vamos trabalhar com 3 defasagens
johansen = ca.jo(dados, type = 'eigen', ecdet = c('none'), K=3,spec = 'transitory', season=12)

summary(johansen)

#Conclusão do teste: UMA relação de cointegração

#Testando a nula de que atividade econômica NÃ0 participa da relação de cointegração
teste = blrtest(johansen, matrix(c(c(0,1,0), c(0,0,1)), nrow=3), r=1)
summary(teste)
#Claramente rejeitamos a nula


#Testando a nula de que equação quantitativa é boa descrição
teste = blrtest(johansen, matrix(c(1,-1,1),nrow=3), r=1)
summary(teste)
#Também rejeitamos a nula!


#Com mais de uma relação de cointegração, para testarmos hipóteses
# de que r1 < r relações de cointegração são iguais a uma matriz H,
# usamos bh5lrtest.
# Para testar restrições sobre r1 < r relações (por exemplo, que algumas,
# variáveis participam de r1 relações), usamos bh6lrtest.


#Estimando VECM
modelo = cajorls(johansen, r=1)
modelo$beta
summary(modelo$rlm)

#Representação em VAR
vs = vec2var(johansen, r=1)

#Predições
fanchart(predict(vs), plot.type = 'single')

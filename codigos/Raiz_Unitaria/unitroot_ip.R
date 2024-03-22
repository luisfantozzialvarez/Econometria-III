#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Documentos/GitHub/Econometria-III/codigos/Raiz_Unitaria")

#Carregando pacotes necessários. Instale-os se não os possuir
library("CADFtest") # testes de raiz unitária
library("sandwich") #Erros padrão HAC
library("lmtest") #Teste robusto
library("car") #Teste de restrições lineares

#Carregando dados disponíveis no pacote URCA
library("urca")
data("nporg")
industrial_prod  = ts(log(nporg$ip), start = nporg$year[1], frequency = 1)
plot(industrial_prod)

#Testando a presença de raízes unitárias
#Vamos conduzir os testes a 10% de significância

#Começamos pelo modelo mais completo:
teste = CADFtest(industrial_prod, type = "trend", max.lag.y = ceiling(12*(length(industrial_prod)/100)^(1/4)),
                 criterion = "MAIC")

print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.1394 > 0.10)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendência linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estatística F é de 4.48865. Usando o Caso 4 da tabela na página 11 dos slides, vemos que, a 10% de significância,
#o valor crítico para 111 observações é 5.47. Como F < 5.47, não rejeitamos a hipótese nula de que tanto gamma como beta são zero.
#Assim, vamos para o modelo só com drift

teste = CADFtest(industrial_prod, type = "drift", max.lag.y = ceiling(12*(length(industrial_prod)/100)^(1/4)),
                 criterion = "MAIC")
print(teste)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.8905 > 0.10)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta drift
print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estatística F é de 10.211 Usando o Caso 2 da tabela na página 11 dos slides, vemos que, a 10% de significância,
#o valor crítico para 111 observações é 3.86. Como F > 3.86, rejeitamos a nula de que tanto alpha como gamma são zero.
#Como não rejeitamos a nula de que gamma = 0 anteriormente, concluímos que alpha é diferente de zero. Nesse caso, podemos 
#repetir o teste de gamma = 0 contra gamma < 0 usando o valor crítico da normal. Esse valor crítico é dado por:
qnorm(0.10)

#Enquanto nossa t é:
summary(teste)
#-0.687

#Logo NÃO REJEITAMOS a nula de raiz unitária a 10%

#Como paramos no modelo com drift, podemos testar a hipótese de raiz unitária usando
#o procedimento de ERS. Esse procedimento está disponível no pacote urca, que carregamos
#anteriormente

#Vamos rodar o método de ERS com o número de defasagens selecionados pelo MAIC no teste ADF
summary(ur.ers(industrial_prod, type = 'DF-GLS', model = 'constant', lag.max = 5))


#Aqui também não rejeitamos a nula de raiz unitária.

#Vamos testar a presença de componentes determinísticos.
#Como os dados apresentam tendência estocástica, trabalhamos com a série diferenciadas
trend = 1:(length(industrial_prod)-1)
modelo = lm(diff(industrial_prod)~trend)

coeftest(modelo, vcov. = vcovHAC)

#Não rejeitamos a nula de que não há tendência linear determinística na série em diferenças

#Observe que o intercepto do modelo em diferenças é estatisticamente significante. De fato,
#o procedimento sequencial nos indicou que um passeio aleatório com drift como aproximação
#ao processo.



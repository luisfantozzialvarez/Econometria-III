#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Documentos/GitHub/Econometria-III/codigos/Raiz_Unitaria")

#Carregando pacotes necessários. Instale-os se não os possuir
library("CADFtest") # testes de raiz unitária
library("sandwich") #Erros padrão HAC
library("lmtest") #Teste robusto
library("car") #Teste de restrições lineares

#Carregando dados
data = read.csv2("desemprego.csv")

desemprego_brasil = ts(data = data$Desemprego.PNADC., start = c(2012,03), frequency = 12)

#Testando a presença de raízes unitárias
#Vamos conduzir os testes a 5% de significância

#Começamos pelo modelo mais completo:
teste = CADFtest(desemprego_brasil, type = "trend", max.lag.y = ceiling(12*(length(desemprego_brasil)/100)^(1/4)),
                 criterion = "MAIC")

print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 5% (p-valor é de 0.8026 > 0.05)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendência linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estatística F é de 1.7638. Usando o Caso 4 da tabela na página 11 dos slides, vemos que, a 5% de significância,
#o valor crítico para 118 observações é 6.49. Como F < 6.49, não rejeitamos a hipótese nula de que 
#a tendência e o coeficiente associado a y_{t-1} são ambos zero. Nesse caso, vamos para o modelo somente com
#intercepto

teste = CADFtest(desemprego_brasil, type = "drift", max.lag.y = ceiling(12*(length(desemprego_brasil)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 5% (p-valor é de 0.4973 > 0.05)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta intercepto.

print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estatística F é de 1.3763. Usando o Caso 2 da tabela na página 11 dos slides, vemos que, a 5% de significância,
#o valor crítico para 118 observações é 4.71. Como F < 4.71, não rejeitamos a hipótese nula de que 
#o intercepto e o coeficiente associado a y_{t-1} são ambos zero. Nesse caso, vamos para o modelo sem componentes
#determinísticos

teste = CADFtest(desemprego_brasil, type = "none", max.lag.y = ceiling(12*(length(desemprego_brasil)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste)

#Aqui, também não rejeitamos a hipótese nula. Concluímos que a série apresenta UMA raiz unitária


#Vamos testar a presença de componentes determinísticos.
#Como os dados apresentam tendência estocástica, trabalhamos com a série diferenciadas
trend = 1:(length(desemprego_brasil)-1)
modelo = lm(diff(desemprego_brasil)~trend)

coeftest(modelo, vcov. = vcovHAC)

#Não rejeitamos a hipótese nula de que NÃO HÁ tendência linear determinística
#na série em primeira diferença
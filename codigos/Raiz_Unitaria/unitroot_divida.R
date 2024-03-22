#Decompondo séries de tempo

#Fixando pasta com arquivos
setwd("~/Documentos/GitHub/Econometria-III/codigos/Raiz_Unitaria")

#Carregando pacotes necessários. Instale-os se não os possuir
library("CADFtest") # testes de raiz unitária
library("sandwich") #Erros padrão HAC
library("lmtest") #Teste robusto
library("car") #Teste de restrições lineares

#Carregando dados
data = read.csv2("divida_liquida.csv")

divida = ts(data = data$Divida.liquida.do.Setor.publico, start = c(2001,12), frequency = 12)

plot(divida)

#Testando a presença de raízes unitárias
#Vamos conduzir os testes a 5% de significância

#Começamos pelo modelo mais completo:
teste = CADFtest(divida, type = "trend", max.lag.y = ceiling(12*(length(divida)/100)^(1/4)),
                 criterion = "MAIC")

print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 5% (p-valor é de 0.8975 > 0.05)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendência linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estatística F é de 3.1089. Usando o Caso 4 da tabela na página 11 dos slides, vemos que, a 5% de significância,
#o valor crítico para 242 observações é 6.34. Como F < 6.34, não rejeitamos a hipótese nula de que 
#a tendência e o coeficiente associado a y_{t-1} são ambos zero. Nesse caso, vamos para o modelo somente com
#intercepto

teste = CADFtest(divida, type = "drift", max.lag.y = ceiling(12*(length(divida)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 5% (p-valor é de 0.742 > 0.05)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta intercepto.

print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estatística F é de 0.5362. Usando o Caso 2 da tabela na página 11 dos slides, vemos que, a 5% de significância,
#o valor crítico para 242 observações é 4.63. Como F < 4.63, não rejeitamos a hipótese nula de que 
#o intercepto e o coeficiente associado a y_{t-1} são ambos zero. Nesse caso, vamos para o modelo sem componentes
#determinísticos

teste = CADFtest(divida, type = "none", max.lag.y = ceiling(12*(length(divida)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste)

#Aqui, também não rejeitamos a hipótese nula. Concluímos que a série apresenta UMA raiz unitária


#Vamos testar a presença de componentes determinísticos.
#Como os dados apresentam tendência estocástica, trabalhamos com a série diferenciadas
trend = 1:(length(divida)-1)
modelo = lm(diff(divida)~trend)

coeftest(modelo, vcov. = vcovHAC)

#Não rejeitamos a hipótese nula de que NÃO HÁ tendência linear determinística
#na série em primeira diferença
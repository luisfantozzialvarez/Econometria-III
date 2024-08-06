library(lmtest)
library(sandwich)
library(forecast)
library(tseries)

#Para os modelos Garch
library(rugarch)

setwd("~/Documentos/GitHub/Econometria-III/codigos/Arch_Garch")

ibov = read.csv('ibovespa.csv')


#Removendo dias faltantes
ibov = ibov[complete.cases(ibov[,2]),]
ibov$Data = as.Date(ibov[,1], format = "%d/%m/%Y")
ibov = ibov[ibov$Data>=as.Date("2010-01-01"),]

datas = ibov$Data[2:nrow(ibov)]
d_ibov = diff(log(ibov[,2]))

plot(datas, d_ibov, type = 'l')



acf((d_ibov))
acf((d_ibov), ylim = c(-0.07,0.07))
pacf(as.numeric(d_ibov))


coeftest(lm(d_ibov~1), vcov. = vcovHAC)

#Não há evidência de retorno médio =/= 0 no período.

#Vamos considerar todos os ARMA(p,q), 0<=p<=10, 0<=q<=10, sem intercepto
source("../Box_jenkins/box_jenkins_parallel.R")

tabela = arima.est.parallel(d_ibov, 7, 7, 0, include.constant = F)
#Removendo modelos que não convergiram
tabela = tabela[tabela$Converged,]

#Vamos usar um AR(7)
modelo_media = Arima(d_ibov, c(7,0,0), include.constant=F)
summary(modelo_media)
checkresiduals(modelo_media)

res = residuals(modelo_media)

#Detectando heterocedasticidade condicional

Box.test(res^2, lag = 12, type = 'Ljung-Box')

#ARCH-LM test
arch.test <- function(x, lag.max)
{
 rss = (x - mean(x))^2
 covariates = (sapply(1:lag.max, function(j) c(rep(NA,j), rss[1:(length(rss)-j)])))
 print(dim(covariates))
 summary(lm(rss~covariates))
}

arch.test(res, 12)

acf(res^2)

#Evidência forte de heterocedasticidade condicional

#Vamos começar por um modelo ARCH(m)
pacf(res^2)

#Vamos considerar ARCH(10)
model = ugarchfit(ugarchspec(variance.model = list(model='sGARCH', garchOrder =c(10,0)),
                             mean.model = list(armaOrder = c(7,0), include.mean = F),
                             distribution.model = 'norm'), data.frame(d_ibov, row.names = datas))

print(model)

#Modelo parece funcionar bem em termos das covariâncias de nu_t e nu_t^2
#Vamos checar normalidade
nu = residuals(model, standardize = T)

jarque.bera.test(nu)
#Claramente rejeitamos a nula de normalidade, portanto, devemos interpretar
#o estimador como uma pseudo máxima verossimilhança, em que temos estimadores consistentes,
#para os parâmetros da média e variância condicionais, mas análises de risco 
#não devem ser baseadas na normal

#QQ-plot e densidades para visualização
plot(model,which =8)
plot(model, which =9)

#Caudas mais pesadas que a normal

#Volatilidade implícita do modelo na amostra
plot(sigma(model),type='l')

#Volatilidade e retornos
plot(model, which = 3)

#Use plot(model) e selecione uma das opções para mais informação


forecasts = ugarchforecast(model, n.ahead= 10)

print(forecasts)

plot(forecasts, which = 1)
plot(forecasts, which = 3)


#Vamos considerar um GARCH(2,1)
model2 = ugarchfit(ugarchspec(variance.model = list(model='sGARCH', garchOrder =c(2,2)),
                             mean.model = list(armaOrder = c(7,0), include.mean = F),
                             distribution.model = 'norm'), data.frame(d_ibov, row.names = datas))

print(model2)

#Note que esse modelo possui menor BIC que o anterior


plot(model,which =8)
plot(model, which =9)

#Volatilidade implícita do modelo na amostra
plot(sigma(model2))



#Predições fora da amostra
forecasts = ugarchforecast(model2, n.ahead= 10)

plot(forecasts, which = 1)
plot(forecasts, which = 3)



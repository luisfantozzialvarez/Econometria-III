library(lmtest)
library(sandwich)
library(forecast)

#Para os modelos Garch
library(rugarch)

setwd("~/Documents/GitHub/Econometria-III/codigos/Arch_Garch")

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

tabela = arima.est.parallel(d_ibov, 10, 10, 0, include.constant = F)
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

#Evidência forte de heterocedasticidade condicional

#Vamos começar por um modelo ARCH(m)
pacf(res^2)

#Vamos considerar ARCH(10)
model = ugarchfit(ugarchspec(variance.model = list(model='sGARCH', garchOrder =c(10,0)),
                             mean.model = list(armaOrder = c(7,0), include.mean = F),
                             distribution.model = 'norm'), d_ibov)

print(model)

#Volatilidade implícita do modelo na amostra
plot(datas,as.numeric(sigma(model)),type='l')

forecasts = ugarchforecast(model, n.ahead= 10)


#Vamos considerar um GARCH(2,1)
model2 = ugarchfit(ugarchspec(variance.model = list(model='sGARCH', garchOrder =c(2,2)),
                             mean.model = list(armaOrder = c(7,0), include.mean = F),
                             distribution.model = 'norm'), d_ibov)

print(model2)


#Volatilidade implícita do modelo na amostra
plot(datas,as.numeric(sigma(model2)),type='l')


#Predições fora da amostra
forecasts = ugarchforecast(model2, n.ahead= 10)




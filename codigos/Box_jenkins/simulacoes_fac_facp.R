set.seed(123)


ar2<- arima.sim(list("ar"=c(0.5, 0.2)), n = 10000)

acf(ar2)#decai
pacf(ar2)#truncada em 2

ma3 <- arima.sim(list("ma" =c(0.5,0.1, 0.25)), n = 10000)

acf(ma3)#truncada em 3
pacf(ma3)#decai 

arma23 <- arima.sim(list("ar"=c(0.5, 0.2),"ma" =c(0.5,0.1, 0.25)), n = 10000)

acf(arma23) #decai
pacf(arma23) #decai


#AR(1) sazonal (a cada doze meses)
sar1 <- arima.sim(list("ar"=c(rep(0,11), 0.5)), n = 10000)

acf(sar1) #Decaimento a cada 12 períodos
pacf(sar1) #truncada em 12

#MA(1) sazonal (a cada doze meses)
sma1  <- arima.sim(list("ma"=c(rep(0,11),0.5)), n = 10000)

acf(sma1) #Truncada em 12
pacf(sma1) #Decaimento a cada 12 períodos
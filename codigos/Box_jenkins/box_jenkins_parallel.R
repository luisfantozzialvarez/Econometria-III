library(parallel)



#Função para avaliação de um ARIMA. Argumentos:

#data_raw: série de dados original (sem estacionarizar)
#pmax: ordem máxima da parte AR a ser testada
#qmax: ordem máxima da parte MA a ser testada
#d: ordem de integração
#pseas_max: ordem máxima do AR sazonal a ser testado
#qseas_max: ordem máxima do MA sazonal a ser testado
#Dseas: ordem de integração sazonal
#include.constant: incluir intercepto no modelo ARIMA (drift se d=1)?
#include.trend: incluir tendência no modelo ARIMA (drift se d=1)?
#signif: nível de significância para testar significância dos coeficientes
#lags.lbox: vetor com número de defasagens para os testes de Ljung-Box
#cores: número de processos paralelos a serem executados
arima.est.parallel <- function(data_raw, pmax, qmax, d, pseas_max= 0, qseas_max = 0, Dseas = 0,  include.constant = T, include.trend = F,signif = 0.10, lags.lbox = 20,
                               cores = detectCores())
{
  mmd = expand.grid(0:pmax, d, 0:qmax, 0:pseas_max,Dseas, 0:qseas_max)
  
  cluster = makeCluster(cores)
   msd = parApply(cl = cluster, X = mmd, MARGIN = 1, FUN = bx.line,  data_raw = data_raw, include.constant= include.constant,
            include.trend = include.trend, signif = signif, lags.lbox = lags.lbox)
  
  #msd = apply(mmd, MARGIN = 1, FUN = bx.line, data_raw = data_raw, include.constant= include.constant,
  #        include.trend = include.trend, signif = signif, lags.lbox = lags.lbox)

  tabla = do.call(rbind, msd)
  
  keeper = c(pmax>0, qmax>0, pseas_max>0, qseas_max>0, rep(T, ncol(tabla)-4))
  tabla = tabla[,keeper]
  stopCluster(cluster)
  
  return(tabla)
}

  


bx.line <- function(order, data_raw, include.constant, include.trend, signif, lags.lbox)
{
  library(forecast)
  library(tseries)
  raw = as.numeric(order[1:3])
  seas = as.numeric(order[4:6])
  #print(seas)
  
  modelo = tryCatch({Arima(data_raw,order =raw, seasonal = seas, include.constant = include.constant,
                           include.drift = include.trend) }, error  = function(e){
    #warning(e)
    return(NULL)
  })
  
  if(!is.null(modelo))
  {
    if(raw[1]+seas[1] > 0)
    {
      if(T %in% (abs(polyroot(c(1,-1*modelo$model$phi))) <= 1))
        estacionario = "Não é estacionário" else
          estacionario = "É estacionário"
    } else estacionario = "Não se aplica"
    
    if(raw[3]+seas[3] > 0)
    {
      if(T %in% (abs(polyroot(c(1,modelo$model$theta))) <= 1))
        invertivel = "Não é invertível" else
          invertivel = "É invertível"
    } else invertivel = "Não se aplica"
    
    lb = sapply(lags.lbox, function(x) Box.test(modelo$residuals, lag = x, type = "Ljung-Box", fitdf = raw[1]+raw[3]+seas[1]+seas[3])$p.value)
    
    jb = jarque.bera.test(modelo$residuals)$p.value
    
    linha = data.frame("p"=raw[1], "q"=raw[3], 'sa_p' =seas[1], 'sa_q' = seas[3], "Não significantes" =
                         paste(names(modelo$coef)[abs(modelo$coef/sqrt(diag(modelo$var.coef)))<=qnorm(1-signif/2)],
                               collapse = " ; "),"AIC" = AIC(modelo), "BIC" = BIC(modelo),
                       "Estacionariedade" = estacionario, "Invertibilidade" = invertivel,
                       "p-valor LB" = paste(lags.lbox, ": ", lb,sep="",collapse="; "),
                       'p-valor JB' = jb,
                       "Nobs" = nobs(modelo),
                       "Converged"= modelo$code==0)
    
  } else  linha = data.frame("p"=raw[1], "q"=raw[3], 'sa_p' =seas[1], 'sa_q' = seas[3], "Não significantes" =
                        NA,"AIC" = NA, "BIC" = NA,
                     "Estacionariedade" = NA, "Invertibilidade" = NA,
                     "p-valor LB" = NA,
                     "p-valor JB" = NA,
                     "Nobs" = NA,
                     "Converged"= FALSE)
  
  return(linha)
}
  


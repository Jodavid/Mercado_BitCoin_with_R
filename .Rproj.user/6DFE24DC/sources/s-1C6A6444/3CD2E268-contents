#Funções para analises iniciais


#AROON
aroon_series_iniciais<-function(series){

  quant <- length(series)

  saida <- t(simplify2array(mclapply(1:quant, function(i){
    
    #números de períodos
    n_aroon <- ifelse(nrow(series[[i]]) < 14,nrow(series[[i]]), 14);
      
    trend <- TTR::aroon( series[[i]][,c("highest", "lowest")], n=(n_aroon-1) )
    arron_resul <- trend[dim(trend)[1],]
      return(c(series[[i]]$Cripto[1], arron_resul))
        } , mc.cores=cores)))
  
  saida <- data.frame(saida);
  return(saida)
}


# RSI
# Default case - preco de fechamento
rsi_series_iniciais <- function(series){
  
  quant <- length(series)
  
  saida <- t(simplify2array(mclapply(1:quant, function(i){
  # atual
  price <- series[[i]]$closing
  rsi <- TTR::RSI(price)
  # 30 min atras
  rsi_anterior <- TTR::RSI(price[-length(price)])
  dif_rsi <- tail(rsi,1)-tail(rsi_anterior,1)
  
  return(c(series[[i]]$Cripto[1], rsi[length(price)], dif_rsi,series[[i]]$volume[length(price)]))
  } , mc.cores=cores)))
  
  saida <- data.frame(saida);
  return(saida)
}


# MACD
macd_series_iniciais <- function(series){
  
  quant <- length(series)
  
  saida <- t(simplify2array(mclapply(1:quant, function(i){
    # atual
    x <- series[[i]]$closing
    temp <- try(TTR:MACD(x, nFast = 12, nSlow = 26, nSig = 9), T)
    if(typeof(temp) != 'double') temp <- TTR::MACD(x, nFast = 12, nSlow = 20, nSig = 9)
    macd <- temp
    # 30 min atras
    macd_anterior <- tail(macd,2)[1,] 
    dif_macd <- tail(macd,1)-macd_anterior

    return(c(series[[i]]$Cripto[1], dif_macd))
  } , mc.cores=cores)))
  
  saida <- data.frame(saida);
  return(saida)
}


# Redes Neurais
redes_neurais_series_iniciais <- function(series){
  
  quant <- length(series)
  
  saida <- t(simplify2array(mclapply(1:quant, function(i){
    
    data <- series[[i]]
    
    data2 <- data
    
    data2$rsi <- TTR::RSI(as.numeric(data2$closing))
    #' --
    temp <- try(data.frame(data2,TTR::MACD(as.numeric(data2$closing), 12, 26, 9, maType="SMA")), T)
    if(typeof(temp) != 'double') temp <- TTR::MACD(as.numeric(data2$closing), 12, 20, 9, maType="SMA")
    #' --
    data2 <- data.frame(data2, temp)
    data2$will <- TTR::williamsAD(data2[,c("highest","lowest","closing")])
    data2$cci <-  TTR::CCI(data2[,c("highest","lowest","closing")])
    data2 <- data.frame(data2,TTR::stoch(data2[,c("highest","lowest","closing")]))
    data2 <- data.frame(data2,TTR::aroon(data2[, c("highest","lowest")]))
    data2 <- data.frame(data2,TTR::ATR(data2[, c("highest","lowest","closing")]))
    data2$Return <- c(NA,diff(log(data2$closing)))
    x <- nrow(data2)
    ultcolun <- ncol(data2)
    
    #For log data close
    for (i in 0:(x-1)){
      data2[i, ultcolun] <- data2[i+1, ultcolun] 
    }
    
    fit1 <- nnet::nnet( Return~ opening+highest+lowest+closing+volume+rsi+macd+
                          signal+will+cci+fastK+fastD+slowD+aroonUp+aroonDn+
                          oscillator+tr+atr+trueHigh+trueLow, 
                        data = data2[1:(x-1), ], maxit = 5000, 
                        size = 20, decay = 0.01, linout = 1)
    pred <- predict(fit1, newdata = data2[(x-1), ])
    vl_pred <- exp(pred[1])*data[(x-1),4] #Preço Predito
    
    result <- ifelse((vl_pred-data[(x-1),4])>0, data$Cripto[1], "NAO") 
    
    
    return(result)
    
  }, mc.cores=cores)))
  
  
  saida <- data.frame(saida);
  return(saida)
}

#' ------------------------------------------------
#'     Leitura dos dados e Analise dos precos
#'     
#' Objetivo: Ler os datasets e fazer análises
#' sobre os preços ao longo dos dias
#' ------------------------------------------------

#' ----
#' Scripts necessarios:
source("R/Funcoes_para_Scraping_Ativos_Mercado_Bitcoin.R")
source("R/funcoes_para_series_iniciais.R")
library(parallel)
#library(TTR)
#library(nnet)
#' Pegando os núcleos
cores <- detectCores()-1
#' --
#' Lendo os dados
data <- readCsvInsideFolder("dados")
for (i in 3:ncol(data)) data[,i] <- as.numeric(data[,i])
#' --
#' Criando Lista por ativo
simbols <- unique(data$Cripto)
serie_p_analise <- lapply(1:length(simbols), \(i) data |> dplyr::filter(Cripto == simbols[i]) )
#' --
#' numero de fechamentos - pegando maiores de 20 fechamentos
rows <- sapply(1:length(serie_p_analise), function(i){ return(nrow(serie_p_analise[[i]]))})
serie_p_analise <- subset(serie_p_analise, rows > 40 )
#' -----------
variance <- sapply(1:length(serie_p_analise), function(i){ return(var(serie_p_analise[[i]]$avg_price))})
serie_p_analise <- subset(serie_p_analise, variance > 4 )
#' -----------


#' -----------
#' Analise da Cripto
#' --
#' Redes Neurais
redes_net <- redes_neurais_series_iniciais(serie_p_analise)
redes_net <- unlist(redes_net)
redes_net <- subset(redes_net,redes_net!="NAO")
#' ---
vec <- as.numeric(substr(names(redes_net), start=2, stop=7))
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])
#' --------
#' --------
#' RSI
rsi <- rsi_series_iniciais(serie_p_analise)
rsi <- subset(rsi, as.numeric(as.character(rsi[,2])) < 60) # Observar esse 35
colnames(rsi) <- c("Cripto", "rsi", "dif_rsi","volume")
#' --------
vec <- attributes(rsi)$row.names
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])
#' --------
#' --------
#' MACD
macd <- macd_series_iniciais(serie_p_analise)
macd <- subset(macd,as.numeric(as.character(macd[,2]))>0 | as.numeric(as.character(macd[,3]))>0 )
colnames(macd) <- c("Cripto", "dif_macd_1","dif_macd_2")

# Series q ficaram
vec <- attributes(macd)$row.names
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])
#' --------
#' --------
#' AROON
aroon <- aroon_series_iniciais(serie_p_analise)
aroon <- subset(aroon,as.numeric(as.character(aroon$aroonDn)) < 30 &
                  as.numeric(as.character(aroon$aroonUp)) > 70 )
aroon <- aroon[order(as.numeric(as.character(aroon[,2])),decreasing = T),]
colnames(aroon)[1] <- c("Cripto")
vec <- attributes(aroon)$row.names
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])


#' --------
#' Price
price <-  data |>
  dplyr::filter( (Cripto %in% aroon$Cripto) & (date ==  tail(sort(data$date),1) )) |>
  dplyr::select(Cripto, closing)
colnames(price) <- c("Cripto", "price")
#' --------
investir <- merge(aroon,rsi,by="Cripto")
investir <- merge(investir,macd,by="Cripto")
investir <- merge(investir,price,by="Cripto")
#' --------
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[ which(aroon$Cripto == investir$Cripto[i]) ]])

#' #' --------
#' # Calculate the mean and standard error
#' l.model <- lm(serie_p_analise[[1]]$avg_price ~ 1)
#' # Calculate the confidence interval
#' levels <- confint(l.model, level=0.95)
#' levels2 <- confint(l.model, level=0.97)
#' levels3 <- confint(l.model, level=0.99)
#' #' --------

#' --------
#' Valor de compra. stop loss e stop gain
compra <- cbind(menorcompra=investir$price - .006*investir$price,
                maiorcompra=investir$price + .006*investir$price)
stop_loss <- investir$price- 0.035*investir$price
stop_gain <- cbind(ganho1p=investir$price+ 0.01*investir$price,
                   ganho2p=investir$price+ 0.02*investir$price,
                   ganho3p=investir$price+ 0.03*investir$price)

investir <- cbind(investir,compra, stop_loss, stop_gain)

hora.verificacao <- format(Sys.time(), "%X - %A,  %d.%b.%Y")

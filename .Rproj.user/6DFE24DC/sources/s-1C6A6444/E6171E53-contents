#<>----------------------------------------------------------<>
#                      Pacotes Necessários                    #

#install.packages("devtools")
library(devtools)
#Install Package Binancer
#install_github("daroczig/binancer")
library(binancer)
library(httr)
library(data.table) #function rbindlist
library(TTR)
library(parallel)
library(TTR)
library(quantmod)
library(nnet)

#<>----------------------------------------------------------<>

#Pegando os núcleos
cores <- detectCores()-1

setwd("/home/jodavid/srv/shiny-server/binance/scripts/")
#Codigos
source("funcoes_para_series_iniciais.R")

#<>----------------------------------------<>
#         Pegar a série para Analise        #

#<>------------------------------------------<>
#Selecionando as Moedas BTC
simbols_BTC <- binance_symbols()

##Selecionando apenas os BTC
currency <- ifelse(nchar(simbols_BTC)==5, substr(simbols_BTC, start=3, stop=7),
                   ifelse(nchar(simbols_BTC)==6, substr(simbols_BTC, start=4, stop=7),
                          ifelse(nchar(simbols_BTC)==7, substr(simbols_BTC, start=5, stop=7),substr(simbols_BTC, start=6, stop=8))))

simbols_BTC <- subset(simbols_BTC,currency=="BTC")
#<>------------------------------------------<>

saidass=c()

serie_p_analise <- mclapply(1:length(simbols_BTC), function(i) binance_klines(simbols_BTC[i], interval = '1d'), mc.cores=cores)
serie_p_analise_final <- serie_p_analise

#numero de fechamentos - pegando maiores de 26 fechamentos
rows <- sapply(1:length(serie_p_analise), function(i){ return(nrow(serie_p_analise[[i]]))})
serie_p_analise <- subset(serie_p_analise, rows >40 )
#<>----------------------------------------<>


#<>----------------------------------------<>
#            Analise da Crypto              #

#Redes Neurais
redes_net <- redes_neurais_series_iniciais(serie_p_analise)
redes_net <- unlist(redes_net)
redes_net <- subset(redes_net,redes_net!="NAO")

vec <- as.numeric(substr(names(redes_net), start=2, stop=7))
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])

#RSI
rsi <- rsi_series_iniciais(serie_p_analise)
rsi <- subset(rsi,as.numeric(as.character(rsi[,2]))<35) #& as.numeric(as.character(rsi[,2]))<70
colnames(rsi) <- c("crypto", "rsi", "dif_rsi","volume")

#Series q ficaram
vec <- attributes(rsi)$row.names
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])



#MACD
macd <- macd_series_iniciais(serie_p_analise)
macd <- subset(macd,as.numeric(as.character(macd[,2]))>0 | as.numeric(as.character(macd[,3]))>0 )
colnames(macd) <- c("crypto", "dif_macd_1","dif_macd_2")

#Series q ficaram
vec <- attributes(macd)$row.names
serie_p_analise <- lapply(1:length(vec), function(i) serie_p_analise[[vec[i]]])

#AROON
aroon <- aroon_series_iniciais(serie_p_analise)
aroon <- subset(aroon,as.numeric(as.character(aroon[,2]))< 30 &# as.numeric(as.character(aroon[,2]))<80 &
                  as.numeric(as.character(aroon[,3]))>70 )#& #as.numeric(as.character(aroon[,3]))<70 &
#as.numeric(as.character(aroon[,4]))>-50 & as.numeric(as.character(aroon[,4]))<=20) #as.numeric(as.character(aroon[,3]))>30 &
aroon <- aroon[order(as.numeric(as.character(aroon[,2])),decreasing = T),]
colnames(aroon)[1] <- c("crypto")


#Price
price <- binance_ticker_all_prices()[,1:2]
colnames(price) <- c("crypto", "price")

investir <- merge(aroon,rsi,by="crypto")
investir <- merge(investir,macd,by="crypto")
investir <- merge(investir,price,by="crypto")
#investir <- investir[order(as.numeric(as.character(investir[,4])),decreasing = T),]

#Valor de compra. stop loss e stop gain
compra <- cbind(menorcompra=investir$price - .006*investir$price,
                maiorcompra=investir$price + .006*investir$price)
stop_loss <- investir$price- 0.035*investir$price
stop_gain <- cbind(ganho1p=investir$price+ 0.01*investir$price,
                   ganho2p=investir$price+ 0.02*investir$price,
                   ganho3p=investir$price+ 0.03*investir$price)

investir <- cbind(investir,compra, stop_loss, stop_gain)

hora.verificacao <- format(Sys.time(), "%a %b %d %X %Y")

save.image("ativos_selecionados.Rdata")

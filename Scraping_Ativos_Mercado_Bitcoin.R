#' ------------------------------------------------
#'     Scraping Ativos do Mercado Bitcoin
#'     
#' Objetivo: Pegar precos dos ativos até X dias para trás
#' ------------------------------------------------
source("R/Funcoes_para_Scraping_Ativos_Mercado_Bitcoin.R")
#'--
criptos <- moedas_MBC()
moedas <- unlist(criptos[1])
#'--
Hoje <- Sys.Date()
j=1
#'--
QutdeDias <- 30
#'--
while(j <= QutdeDias ){
  #' -----
  Data_Anterior <- Hoje-j
  
  if(!file.exists(paste0("dados/dados_",Data_Anterior,".csv"))) {
  
  #' -----
  #' aplicando loop para obter os indicadores das ações armazenados em um só dataframe
  retornos_cripto <- purrr::map2_df(moedas,as.Date(Data_Anterior),day_summary_MBC)
  #'--
  data_frame_cripto <- map_to_dataframe(retornos_cripto)
  #'--
  write.csv2(data_frame_cripto,paste0("dados/dados_",Data_Anterior,".csv"),row.names = F)
  #' -----
  }
  
  j=j+1
  print(Data_Anterior)
  #' -----
  
}
#' -----

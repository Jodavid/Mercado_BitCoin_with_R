#' ------------------------------------------------
#'     Scraping Ativos do Mercado Bitcoin
#'     
#' Objetivo: Através da Página de documentação da API
#' do Mercado BitCoin, pegar os nomes das criptos
#' para retornar com os últimos preço do dia atual
#' ------------------------------------------------
#' ----
#' Bibliotecas necessarias:
#' --
#' jsonlite
#' dplyr
#' rvest
#' lubridate
#' purrr
#' plyr
#' --

#' Funcao: moedas_MBC
#' Descrição: Retorna as criptos existentes no mercado bitcoin
moedas_MBC <- function() {
  #' --
  #' URL da documentação da API
  url <- "https://www.mercadobitcoin.com.br/api-doc/"
  #' Fazendo a leitura da url
  webpage <- rvest::read_html(url)
  #' --
  names_html <- rvest::html_nodes(webpage, ".domain")
  names <- rvest::html_text2(names_html)
  names <- names[1]
  vetor1 <- unlist(strsplit(names, split = "\n"))
  #' --
  #' Retorno com os nomes dos ativos
  vetor_ativos <- matrix(unlist(strsplit(vetor1, split = " :")), ncol = 2, byrow = T)
  colnames(vetor_ativos) <- c("Cripto", "Descricao")
  vetor_ativos <- dplyr::as_tibble(vetor_ativos)
  #' --
  return(vetor_ativos)
}
#' --

#' Funcao: day_summary_MBC
#' Descrição: Retorna resumo diário de negociações realizadas.
day_summary_MBC <- function(moeda = "BTC", data = Sys.Date()) {
  #' --
  #print(moeda)
  #' --
  #' Dia anterior
  data <- data - 1
  #' --
  res <- httr::GET(paste0(
    "https://www.mercadobitcoin.net/api/", moeda,
    "/day-summary/", lubridate::year(data), "/", lubridate::month(data), "/", lubridate::day(data), "/"
  ))
  #' --
  ret <- try( data.frame(Cripto = moeda, t(unlist(jsonlite::fromJSON(rawToChar(res$content))))),
              TRUE)
  #' --
  return(ret)
  #' --
}
#' --

#' Funcao: map_to_dataframe
#' Descrição: Retorna um data.frame com os resultados do map_df
map_to_dataframe <- function(retornos_cripto) {
  #' --
  #' Contando as colunas para criar data.frame
  base <- ncol(t(data.frame(unlist(retornos_cripto[1]))))
  #' --
  nomes <- c("Cripto", "date", "opening", "closing", "lowest", "highest", "volume", "quantity", "amount", "avg_price")
  #' --
  data_frame_res <- c()
  #' --
  for (i in 1:length(retornos_cripto)) {
    #' --
    linha_i <- t(data.frame(unlist(retornos_cripto[i])))
    #' --
    if (ncol(linha_i) == base) {
      #' --
      colnames(linha_i) <- nomes
      data_frame_res <- rbind(data_frame_res, linha_i)
      #' --
    }
    #' --
    row.names(data_frame_res) <- NULL
    #' --
  }
  #' --
  return(data_frame_res)
  #' --
}

#' Funcao: readCsvInsideFolder
#' Descrição: Retorna um data.frame com todos os arquivos da pasta
readCsvInsideFolder <- function(path){
  
  # Return a single dataframe from a list of dataframes imported as xls files
  files <- list.files(path = path)
  df_list <- list()
  
  for (i in 1:length(files)){
    data         <- read.csv2(paste0(path,"/", files[i]), stringsAsFactors = FALSE)
    df_list[[i]] <- data
  }
  
  df <- plyr::ldply(df_list, data.frame)
  return(df)
}


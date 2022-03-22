# ------------------------------------------
#     Credenciais - Mercado Bitcoin
# ------------------------------------------


# ID: bbd84c26110428bd558149e392c10f41
# Chave Secreta: 66ba0831ab451564a733b2a5e4a39792d6a348be2f1c3a834a7d0cc775a19875

library(httr)
abc <- httr::GET("https://www.mercadobitcoin.net/tapi/v3/")
abc$request

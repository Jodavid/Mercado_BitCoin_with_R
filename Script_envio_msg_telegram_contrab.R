# <> ---------------------------------------- #
# Script para enviar mensagem no Telegram
#
# Criado dia: 22.03.2022
# <> --------------------------------------- #

# Instalação os pacotes:
# install.packages("telegram.bot")
# devtools::install_github("GuangchuangYu/emojifont")

# Pacote Necessário:
# telegram.bot

# Token do Bot:
bot <- telegram.bot::Bot(token = "1778236079:AAGV2ojus21kRQQc78nQESy51LjaN-0YBNY")

# ID Chat Pedro - "920083323"
# ID Chat Jodavid - "355364195"
chatid <- c("355364195")

# -------------------------------------------------------------- #


# -------------------------------------------------------------- #

# Texto:
texto <- paste(
      emojifont::emoji('rotating_light'),"ATIVOS SELECIONADOS PARA HOJE!",emojifont::emoji('rotating_light'),"\n\n",
      emojifont::emoji('dart')," Cripto: ",investir$Cripto,"\n",
      emojifont::emoji('moneybag')," Valor: R$",round(investir$price,4),"\n\n",
      emojifont::emoji('moneybag')," Maior Compra: R$",round(investir$maiorcompra,4),"\n",
      emojifont::emoji('moneybag')," Menor Compra: R$",round(investir$menorcompra,4),"\n\n",
      emojifont::emoji('moneybag')," Stop Loss: R$",round(investir$stop_loss,4),"\n\n",
      emojifont::emoji('moneybag')," Ganho 1%: R$",round(investir$ganho1p,4),"\n",
      emojifont::emoji('moneybag')," Ganho 2%: R$",round(investir$ganho2p,4),"\n",
      emojifont::emoji('moneybag')," Ganho 3%: R$",round(investir$ganho3p,4),"\n\n",
      emojifont::emoji('clock2'),"Data:",format(Sys.Date(), "%d-%m-%Y"),"\n",
      emojifont::emoji('clock3'),"Hora:",format(Sys.time(), "%X"),"\n\n",
      "Até Logo!","\n"
      )


# Envio das mensagens
for(t in 1:length(texto)){
  for (j in 1:length(chatid)){
    bot$sendMessage(chat_id = chatid[j],
                    text = texto[t]
    )
  }
}

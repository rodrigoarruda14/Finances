# PROGRAMA PARA LER COTA??O DOS ATIVOS E ENVIAR VIA TELEGRAM

# CARREGANDO PACOTES
library(rvest)
library(rjson)
library(telegram)
library(PerformanceAnalytics)
library(quantmod) 
library(dplyr)

carteira <- c("BBAS3.SA","BBSE3.SA","CSAN3.SA","ITUB4.SA","MGLU3.SA","TIET11.SA")

bot <- TGBot$new(token = bot_token('ConsultaBolsaBot'))
bot$set_default_chat_id(417504258)

consulta_preco <- function(carteira)
{
  link1 <- "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22"
  link2 <- "%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="
  cotacoes <- data.frame()
  realtime.price <- 0
  previous.close <- 0
  variacao.perct <- 0
  dta_hora_trade <- 0
  for(i in 1:length(carteira))
  {
    url <- paste(link1,carteira[i],link2, collapse = "")
    json <- fromJSON(file = url, method = "C")
    realtime.price[i] <- as.numeric(json$query$results$quote$LastTradePriceOnly)
    previous.close[i] <- as.numeric(json$query$results$quote$PreviousClose)
    variacao.perct[i] <- round(((realtime.price[i]/previous.close[i])-1)*100,3)
    dta_hora_trade[i] <- json$query$created
  }
  cotacoes <- cbind.data.frame(carteira,realtime.price,previous.close,variacao.perct,dta_hora_trade)
}


acompanhar_cotacao <- function(frequencia = 30)
{
  #load("historico.RData")
  # loop infinito
  while(TRUE) {
    # pega a cota??o da carteira informada, atrav?s da API
    nova_consulta <- consulta_preco(carteira)
    #print(nova_consulta)
    for(j in 1:length(carteira))
    {
      if(nova_consulta$variacao[j] < -1) {
        bot$sendMessage('Oportunidade de compra!')
        bot$sendMessage(nova_consulta$carteira[j])
        bot$sendMessage(nova_consulta$variacao.perct[j])
      }
      if(nova_consulta$variacao[j] > 1) {
        bot$sendMessage('Oportunidade de venda!')
        bot$sendMessage(nova_consulta$carteira[j])
        bot$sendMessage(nova_consulta$variacao.perct[j])
      }
    }
    # guarda a consulta
    #historico <- bind_rows(historico, nova_consulta)
    #   save(historico, file = "historico.RData")
  }
  Sys.sleep(frequencia)
}

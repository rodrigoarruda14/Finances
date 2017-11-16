
library(quantmod)
library(dplyr)
library(tidyr)

## 

getSymbols('BBAS3',src = 'google')

x <- BBAS3 %>% as.data.frame() %>% mutate(date = index(BBAS3)) %>%
  select(date, BBAS3.Open, BBAS3.Close, BBAS3.High, BBAS3.Low, BBAS3.Volume) %>%
  rename(open = BBAS3.Open, close = BBAS3.Close, high = BBAS3.High, low = BBAS3.Low, volume = BBAS3.Volume)

x %>% drop_na() %>%
  mutate(ano = format(date,'%Y'), up = c(((.$close[2:length(.$close)]-.$close[1:length(.$close)-1])>0),"NA")) %>% group_by(ano) %>%
  summarise(minima = round(min(close,na.rm = TRUE),2), maxima = max(close,na.rm = TRUE), abertura = first(close), fechamento = last(close), retorno = round((fechamento/abertura),2)-1, qtd_ups = sum(up=='TRUE'), qtd_downs = sum(up=='FALSE')) %>%
  ungroup() %>% left_join(.,mutate(as.data.frame(annualReturn(BBAS3)), ano = format(index(annualReturn(BBAS3)), '%Y'))) %>%
  knitr::kable()



## RETURNS

data <- BBAS3
results <- NULL

for (ii in symbolList){
  
  data <- getSymbols('BBAS3',
                     src = 'google', 
                     from = "2017-01-01", 
                     auto.assign = FALSE)
  
  colnames(data) <- c("open","high","low","close","volume")
  
  dailyRtn <- (as.numeric(data[2:nrow(data),"close"])/as.numeric(data[1:(nrow(data)-1),"close"])) - 1
  intradayRtn <- (as.numeric(data[,"close"])/as.numeric(data[,"open"]))-1
  overnightRtn <- (as.numeric(data[2:nrow(data),"open"])/as.numeric(data[1:(nrow(data)-1),"close"])) - 1
  
  results <- rbind(results,cbind(
    paste(round(100 * sum(dailyRtn,na.rm=TRUE),1),"%",sep=""),
    paste(round(100 * sum(intradayRtn,na.rm=TRUE),1),"%",sep=""),
    paste(round(100 * sum(overnightRtn,na.rm=TRUE),1),"%",sep="")))
} 
colnames(results) <- c("dailyRtn","intradayRtn","overnightRtn")
rownames(results) <- symbolList


# Quick returns - quantmod style 

getSymbols("SBUX") 

dailyReturn(SBUX) # returns by day 
weeklyReturn(SBUX) # returns by week 
monthlyReturn(SBUX) # returns by month, indexed by yearmon 

# daily,weekly,monthly,quarterly, and yearly 
allReturns(SBUX) # note the plural


BBAS3['2017-08/']
monthlyReturn(BBAS3['2017-08/'])

mutate(as.data.frame(annualReturn(BBAS3)), ano = format(index(annualReturn(BBAS3)), '%Y'))



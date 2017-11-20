
library(quantmod)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(ggplot2)

## 

symbolList <- read.csv("~/Finances/cod_acoes.txt", sep="")  
symbolList <- as.data.frame(symbolList)

y <- head(symbolList)

d <- as.vector(t(extract(y, col= Codigo, "[[A-Z]+[0-9]]"))) %>% paste0("BVMF:",.)

for (ii in d){
  
  data <- getSymbols(Symbols = ii,
                     src = 'google', 
                     auto.assign = FALSE)
  
colnames(data) <- c("open","high","low","close","volume")  

x <- data %>% as.data.frame() %>% mutate(date = index(data)) %>% drop_na() 

tab <- x %>% 
  mutate(ano = format(date,'%Y'), up = c(((.$close[2:length(.$close)]-.$close[1:length(.$close)-1])>0),"NA")) %>% group_by(ano) %>%
  summarise(Open = first(open), Close = last(close), Min. = round(min(close,na.rm = TRUE),2), Max. = max(close,na.rm = TRUE), number.ups = sum(up=='TRUE'), number.downs = sum(up=='FALSE')) %>%
  ungroup() %>% left_join(.,mutate(as.data.frame(round(annualReturn(data),3)), ano = format(index(annualReturn(data)), '%Y'))) %>%
  knitr::kable()

print(ii)
print(tab)

cycles.dates<-c("2015-12-02/2016-08-31")
risk.dates = c("2017-05-17")
risk.labels = c("Delação JBS")

chart.TimeSeries(data[,4], main = 'Banco do Brasil', colorset = "darkblue", 
                 period.areas = cycles.dates, period.color = "lightblue",
                 event.lines = risk.dates, event.labels = risk.labels, 
                 event.color = "darkred", lwd = 2,)  

}


##############################



  data <- getSymbols(Symbols = 'BVMF:BBAS3',
                     src = 'google', 
                     auto.assign = FALSE)
  
  colnames(data) <- c("open","high","low","close","volume")  
  
  x <- data %>% as.data.frame() %>% mutate(date = index(data)) %>% drop_na() 
  
x %>% 
    mutate(ano = format(date,'%Y'), up = c(((.$close[2:length(.$close)]-.$close[1:length(.$close)-1])>0),"NA")) %>% group_by(ano) %>%
    summarise(Open = first(open), Close = last(close), Min. = round(min(close,na.rm = TRUE),2), Max. = max(close,na.rm = TRUE), number.ups = sum(up=='TRUE'), number.downs = sum(up=='FALSE')) %>%
    ungroup() %>% left_join(.,mutate(as.data.frame(round(annualReturn(data),3)), ano = format(index(annualReturn(data)), '%Y'))) %>%
    knitr::kable()

tail(ave(x$close, FUN=function(x) c(0, diff(x))))
sign(tail(ave(x$close, FUN=function(x) c(0, diff(x)))))
y <- rle(c(1,0,0,0,1,0,0,0,0,0,2,0,0))
y$lengths[y$values==0]



anos <- x %>% mutate(ano=format(date, '%Y')) %>% distinct(.$ano) %>% rename(ano = ".$ano") %>% t() %>% as.vector()

close  <- x %>% filter(date >= '2017-01-01') %>% .$close

teste <- close %>% ave( FUN = function(x) c(0, diff(x))) %>% sign() %>% rle() 
table(teste$lengths[teste$values==-1])


for(j in anos){
  
  close  <- x %>% filter(date >= paste0(j,"-01-01")) %>% .$close
  print(paste0(j,"-01-01"))
  teste <- close %>% ave( FUN = function(x) c(0, diff(x))) %>% sign() %>% rle() 
  print(teste$lengths[teste$values==1])
}



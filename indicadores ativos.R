
library(quantmod)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(ggplot2)
library(highcharter)
library(viridisLite)
library(forecast)
library(treemap)
library(flexdashboard)

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

chart.TimeSeries(data[,4], main = ii, colorset = "darkblue", 
                 period.areas = cycles.dates, period.color = "lightblue",
                 event.lines = risk.dates, event.labels = risk.labels, 
                 event.color = "darkred", lwd = 2,)  

anos <- x %>% mutate(ano=format(date, '%Y')) %>% distinct(.$ano) %>% rename(ano = ".$ano") %>% t() %>% as.vector()
close  <- x %>% filter(date >= '2017-01-01') %>% .$close

for(j in anos){
  
  close  <- x %>% filter(date >= paste0(j,"-01-01")) %>% .$close
  print(paste0(j,"-01-01"))
  teste <- close %>% ave( FUN = function(x) c(0, diff(x))) %>% sign() %>% rle() 
  print(teste$lengths[teste$values==-1])
 }

}


##############################

### summary annual indicators

data <- getSymbols(Symbols = 'BVMF:BOVA11',
                   src = 'google', 
                   auto.assign = FALSE)
  
colnames(data) <- c("open","high","low","close","volume")  
  
x <- data %>% as.data.frame() %>% mutate(date = index(data)) %>% drop_na() %>% mutate(valuation = ave(.$close, FUN = function(y) c(0, diff(y))), daily.return = as.vector(dailyReturn(data)))

tab <- x %>% 
  mutate(ano = format(date,'%Y'), up = c(((.$close[2:length(.$close)]-.$close[1:length(.$close)-1])>0),"NA")) %>% group_by(ano) %>%
  summarise(Open = first(open), Close = last(close), Min. = round(min(close,na.rm = TRUE),2), Max. = max(close,na.rm = TRUE), number.ups = sum(up=='TRUE'), number.downs = sum(up=='FALSE')) %>%
  ungroup() %>% left_join(.,mutate(as.data.frame(round(annualReturn(data),3)), ano = format(index(annualReturn(data)), '%Y'))) %>%
  knitr::kable()

print(tab)


### min/max historical

historical.min <- x %>% filter(close==min(close)) %>% print()
historical.max <- x %>% filter(close==max(close)) %>% print()

### asset price evolution

cycles.dates<-c("2015-12-02/2016-08-31")
risk.dates = c("2017-05-17", as.character(historical.min$date), as.character(historical.max$date))
risk.labels = c("Delação JBS","Mínima Histórica","Máxima Histórica")

chart.TimeSeries(data[,4], main = ii, colorset = "darkblue", 
                 period.areas = cycles.dates, period.color = "lightblue",
                 event.lines = risk.dates, event.labels = risk.labels, 
                 event.color = "darkred", lwd = 2,)  

hchart(data)

### average daily evaluation/devaluation

avg.daily.devaluation <- x%>% filter(daily.return < 0) %>% summarize(avg.dev = round((mean(daily.return)*100),3)) %>% paste0('%') %>% print()
avg.daily.evaluation  <- x%>% filter(daily.return > 0) %>% summarize(avg.ev = round((mean(daily.return)*100),3)) %>% paste0('%') %>% print()

### average number of consecutive devaluations

anos <- x %>% mutate(ano=format(date, '%Y')) %>% distinct(.$ano) %>% rename(ano = ".$ano") %>% t() %>% as.vector()
avg.sequent.downgrade <- NULL

for(j in anos){
  
  base  <- x %>% filter(date >= paste0(j,"-01-01")) 
  teste <- base$close %>% ave( FUN = function(x) c(0, diff(x))) %>% sign() %>% rle() 
  print(j)
  print(teste$lengths[teste$values==-1])
  avg.sequent.downgrade[j] <- mean(teste$lengths[teste$values==-1])
}


### data distribution of downgrade in sequence

teste <- x$close %>% ave( FUN = function(x) c(0, diff(x))) %>% sign() %>% rle() 
ups <- teste$lengths[teste$values==1]
downs <- teste$lengths[teste$values==-1]

t2 <- c(rep(0,length(ups)),downs)

t3 <- chisq.test(t2,rpois(n = length(t2), lambda = 1))

cat("H0: A Amostra tem distribuição Poisson? \n", t3$p.value) 

### calculate Poisson distribution P(x>=2)

cat("Probabilidade da cotação cair duas vezes seguidas:\n", 1 - ppois(q = 2,lambda = 1)) 


### calculate Binomial distribution P(x=1)

teste$values %>% as.data.frame() %>% rename(values = ".") %>% filter(values!= 0)

y <- new.env() %>% as.list()

y <- getSymbols(Symbols = d,
                           src = 'google', 
                           auto.assign = TRUE)



symbolList <- read.csv("~/Finances/cod_acoes.txt", sep="")  
symbolList <- as.data.frame(symbolList)

symbolList <- as.vector(t(extract(symbolList, col= Codigo, "[[A-Z]+[0-9]]")))

  stock <- getSymbols(Symbols = paste0("BVMF:",ii),
                      src = 'google', 
                      auto.assign = FALSE)
  
  colnames(stock) <- c("open","high","low","close","volume")
  
  stock <- stock %>% as.data.frame() %>% mutate(date = index(stock)) %>%   drop_na() %>% mutate(valuation = ave(.$close, FUN = function(y) c(0, diff(y))), daily.return = as.vector(dailyReturn(stock)))
  



# Function to calculate daily, weekly and monthly returns
valuation <- function(ii)
{
    rtn <- as.data.frame(NULL)
    stk <- getSymbols(Symbols = paste0("BVMF:",ii), src = 'google', auto.assign = FALSE, warnings = FALSE)
    rtn.1 <- (last(dailyReturn(stk))*100) %>% round(.,2)
    rtn.2 <- (last(weeklyReturn(stk))*100) %>% round(.,2)
    rtn.3 <- (last(monthlyReturn(stk))*100) %>% round(.,2)
    rtn <- cbind.data.frame(ii, rtn.1, rtn.2, rtn.3) %>% 
      rename(Stock = ii, Daily = rtn.1, Weekly = rtn.2, Monthly = rtn.3)
    
}

# Calculate returns to all the stocks in Ibovespa 
t <- NULL

for(i in symbolList)
{
  t <- rbind(t, valuation(i))
}

# Function to calculate daily, weekly and monthly minimum
min.n <- function(d, n)
{
   minimos <- NULL
    minimos[[1]] <- d %>% filter(Daily %in% head(sort(d$Daily), n= 5)) %>% select(Stock, Daily) 
    minimos[[2]] <- d %>% filter(Weekly %in% head(sort(d$Weekly), n= 5)) %>% select(Stock, Weekly) 
    minimos[[3]] <- d %>% filter(Monthly %in% head(sort(d$Monthly), n= 5)) %>% select(Stock, Monthly)
  return(minimos)
}

n.min.daily %>% hchart("bar")

## Applied Econometrics of FX markets 

# Tutorial 1 

# delete workspace 
rm(list=ls())


# libraries 
library(tidyverse)


# import data
data <-  read_delim("spot_rates.csv",
               col_names = T,
               delim = ";",
               locale=locale(decimal_mark = ","))

str(data)
summary(data)

data2 <- read.csv("spot_rates.csv", header =TRUE,sep=";",dec = ",")
#summary(data2)
data2$USD = as.numeric(data2$USD)
data2$EURUSD = 1/data2$USD
data2$Date = as.Date(with(data,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

#b) 
EURUSD = 1/data2$USD
plot(EURUSD, type="l")

# c)
logreturn = diff(log(data2$EURUSD))
plot(logreturn)

#d)

sqlogreturn = diff(log(data2$USD^2))
plot(sqlogreturn)

# change columns to numerical 
# and geneate EUR/USD variable
data <- 
  data %>% 
  mutate(USD = as.numeric(USD),
         JPY = as.numeric(JPY),
         GBP = as.numeric(GBP),
         CHF = as.numeric(CHF),
         CAD = as.numeric(CAD),
         NOK = as.numeric(NOK),
         SEK = as.numeric(SEK),
         AUD = as.numeric(AUD),
         NZD = as.numeric(NZD),
         DKK = as.numeric(DKK),
         EURUSD = 1/USD,
         DATE = as.Date(with(data,paste(Year,Month,Day,sep="-")),"%Y-%m-%d"),
         logreturn = diff(log(EURUSD)))


plot(data$DATE,data$EURUSD)

# b) plot
ggplot(data=data, aes(x = DATE,y=EURUSD))+
  geom_line()+
  xlab("DATE")+
  ylab("EURUSD")+
  ggtitle("")

         
# c) plot 
ggplot(data = data,aes(x=DATE,y=EURUSD))+
  geom_line()+
  xlab("date")+
  ylab("logreturn")


#f)

par( mfrow= c(3,1) )
plot(EURUSD, type="l")
plot(logreturn)
plot(sqlogreturn)




# 2 OLS Estimation 



# a) 

OLS_1 <-  function(y,X){
  
  solve(t(X)%*%X)%*% t(X) %*% y
  
}




# b)

OLS_2 <- function(y,X){
  
  betahead = solve(t(X)%*%X)%*% t(X) %*% y
}


#3.

# coeftest(fit, vcov=vcovHC(model, type="HC1"))
# coeftest(fit, vcov.=NeweyWest(model,...)




















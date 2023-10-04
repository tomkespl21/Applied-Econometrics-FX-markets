#libraries
library(foreign)
library(sandwich)
library(lmtest)
library(tseries)
library(calibrate)


###################################################################
## Load data
SpotRates <- read.csv("fxrates.csv", header = TRUE, sep=";",
                      dec="." , check.names=TRUE)
InterestRates3m <- read.csv("irates3M.csv", header =TRUE,
                            sep=";", dec="." , check.names=TRUE)


View(SpotRates)
View(InterestRates3m)
##################################################################



## Compute end of month values
MonthlySpotRates <- NULL
MonthlyInterestRates3m <- NULL
for(i in 2:dim(SpotRates)[1]){
  
  if(SpotRates[i,2]!=SpotRates[(i-1),2]){
    MonthlySpotRates <- rbind(MonthlySpotRates,SpotRates[(i-1),])} 
  if(InterestRates3m[i,2]!=InterestRates3m[(i-1),2]){
    MonthlyInterestRates3m <- rbind(MonthlyInterestRates3m,InterestRates3m[(i-1),])} 
  
}

##########################################################################################################
## Compute quarterly values
QuarterlySpotRates <- NULL
QuarterlyInterestRates3m <- NULL
for(i in seq(1,dim(MonthlySpotRates)[1], by=3)){
  
  QuarterlySpotRates <- rbind(QuarterlySpotRates,MonthlySpotRates[i,])
  QuarterlyInterestRates3m <- rbind(QuarterlyInterestRates3m, MonthlyInterestRates3m[i,])
  
}


N <- dim(MonthlyInterestRates3m)[1]


# pick interest rates and compute the differential
i <- MonthlyInterestRates3m[,14]   # euro interest rate
i_star <- MonthlyInterestRates3m[,4]  # us dollar interest rate 
x <- (i-i_star)[2:N]                  # compute differential 
xlag <- (i-i_star)[1:(N-1)]           # compute lag differential 
print(x)

# Graphing
plot.ts(x, plot.type = "single")

# Estimating arma model 
mod1 <- arma(x,c(1,1))
summary(mod1)

# linear model
mod2 <- lm(x ~ xlag)
summary(mod2)






###########################################################################################################
## 1. EUR/DOL Plot
MonthlySpotRates <- rbind(MonthlySpotRates,SpotRates[dim(SpotRates)[1],])

EUR_DOL <- SpotRates[,4]
plot.ts(EUR_DOL)

###########################################################################################################
## 2. Stationarity Simulation Example
y1 <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 200)
y2 <- arima.sim(list(order = c(1,1,0), ar = 0.5), n = 200)

plot.ts(y2/10, ylim=c(-5,6))
lines(y1)

y3 <- arima.sim(list(order = c(0,0,0)), n = 500)
plot.ts(y3)


###########################################################################################################
## 3. MA Simulation Example
m1 <- arima.sim(list(order = c(0,0,1), ma = 0), n = 1000)
m2 <- arima.sim(list(order = c(0,0,10), ma = ones(10,1)), n = 1000)

par(mfrow=c(2,1))
plot.ts(m1)
plot.ts(m2)


###########################################################################################################
## 4. AR Simulation Example
a1 <- arima.sim(list(order = c(1,0,0), ar = 0.1), n = 500)
a2 <- arima.sim(list(order = c(1,0,0), ar = 0.3), n = 500)
a3 <- arima.sim(list(order = c(1,0,0), ar = 0.7), n = 500)
a4 <- arima.sim(list(order = c(1,0,0), ar = 0.95), n = 500)

par(mfrow=c(2,1))
plot.ts(a1)
plot.ts(a4)


###########################################################################################################
## 5. Fitting ARMA model for EURO/DOLLAR Exchange Rate
mod10 <- arma(EUR_DOL,c(1,0))
mod01 <- arma(EUR_DOL,c(0,1))
mod11 <- arma(EUR_DOL,c(1,1))
mod01 <- arma(EUR_DOL,c(0,1))
mod21 <- arma(EUR_DOL,c(2,1))
mod12 <- arma(EUR_DOL,c(1,2))
mod22 <- arma(EUR_DOL,c(2,2))

summary(mod10)
summary(mod01)
summary(mod11)
summary(mod01)
summary(mod21)
summary(mod12)
summary(mod22)

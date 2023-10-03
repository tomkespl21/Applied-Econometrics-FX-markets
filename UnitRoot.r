# testing for  unit roots 

library(fUnitRoots)



##########################  Granger Newbold #########################

N <- 1000    # Monte Carlo Replications
TS <- 100   # Sample Size


dx <- rnorm(TS)  # pseudo random normal dist. numbers 
dy <- rnorm(TS)  # pseudo random normal dist. numbers

# cummulative sum:
x <- cumsum(dx)
y <- cumsum(dy)

graphdata <- cbind(x,y)

# time series plot of cummulaitve sums 
plot.ts(graphdata,plot.type = "single", col = c("red","blue"))

plot(x,y)

linmod <- lm(y ~ x)
summary(linmod)


############# Monte Carlo Simulation of I(1) Regressions  ############

tstats <- matrix(0,K,1)   # empty matrix for t stats 
five <- matrix(0,K,1)
one <- matrix(0,K,1)






###########################################################################################################
## Load data
SpotRates <- read.csv("fxrates.csv", header = TRUE, sep=";", dec="." , check.names=TRUE)
InterestRates3m <- read.csv("irates3M.csv", header =TRUE, sep=";", dec="." , check.names=TRUE)

###########################################################################################################



## Compute end of month values
MonthlySpotRates <- NULL
MonthlyInterestRates3m <- NULL
for(i in 2:dim(SpotRates)[1]){
  
  if(SpotRates[i,2]!=SpotRates[(i-1),2]){MonthlySpotRates <- rbind(MonthlySpotRates,SpotRates[(i-1),])} 
  if(InterestRates3m[i,2]!=InterestRates3m[(i-1),2]){MonthlyInterestRates3m <- rbind(MonthlyInterestRates3m,InterestRates3m[(i-1),])} 

}




##########################################################################################################
## Nominal Exchange Rate



T <- dim(MonthlySpotRates)[1]

# 1. EUR/USD

s <- 100*log(MonthlySpotRates[,4])
x <- s[3:(T-1)]
ds <- (s[4:T] - s[3:(T-1)])
ds1 <- (s[3:(T-1)] - s[2:(T-2)])
ds2 <- (s[2:(T-2)] - s[1:(T-3)])




# Graphing
plot.ts(s)

# Fit and report linear model

X <- cbind(x, ds1, ds2)
unit <- lm(ds~X)
summary(unit)

# The STATISTIC in the output is the t-statistic of the gamma coefficient! #####
# augmented dickey fuller tes: 
adfTest(s, lags = 2, type = "c")




# 2. EUR/GBP

s <- 100*log(MonthlySpotRates[,5])
x <- s[3:(T-1)]
ds <- (s[4:T] - s[3:(T-1)])
ds1 <- (s[3:(T-1)] - s[2:(T-2)])
ds2 <- (s[2:(T-2)] - s[1:(T-3)])


X <- cbind(x, ds1, ds2)
unit <- lm(ds~X)
summary(unit)

# The STATISTIC in the output is the t-statistic of the gamma coefficient! #####
adfTest(s, lags = 2,type = "c")





###################################################  Real Exchange Rate   #######################

## Load data
cpis <- read.csv("cpis.csv", header = TRUE, sep=";", dec="." , check.names=TRUE)

###########################################################################################################

head(cpis)




# Calculate USD real exchange rate 2004/1 = 100

S2004 <- MonthlySpotRates[,4]/MonthlySpotRates[60,4]
Q <- 100*log(S2004*cpis[,13]/cpis[,3])

plot.ts(Q)


x <- Q[3:(T-1)]
dq <- Q[4:T] - Q[3:(T-1)]
dq1 <- Q[3:(T-1)] - Q[2:(T-2)]
dq2 <- Q[2:(T-2)] - Q[1:(T-3)]


X <- cbind(x, dq1, dq2)
unit <- lm(dq~X)
summary(unit)
adfTest(Q, lags = 2,type = "c")




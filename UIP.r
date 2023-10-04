# uncovered interest rate parity (UIP)
# libraries: 
library(foreign)
library(sandwich)
library(lmtest)



#######################################################################
## Load data
SpotRates <- read.csv("fxrates.csv", header = TRUE, sep=";",
                      dec="." , check.names=TRUE)
InterestRates3m <- read.csv("irates3M.csv", header =TRUE,
                            sep=";", dec="." , check.names=TRUE)

########################################################################

print(SpotRates)

#write.table(SpotRates, file = "fxrates.csv", sep = ";", dec = ".")
#write.table(InterestRates3m, file = "irates3m.csv", sep = ";", dec = ".")


## Compute end of month values    Attention: != means unequal!
MonthlySpotRates <- NULL
MonthlyInterestRates3m <- NULL
for(i in 2:dim(SpotRates)[1]){
  
  if(SpotRates[i,2]!=SpotRates[(i-1),2]){MonthlySpotRates <- rbind(MonthlySpotRates,SpotRates[(i-1),])} 
  if(InterestRates3m[i,2]!=InterestRates3m[(i-1),2]){MonthlyInterestRates3m <- rbind(MonthlyInterestRates3m,InterestRates3m[(i-1),])} 

}

print(MonthlySpotRates)

##########################################################################################################
## Compute quarterly values
QuarterlySpotRates <- NULL
QuarterlyInterestRates3m <- NULL
for(i in seq(1,dim(MonthlySpotRates)[1], by=3)){
  
  QuarterlySpotRates <- rbind(QuarterlySpotRates,MonthlySpotRates[i,])
  QuarterlyInterestRates3m <- rbind(QuarterlyInterestRates3m, MonthlyInterestRates3m[i,])
  
}


##########################################################################################################
## UIP Example


##  Monthly Data

T <- dim(MonthlySpotRates)[1]

N <- 90

# 1. EUR/USD
# pick EUR/USD exchange rate and compute log changes
S <- MonthlySpotRates[,4]
y <- 100*(log(S[4:N]) - log(S[1:(N-3)]))


# pick interest rates and compute the differential
i <- MonthlyInterestRates3m[,14]   # Euro interest rate
i_star <- MonthlyInterestRates3m[,4]   # Dollar interest rate
x <- (i-i_star)[1:(N-3)]/4


# Graphing
graphdata <- cbind(y,x)
plot.ts(graphdata, plot.type = "single", col = c("blue","red"))

# Fit and report linear model
USmod <- lm(y~x)
summary(USmod)


# Economic Test has been H0: Beta = 1

coeffs <- USmod$coefficients
se <- sqrt(diag(vcov(USmod)))
print(coeffs)
print(se)
tstat <- (coeffs[2] - 1)/se[2]   # - 1 because beta=1 
print(tstat)


# 2. EUR/JPY
# pick EUR/JPY exchange rate and compute log changes
S <- MonthlySpotRates[,5]
y <- 100*(log(S[4:N]) - log(S[1:(N-3)]))

# pick interest rates and compute the differential
i <- MonthlyInterestRates3m[,14]
i_star <- MonthlyInterestRates3m[,5]
x <- (i-i_star)[1:(N-3)]/4

# Fit and report linear model
JPYmod <- lm(y~x)
summary(JPYmod)

# Economic Test has been H0: Beta = 1

coeffs <- JPYmod$coefficients
se <- sqrt(diag(vcov(JPYmod)))
print(coeffs)
print(se)
tstat <- (coeffs[2] - 1)/se[2]
print(tstat)







# 3. EUR/GBP
# pick EUR/GBP exchange rate and compute log changes
S <- MonthlySpotRates[,6]
y <- 100*(log(S[4:N]) - log(S[1:(N-3)]))

# pick interest rates and compute the differential
i <- MonthlyInterestRates3m[,14]
i_star <- MonthlyInterestRates3m[,6]
x <- (i-i_star)[1:(N-3)]/4

# Fit and report linear model
GBPmod <- lm(y~x)
summary(GBPmod)





################ Diagnostics ################################

# Durbin Watson Test statistic

U <- GBPmod$residuals
T <- length(U)

ut <- U[2:T]
ut_1 <- U[1:(T-1)]

DW <- (t(ut - ut_1)%*%(ut - ut_1))/(t(ut)%*%ut)
print(DW)

######################################################################

## Ljung Box Q

ut <- U[5:T]
ut_1 <- U[4:(T-1)]
ut_2 <- U[3:(T-2)]
ut_3 <- U[2:(T-3)]
ut_4 <- U[1:(T-4)]

errors <- cbind(ut, ut_1, ut_2, ut_3, ut_4)
correls <- cor(errors)
print(correls[,1])
LBQTest <- T*(T + 2)*(correls[2,1]^2/(T-1) + correls[3,1]^2/(T-2) + correls[4,1]^2/(T-3) + correls[5,1]^2/(T-4))
print(LBQTest)

critval <- qchisq(0.95, 4)
print(critval)

p_val <- 1 - pchisq(LBQTest, 4)
print(p_val)
################################

summary(GBPmod)
coeftest(GBPmod,vcov=sandwich)




##################################################################################

# Quarterly Data

N <- dim(QuarterlySpotRates)[1]
N <- 35   # Until Financial Crisis


# 1. EUR/USD
# pick EUR/USD exchange rate and compute log changes
S <- QuarterlySpotRates[,4]
y <- 100*(log(S[2:N]) - log(S[1:(N-1)]))

# pick interest rates and compute the differential
i <- QuarterlyInterestRates3m[,14]
i_star <- QuarterlyInterestRates3m[,4]
x <- (i-i_star)[1:(N-1)]/4

# Fit and report linear model
USDmod <- lm(y~x)
summary(USDmod)

U <- USDmod$residuals
T <- length(U)

ut <- U[2:T]
ut_1 <- U[1:(T-1)]

DW <- (t(ut - ut_1)%*%(ut - ut_1))/(t(ut)%*%ut)
print(DW)


## Ljung Box Q

ut <- U[5:T]
ut_1 <- U[4:(T-1)]
ut_2 <- U[3:(T-2)]
ut_3 <- U[2:(T-3)]
ut_4 <- U[1:(T-4)]

errors <- cbind(ut, ut_1, ut_2, ut_3, ut_4)
correls <- cor(errors)
print(correls[,1])
LBQTest <- T*(T + 2)*(correls[2,1]^2/(T-1) + correls[3,1]^2/(T-2) + correls[4,1]^2/(T-3) + correls[5,1]^2/(T-4))
print(LBQTest)

critval <- qchisq(0.95, 4)
print(critval)

p_val <- 1 - pchisq(LBQTest, 4)
print(p_val)
################################

coeftest(USDmod,vcov=sandwich)





# 2. EUR/JPY
# pick EUR/JPY exchange rate and compute log changes
S <- QuarterlySpotRates[,5]
y <- 100*(log(S[2:N]) - log(S[1:(N-1)]))

# pick interest rates and compute the differential
i <- QuarterlyInterestRates3m[,14]
i_star <- QuarterlyInterestRates3m[,5]
x <- (i-i_star)[1:(N-1)]/4

# Fit and report linear model
JPYmod <- lm(y~x)
summary(JPYmod)

U <- JPYmod$residuals
T <- length(U)

ut <- U[2:T]
ut_1 <- U[1:(T-1)]

DW <- (t(ut - ut_1)%*%(ut - ut_1))/(t(ut)%*%ut)
print(DW)



######################################################################

## Ljung Box Q

ut <- U[5:T]
ut_1 <- U[4:(T-1)]
ut_2 <- U[3:(T-2)]
ut_3 <- U[2:(T-3)]
ut_4 <- U[1:(T-4)]

errors <- cbind(ut, ut_1, ut_2, ut_3, ut_4)
correls <- cor(errors)
print(correls[,1])
LBQTest <- T*(T + 2)*(correls[2,1]^2/(T-1) + correls[3,1]^2/(T-2) + correls[4,1]^2/(T-3) + correls[5,1]^2/(T-4) )
print(LBQTest)

critval <- qchisq(0.95, 4)
print(critval)

p_val <- 1 - pchisq(LBQTest, 4)
print(p_val)
################################


coeftest(GBPmod,vcov=sandwich)











# 3. EUR/GBP
# pick EUR/GBP exchange rate and compute log changes
S <- QuarterlySpotRates[,6]
y <- 100*(log(S[2:N]) - log(S[1:(N-1)]))

# pick interest rates and compute the differential
i <- QuarterlyInterestRates3m[,14]
i_star <- QuarterlyInterestRates3m[,6]
x <- (i-i_star)[1:(N-1)]/4

# Fit and report linear model
GBPmod <- lm(y~x)
summary(GBPmod)

#coeftest(mod,vcov=sandwich)

U <- GBPmod$residuals
T <- length(U)

ut <- U[2:T]
ut_1 <- U[1:(T-1)]

DW <- (t(ut - ut_1)%*%(ut - ut_1))/(t(ut)%*%ut)
print(DW)


## Ljung Box Q

ut <- U[5:T]
ut_1 <- U[4:(T-1)]
ut_2 <- U[3:(T-2)]
ut_3 <- U[2:(T-3)]
ut_4 <- U[1:(T-4)]

errors <- cbind(ut, ut_1, ut_2, ut_3, ut_4)
correls <- cor(errors)
print(correls[,1])
LBQTest <- T*(T + 2)*(correls[2,1]^2/(T-1) + correls[3,1]^2/(T-2) + correls[4,1]^2/(T-3) + correls[5,1]^2/(T-4) )
print(LBQTest)

critval <- qchisq(0.95, 4)
print(critval)

p_val <- 1 - pchisq(LBQTest, 4)
print(p_val)
################################

##########################################################################################################

#########################################################################################


# News Approach

#N <- dim(SpotRates)[1]
N <- 2200

S <- SpotRates[,4]
y <- 100*(log(S[2:N]) - log(S[1:(N-1)]))


# pick interest rates and compute the differential
i <- InterestRates3m[,14]
i_star <- InterestRates3m[,4]
x <- (i-i_star)[1:(N-1)]/360

deltax <- (i-i_star)[2:N]/360 - (i-i_star)[1:(N-1)]/360

# Note that in theory it is (i_star - i)


X <- cbind(x,deltax)

# Graphing
#graphdata <- cbind(y,x)
#plot.ts(graphdata, plot.type = "single", col = c("blue","red"))

# Fit and report linear model
USmod <- lm(y~X)
summary(USmod)












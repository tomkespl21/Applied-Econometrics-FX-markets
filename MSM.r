library(foreign)
library(sandwich)
library(lmtest)
library(tseries)
library(MSwM)


###########################################################################################################
## Load data
SpotRates <- read.csv("fxrates.csv", header = TRUE, sep=";", dec="." , check.names=TRUE)
InterestRates3m <- read.csv("irates3M.csv", header =TRUE, sep=";", dec="." , check.names=TRUE)

###########################################################################################################

N <- dim(InterestRates3m)[1]


i <- InterestRates3m[1:(N-1),14]/360
i_star <- InterestRates3m[1:(N-1),4]/360
x <-as.matrix(i-i_star)


S <- SpotRates[,4]
y <- 100*(log(S[2:N]) - log(S[1:(N-1)]))

uiplin <- lm(y ~ x)
summary(uiplin)



## Ljung Box Q  ##############################################################
Ulinsq <- uiplin$residuals^2

for(i in 1:10){
  LBQ <- Box.test(Ulinsq,lag = i,type = "Ljung")
  print(LBQ)
}



############################  Markov Switching Model ###################


################################
# Original Hamilton 1989 model

const <- matrix(1,(N-1),1)

Hamlin <- lm(y ~ const - 1)
summary(Hamlin)


# Set up the model
# Choose the model which may contain switches (uiplin)
# k - Number of regimes
# sw - Which coefficients to switch (Start with coeffs of the mean equation, last is regime variance)
MSHam = msmFit(Hamlin, k = 2, sw = c(TRUE, TRUE))
summary(MSHam)

par(mar=c(3,3,3,3))      # sets margins around the plots, default is mar=c(5,4,4,3)
plotProb(MSHam, which=2) # which sets the plot type: 1 = regime probs, or 2 = resids vs. probs


##############################
# UIP Model


MSuip = msmFit(uiplin, k = 2, sw = c(TRUE, TRUE, TRUE))

summary(MSuip)


par(mar=c(3,3,3,3))      # sets margins around the plots, default is mar=c(5,4,4,3)
plotProb(MSuip, which=2) # which sets the plot type: 1 = regime probs, or 2 = resids vs. probs


# Extract probabilities, residuals, and variances from the model
probs <- MSuip@Fit@filtProb
resids <- MSuip@Fit@error
vars <- MSuip@std^2

# Overall residuals are weighted average of regime residuals  
uipres <- probs[,1]*resids[,1] + probs[,2]*resids[,2]

mu1 <- y - resids[,1]
mu2 <- y - resids[,2]

# Calculate conditional volatility
condvar <- probs[,1]*(mu1**2+vars[1])+((1-probs[,1])*(mu2**2+vars[2]))-((probs[,1]*mu1)+((1-probs[,1])*mu2))**2
plot.ts(condvar)

# Calculate standardized residuals
standres <- uipres/sqrt(condvar)
plot.ts(standres)

## Ljung Box Q  ##############################################################
Usq <- standres^2

for(i in 1:10){
LBQ <- Box.test(Usq,lag = i,type = "Ljung")
 print(LBQ)
}

################################









##################  Simple MSW Model Monthly Data #############################################
## Compute end of month values
MonthlySpotRates <- NULL
MonthlyInterestRates3m <- NULL
for(i in 2:dim(SpotRates)[1]){
  
  if(SpotRates[i,2]!=SpotRates[(i-1),2]){MonthlySpotRates <- rbind(MonthlySpotRates,SpotRates[(i-1),])} 
  if(InterestRates3m[i,2]!=InterestRates3m[(i-1),2]){MonthlyInterestRates3m <- rbind(MonthlyInterestRates3m,InterestRates3m[(i-1),])} 
  
}


N <- dim(MonthlySpotRates)[1]

S <- MonthlySpotRates[,4]
y <- 100*(log(S[4:N]) - log(S[1:(N-3)]))

i <- MonthlyInterestRates3m[,14]
i_star <- MonthlyInterestRates3m[,4]
x <- (i-i_star)[1:(N-3)]/4


# Fit and report linear model
uiplinmonth <- lm(y~x)
summary(uiplinmonth)


# Estimate and report MSM 
MSuipmonth = msmFit(uiplinmonth, k = 2, sw = c(TRUE, TRUE, TRUE))
summary(MSuipmonth)


par(mar=c(3,3,3,3))
plotProb(MSuipmonth, which=1)






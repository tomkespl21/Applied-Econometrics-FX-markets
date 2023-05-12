################################################################################
# Project: Phillips Curve Estimations                                          #
# Dataset: firstprog_data.csv                                                  #
# Date: 27.03.20                                                               #
# Author: Stefan Reitz  / Tomke Splettstößer                                   #
################################################################################

rm(list=ls())     # clear workspace

library(tidyverse)


#Data <- read.csv("C:/work/R/Data/firstprog_data.csv", header =TRUE,sep=";", dec="." , check.names=TRUE)
# I prefer to set working direction before s.t. path not needed 

# read_csv2 because its semicolon separated
data <- read_csv2("firstprog_data.csv")

view(data)

#print(data[1:10,])
# print function not really necessary 
data[1:10,]


# How long is the data set(time series dimension)?
# T <- dim(Data)[1]
#print(TS)
# dont like the use of name "T" because in R this is short for "true"

# indexing to get number of rows 
# R starts counting at 1 not 0 ! 
TS <- dim(data)[1]

# Variable definitions

price <- data[2:TS,5]
prod <- data[2:TS,2]
wage <- data[2:TS,4]
unemp <- data[2:TS,8]

# Reitz lagging
#pricelag <- Data[1:(TS-1),5]
#prodlag <- Data[1:(TS-1),2]
#wagelag <- Data[1:(TS-1),4]


# lagging with dplyr 
pricelag <- lag(price,1)
prodlag <- lag(prod,1)
wagelag <- lag(wage,1)
unemplag <- lag(unemp,1)







# Variable transformations

pricedouble <- price * 2
lnprice <- log(price)
lnpricelag <- log(pricelag)


infl <- 100*(lnprice - lnpricelag)
w <- 100*(log(wage) - log(wagelag))
mpl <- 100*(log(prod) - log(prodlag))

# also check the function diff()


TS <- length(infl)  # Adjust length of the time series

# Graphing Data

graphdata <- cbind(mpl,w)

plot.ts(graphdata, plot.type = "multiple")
# indexing because mpl and w are data frames
plot(mpl[,1],w[,1])


# Doing simple regressions

X <- cbind(infl[4:(T-1)],infl[3:(T-2)],infl[2:(T-3)],infl[1:(T-4)],mpl[5:T])
y <- w[5:T]

wageeq1 <- lm(y ~ X)
summary(wageeq1)


############################
X <- cbind(infl[4:(T-1)],infl[3:(T-2)],infl[2:(T-3)],infl[1:(T-4)],mpl[5:T],unemp[5:T])
y <- w[5:T]
colnames(X) <- c("Infl_-1", "Infl_-2", "Infl_-3", "Infl_-4", "MPL", "Unemp")

wageeq2 <- lm(y ~ X)
summary(wageeq2)

############################
X <- cbind(infl[4:(T-1)],infl[3:(T-2)],infl[2:(T-3)],infl[1:(T-4)],mpl[5:T],unemp[5:T])
y <- infl[5:T]
colnames(X) <- c("Infl_-1", "Infl_-2", "Infl_-3", "Infl_-4", "MPL", "Unemp")

Philleq <- lm(y ~ X)
summary(Philleq)



# End earlier to identify 'Flattening of the Phillips curve'

End <- 169  # 2000:1

X <- cbind(infl[4:(End-1)],infl[3:(End-2)],infl[2:(End-3)],infl[1:(End-4)],mpl[5:End],unemp[5:End])
y <- infl[5:End]
colnames(X) <- c("Infl_-1", "Infl_-2", "Infl_-3", "Infl_-4", "MPL", "Unemp")

Philleqfirst <- lm(y ~ X)
summary(Philleqfirst)




########################################################

#Work with regression output

U <- Philleq$residuals
inflhat <- infl[5:T] - U

graphdata <- cbind(infl[5:T],inflhat)
plot.ts(graphdata, plot.type = "single", col = c("blue","red"))

########################################################

#Durbin Watson Statistic
U <- Philleq$residuals
#U <- wageeq2$residuals
T <- length(U)

ut <- U[2:T]
ut_1 <- U[1:(T-1)]

DW <- (t(ut - ut_1)%*%(ut - ut_1))/(t(ut)%*%ut)
print(DW)

#######################################################

## Ljung Box Q

ut <- U[5:T]
ut_1 <- U[4:(T-1)]
ut_2 <- U[3:(T-2)]
ut_3 <- U[2:(T-3)]
ut_4 <- U[1:(T-4)]

errors <- cbind(ut, ut_1, ut_2, ut_3, ut_4)

print(errors)

correls <- cor(errors)
print(correls)

LBQTest <- T*(T + 2)*(correls[2,1]^2/(T-1) + correls[3,1]^2/(T-2) + correls[4,1]^2/(T-3) + correls[5,1]^2/(T-4))
print(LBQTest)

critval <- qchisq(0.95, 4)
print(critval)

p_val <- 1 - pchisq(LBQTest, 4)
print(p_val)
#########################################################


# We leave the coding for the heteroscedasticiy test and the Jarque Bera Test
# for an excercise


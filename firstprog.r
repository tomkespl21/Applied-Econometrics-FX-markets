####################################################################
# Project: Phillips Curve Estimations                              #            
# Dataset: firstprog_data.csv                                      #             
#                                                                  #                    #             
####################################################################

rm(list=ls())     # clear workspace


# load in data
data <- read.csv("firstprog_data.csv", header =TRUE,sep=";", dec="." , check.names=TRUE)
# I prefer to set working direction before s.t. path not needed 


View(data) # obv. to view data  
str(data)  # display structure of R object

  
  
print(data[1:10,]) # show first 10 rows 
# print function not really necessary in R (different to python)
data[1:10,]

# How long is the data set(time series dimension)?
T <- dim(data)[1]
# dont like the use of name "T" because in R this is short for "true"

# indexing to get number of rows:
TS <- dim(data)[1] # R starts counting at 1 not 0 ! 
TS



# variable transformations:
 price <- data[2:TS,5] # assign 5th column starting with second row  
 prod <- data[2:TS,2]  # assign 2th column starting with second row
 wage <- data[2:TS,4]  # assign 4th column starting with second row
 unemp <- data[2:TS,8] # assign 8th column starting with second row


pricelag_R <- data[1:(TS-1),5] #assign 5th column starting with first row  
prodlag_R <- data[1:(TS-1),2]  #assign 2th column starting with first row
wagelag_R <- data[1:(TS-1),4]  #assign 4th column starting with first row



# Variable transformations
pricedouble <- price * 2    # assign double price to variable
lnprice <- log(price)       # assign logarithm of price to variable
lnpricelag <- log(pricelag_R) # assign lagged logarithm of price to variable


infl <- 100*(lnprice - lnpricelag)   # assign/compute inflation
w <- 100*(log(wage) - log(wagelag_R)) # assign/compute wage changes
mpl <- 100*(log(prod) - log(prodlag_R)) # assign/compute  prod changes



#TS <-  dim(data)[1] # Adjust length of the time series
TS <-  length(infl) # 

 
# save data needed for time series graph:
graphdata <- cbind(mpl,w) # combine mpl with w

# plot time series for mpl and wage change 
plot.ts(graphdata, plot.type = "multiple")




# independent variable matrix  
X <- cbind(data$infl[5:(TS-1)],infl[4:(TS-2)],infl[3:(TS-3)],
           infl[2:(TS-4)],mpl[6:TS])

# dependent variable 
y <- w[6:TS]

# run regression
wageeq1 <- lm(y ~ X)

# show regression table:
summary(wageeq1)


############################

# add additional variables to reduces omitted variable bias

X <- cbind(infl[4:(TS-1)],infl[3:(TS-2)],infl[2:(TS-3)],infl[1:(TS-4)],mpl[5:TS],unemp[5:TS])
y <- w[5:TS]
colnames(X) <- c("Infl_-1", "Infl_-2", "Infl_-3", "Infl_-4", "MPL", "Unemp")

wageeq2 <- lm(y ~ X)
summary(wageeq2)

# first lag inflation , mpl and unemp significant 

############################
X <- cbind(infl[4:(TS-1)],infl[3:(TS-2)],infl[2:(TS-3)],infl[1:(TS-4)],mpl[5:TS],unemp[5:TS])
y <- infl[5:TS]
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
inflhat <- infl[5:TS] - U

graphdata <- cbind(infl[5:TS],inflhat)
plot.ts(graphdata, plot.type = "single", col = c("blue","red"))

########################################################

#Durbin Watson Statistic

library(lmtest) # for dwtest function 

dwtest(Philleq)

# compute sta durbin watson statistic without function:
U <- Philleq$residuals
#U <- wageeq2$residuals
TS <- length(U)

ut <- U[2:TS]
ut_1 <- U[1:(TS-1)]

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


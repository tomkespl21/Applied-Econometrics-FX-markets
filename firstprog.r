####################################################################
# Project: Phillips Curve Estimations                              #            
# Dataset: firstprog_data.csv                                      #             
#                                                                  #                    #             
####################################################################

rm(list=ls())     # clear workspace


# Reitz option 
data <- read.csv("firstprog_data.csv", header =TRUE,sep=";", dec="." , check.names=TRUE)
# I prefer to set working direction before s.t. path not needed 

library(dplyr)

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

 price <- data[2:TS,5] 
 prod <- data[2:TS,2]
 wage <- data[2:TS,4]
 unemp <- data[2:TS,8]


pricelag_R <- data[1:(TS-1),5]
prodlag_R <- data[1:(TS-1),2]
wagelag_R <- data[1:(TS-1),4]


# lagging with dplyr 
pricelag <- lag(price,1)
prodlag <- lag(prod,1)
wagelag <- lag(wage,1)
unemplag <- lag(unemp,1)

# test if lag variavles are the same
pricelag == pricelag_R






# Variable transformations
#pricedouble <- price * 2
#lnprice <- log(price)
#lnpricelag <- log(pricelag)


#infl <- 100*(lnprice - lnpricelag)
#w <- 100*(log(wage) - log(wagelag)) # wage changes
#mpl <- 100*(log(prod) - log(prodlag)) # prod changes



#TS <-  dim(data)[1] # Adjust length of the time series
TS <-  length(data$infl)
# Graphing Data
 
graphdata <- cbind(data$mpl,data$w) # combine mpl with w

plot.ts(graphdata, plot.type = "multiple")
# indexing because mpl and w are data frames
ggplot(data,aes(x=w,y=mpl))+
  geom_point()+
  xlab("mpl")+
  ylab("w")+
  ggtitle("Scatterplot")


# tidyverse version of regression 
reg1data <- 
  data %>% 
  mutate(laginfl1 = lag(infl,1),
         laginfl2 = lag(infl,2),
         laginfl3 = lag(infl,3),
         laginfl4 = lag(infl,4)) %>% 
  select(infl,laginfl1,laginfl2,laginfl3,laginfl4,mpl,w) %>%
  slice(6:n())

reg1 <- lm(w ~ laginfl1 + laginfl2 + laginfl3 + laginfl4 + mpl,data=reg1data)

summary(reg1)


# Reitz version 
X <- cbind(data$infl[5:(TS-1)],data$infl[4:(TS-2)],data$infl[3:(TS-3)],
           data$infl[2:(TS-4)],data$mpl[6:TS])
y <- data$w[6:TS]

wageeq1 <- lm(y ~ X)
summary(wageeq1)


############################
X <- cbind(infl[4:(TS-1)],infl[3:(TS-2)],infl[2:(TS-3)],infl[1:(TS-4)],mpl[5:TS],unemp[5:T])
y <- w[5:TS]
colnames(X) <- c("Infl_-1", "Infl_-2", "Infl_-3", "Infl_-4", "MPL", "Unemp")

wageeq2 <- lm(y ~ X)
summary(wageeq2)

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


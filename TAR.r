


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


N <- dim(MonthlySpotRates)[1]

S <- MonthlySpotRates[,4]
y <- 100*(log(S[4:N]) - log(S[1:(N-3)]))

i <- MonthlyInterestRates3m[,14]
i_star <- MonthlyInterestRates3m[,4]
x <- (i-i_star)[1:(N-3)]/4

T <- length(x)

##############################################   TAR Model ######################################

# Define variables and starting values

# empty matrices:
rss <- matrix(0,T,1)
flag <- matrix(0,T,1)

thresh <- sort(abs(x))               # sorted differential
low <- 1 + as.integer(0.15*T)
high <- T - as.integer(0.15*T)

rss_min <- 10000000000000
tau <- thresh[low]



# Go through loop
for(i in low:high){
  
  # Set indicator variable
  for(j in 1:T){
    if(abs(x[j]) < thresh[i]){flag[j] <- 0}
    else {flag[j] <- 1}
  }
  
  # Do piecewise regression
  outer_x <- flag*x
  inner_x <- (1-flag)*x
  X <- cbind(outer_x,inner_x)
  tarmodel <- lm(y ~ X)
  rss[i] <- sum(tarmodel$residuals^2)
  
  # Book keeping
  if(rss[i] < rss_min){
    rss_min <- rss[i]
    tau <- thresh[i]
  }
  
}




print(c(tau,rss_min))

graphrss <- rss[low:high]
graphthresh <- thresh[low:high]
plot(graphthresh, graphrss, type = "l")


#######################################################  TAR model output  ########################
for(j in 1:T){
  if(abs(x[j]) < tau){flag[j] <- 0} 
  else {flag[j] <- 1}
}

outer_x <- flag*x
inner_x <- (1-flag)*x
X <- cbind(outer_x,inner_x)
colnames(X) <- c("Outer", "Inner")
tarmodel <- lm(y ~ X)
summary(tarmodel)


##############  Graph ###################

upper <- matrix(tau,T,1)
lower <- matrix(-tau,T,1)
graphdata <- cbind(upper, lower, x)
plot.ts(graphdata, plot.type = c("single"), col = c("red","red","blue"))


########################################  Compare to standard OLS #############################

olsmodel <- lm(y ~ x)
summary(olsmodel)



#############################  Compare Fit of the Models #############################################

aictar <- T*log(sum(tarmodel$residuals^2)) + 2*3  # ( 2 * Number of regressors)
bictar <- T*log(sum(tarmodel$residuals^2)) + log(T)*3

print(c(aictar,bictar))

aiclin <- T*log(sum(olsmodel$residuals^2)) + 2*3  # ( 2 * Number of regressors)
biclin <- T*log(sum(olsmodel$residuals^2)) + log(T)*3

print(c(aiclin,biclin))



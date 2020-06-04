# Solution to Financed 567 Homework 6, spring 2020

# read the csv file containing the index data and compute returns
SP500 <- read.csv("F567.s2020.HW5.data.csv") 
SP500 <- data.frame(date = SP500[2:nrow(SP500),1],
                     return = log(SP500[2:nrow(SP500),5]/SP500[1:(nrow(SP500)-1),5]))
ret = SP500$return
samplevariance = sum(ret^2)/length(ret)
spstd = sqrt(sum(ret^2)/length(ret)) #Sample standard deviation--need this in Q1b, c, and d

##Q1a
# parameters to estimate are alpha, beta, sigma, sigma_1

garch11a <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[4]^2 
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess = c(0.1,0.8,0.01,0.01)
Q1a = optim(guess,garch11a)
alpha = Q1a$par[1]
beta = Q1a$par[2]
sigma = Q1a$par[3]
sigma1 = Q1a$par[4]

##Q1b
# parameters to estimate are alpha, beta, sigma_1
# sigma is set to sample std. deviation

#Objective function (negative log likelihood)
garch11b <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[3]^2 
  
  #(negative) log likelihood
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ||  x[3]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*spstd^2 + x[1]*ret[i]^2 + x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  
  return(NeglogLH)
}

# Optimization #
Q1b =optim(c(0.1,0.8,0.001),garch11b)
Q1b
alphaQ1b = Q1b$par[1]
betaQ1b = Q1b$par[2]
sigma1Q1b = Q1b$par[3]

##Q1C
# parameters to estimate are alpha, beta, sigma
# sigma_1 is set to sample std. deviation

#(negative) log likelihood
garch11c <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = spstd^2 
  
  #Constraints
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ||  x[3]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
    
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  
  return(NeglogLH)
}

# Optimization #
Q1c =optim(c(0.1,0.8,0.001), garch11c)
Q1c
alphaQ1c = Q1c$par[1]
betaQ1c = Q1c$par[2]
sigmaQ1c = Q1c$par[3]

##Q1d
# parameters to estimate are alpha and beta
# sigma and sigma_1 are set to sample std. deviation

#(negative) log likelihood
garch11d <- function(x) {
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = spstd^2  
  
  #(negative) log likelihood
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*spstd^2 + x[1]*ret[i]^2 + x[2]*sigmasqhat[i]
    }
    
    #Likelihood 
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  
  return(NeglogLH)
}

# Optimization #
Q1d <- optim(c(0.1,0.8),garch11d)
Q1d
alphaQ1d = Q1d$par[1]
betaQ1d = Q1d$par[2]

##Question 1e
#use the 4 sets of estimates to compute conditional variance for February 24, 2020
#this is the day after the last date of data
sigmasqhatA = rep(0,length(ret)+1)
sigmasqhatA[1] = sigma1Q1b^2 
for (i in 1:length(ret)) {
  sigmasqhatA[i+1]=(1-Q1a$par[1]-Q1a$par[2])*Q1a$par[3]^2+Q1a$par[1]*ret[i]^2+Q1a$par[2]*sigmasqhatA[i]
}
Q1e1 = sqrt(sigmasqhatA[length(ret)+1])

sigmasqhatB = rep(0,length(ret)+1)
sigmasqhatB[1] = sigma1Q1b^2 
for (i in 1:length(ret)) {
  sigmasqhatB[i+1]=(1-Q1b$par[1]-Q1b$par[2])*spstd^2+Q1b$par[1]*ret[i]^2+Q1b$par[2]*sigmasqhatB[i]
}
Q1e2 = sqrt(sigmasqhatB[length(ret)+1])

sigmasqhatC = rep(0,length(ret)+1)
sigmasqhatC[1] = spstd^2
for (i in 1:length(ret)) {
  sigmasqhatC[i+1]=(1-Q1c$par[1]-Q1c$par[2])*Q1c$par[3]^2+Q1c$par[1]*ret[i]^2+Q1c$par[2]*sigmasqhatC[i]
}
Q1e3 = sqrt(sigmasqhatC[length(ret)+1])

sigmasqhatD = rep(0,length(ret)+1)
sigmasqhatD[1] = spstd^2
for (i in 1:length(ret)) {
  sigmasqhatD[i+1]=(1-Q1d$par[1]-Q1d$par[2])*spstd^2+Q1d$par[1]*ret[i]^2+Q1d$par[2]*sigmasqhatD[i]
}
Q1e4 = sqrt(sigmasqhatD[length(ret)+1])


##Question 2
#let's write a function to compute the garch11 variance forecasts
#arguments are alpha, beta, sigma, return at t, cond. variance at t, x[6] = K (days)
garchforecast <- function(x) {  
  K = x[6]
  sigmasqhat = rep(0,K)
  sigmasqhat[1] = (1-x[1]-x[2])*x[3]^2+x[1]*x[4]^2 + x[2]*x[5]
  
  #now compute the variance forecasts through day K
  for (i in 2:K) {
      sigmasqhat[i] = x[3]^2 + (x[1]+x[2])^(K-1) * (sigmasqhat[1]-x[3]^2)
  }
  forecast = sum(sigmasqhat)
  return(forecast)
}

input = c(Q1a$par[1],Q1a$par[2],Q1a$par[3],ret[length(ret)],sigmasqhatA[length(ret)],21)
Q2a1 = garchforecast(input)
input = c(Q1b$par[1],Q1b$par[2],spstd,ret[length(ret)], sigmasqhatB[length(ret)],21)
Q2a2 = garchforecast(input)
input = c(Q1c$par[1],Q1c$par[2],spstd,ret[length(ret)],sigmasqhatC[length(ret)],21)
Q2a3 = garchforecast(input)
input = c(Q1d$par[1],Q1d$par[2],spstd,ret[length(ret)],sigmasqhatD[length(ret)],21)
Q2a4 = garchforecast(input)
Q2b1 = sqrt(12*Q2a1)
Q2b2 = sqrt(12*Q2a2)
Q2b3 = sqrt(12*Q2a3)
Q2b4 = sqrt(12*Q2a4)



##Question 3
##A

library(tseries)  #must have installed package "tseries"
inter = (1-Q1a$par[1]-Q1a$par[2])*Q1a$par[3]^2
Q3a = garch(ts(ret),order = c(1,1),control = garch.control(start = c(inter,Q1a$par[1],Q1a$par[2])))
# Q3a= garch(ts(ret),order = c(1,1)) #almost same as garchfit solution
omega_Q3a <- coef(Q3a)[1]
alpha_Q3a <- coef(Q3a)[2]
beta_Q3a <- coef(Q3a)[3]
sigmasq_Q3a <- omega_Q3a/(1-alpha_Q3a-beta_Q3a)

library(fGarch)  #must have installed package "fGarch"

Q3b = garchFit(~ garch(1,1), data = ret, include.mean = FALSE)
omega_Q3b <- coef(Q3b)[1]
alpha_Q3b <- coef(Q3b)[2]
beta_Q3b <- coef(Q3b)[3]
sigmasq_Q3b <- omega_Q3b/(1-alpha_Q3b-beta_Q3b)

#Question 4 NGARCH
# parameters to estimate are alpha, beta, sigma, sigma_1, theta

ngarch11 <- function(x) {
  #x[1] = alpha
  #x[2] = beta
  #x[5] = theta
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[4]^2 
  if (x[1]*(1+x[5]^2)+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(ret)-1)) {
      sigmasqhat[i+1] = (1-x[1]*(1+x[5]^2)-x[2])*x[3]^2+x[1]*(ret[i]-x[5]*sqrt(sigmasqhat[i]))^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess = c(0.1,0.8,0.03,0.04,0.5)
Q4 = optim(guess,ngarch11)
alphaQ4 = Q4$par[1]
betaQ4 = Q4$par[2]
sigmaQ4 = Q4$par[3]
sigma1Q4 = Q4$par[4]
thetaQ4 = Q4$par[5]

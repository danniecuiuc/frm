# Solution to Fin 567 Homework 4 spring 2020 Questions 1 and 2
# Does not include solution to Question 3 because that does not require R
# 

library(fOptions) #needed to compute option values
library(MASS)     #needed to simulate multivariate Normal rvs
library(mvtnorm)  #needed to simulate multivariate t rvs

# Question 1 Monte Carlo VaR and ES using the Normal distribution
# We will first need to use the binomial model to compute the value of the portfolio

ABCcall0 <- CRRBinomialTreeOption(TypeFlag = "ca", S=101.17, X=100, Time = 21/252,
            r=0.01, b=0.01, sigma=0.45, n=21, title = NULL, description = NULL)@price
ABCput0  <- CRRBinomialTreeOption(TypeFlag = "pa", S=101.17, X=100, Time = 21/252,
            r=0.01, b=0.01, sigma=0.45, n=21, title = NULL, description = NULL)@price

DEFcall0 <- CRRBinomialTreeOption(TypeFlag = "ca", S=148.97, X=150, Time = 21/252,
            r=0.01, b=0.01, sigma=0.37, n=21, title = NULL, description = NULL)@price
DEFput0  <- CRRBinomialTreeOption(TypeFlag = "pa", S=148.97, X=150, Time = 21/252,
            r=0.01, b=0.01, sigma=0.37, n=21, title = NULL, description = NULL)@price
V0 <- -60*100*ABCcall0-60*100*ABCput0 - 40*100*DEFcall0 - 40*100*DEFput0


# (a) Compute the 5% MC VaR
set.seed(137)
mu = c(0.0005, 0.0004) 
cov <- matrix(c(0.028^2,0.028*0.023*0.4,0.028*0.023*0.4,0.023^2), nrow=2, ncol=2)
n = 10000
returns = mvrnorm(n, mu = mu, Sigma = cov)
S <- matrix(rep(0,2*n), nrow= n, ncol = 2)
S[1:n,1]= 101.17*exp(returns[1:n,1])
S[1:n,2]= 148.97*exp(returns[1:n,2])
ABCcall=rep(0,n)
ABCput=rep(0,n)
DEFcall=rep(0,n)
DEFput=rep(0,n)
V <- rep(0,n)

# in the loop below, note that the remaining time to expiration is 20 days
for(i in 1:n){
  ABCcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,1], X=100, Time = 20/252,
                r=0.01, b=0.01, sigma=0.45, n=20, title = NULL, description = NULL)@price
  ABCput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,1], X=100, Time = 20/252,
                r=0.01, b=0.01, sigma=0.45, n=20, title = NULL, description = NULL)@price
  
  DEFcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,2], X=150, Time = 20/252,
                r=0.01, b=0.01, sigma=0.37, n=20, title = NULL, description = NULL)@price
  DEFput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,2], X=150, Time = 20/252,
                r=0.01, b=0.01, sigma=0.37, n=20, title = NULL, description = NULL)@price
}
V <- -60*100*ABCcall-60*100*ABCput - 40*100*DEFcall - 40*100*DEFput
PLQ1 = V - V0
MCVaRQ1 <- - quantile(PLQ1, 0.05)

# (b) Compute the expected shortfall
ESQ1 = -mean(PLQ1[PLQ1<=(-MCVaRQ1)])


#Question 2
# Monte Carlo VaR and expected shortfall using bivariate t distribution
# (a) Compute the 5% MC VaR
mu = c(0.0005, 0.0004)
cov <- matrix(c(0.028^2,0.028*0.023*0.4,0.028*0.023*0.4,0.023^2), nrow=2, ncol=2)
nu = 4
scale = ((nu-2)/nu)*cov  #scale matrix input to mvrt() is smaller than cov matrix
n = 10000
returns = rmvt(n, sigma = scale, df = nu, delta = mu) # parameter sigma is the scale matrix
S <- matrix(rep(0,2*n), nrow= n, ncol = 2)
S[1:n,1]= 101.17*exp(returns[1:n,1])
S[1:n,2]= 148.97*exp(returns[1:n,2])
ABCcall=rep(0,n)
ABCput=rep(0,n)
DEFcall=rep(0,n)
DEFput=rep(0,n)
V <- rep(0,n)

for(i in 1:n){
  ABCcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,1], X=100, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.45, n=20, title = NULL, description = NULL)@price
  ABCput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,1], X=100, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.45, n=20, title = NULL, description = NULL)@price
  
  DEFcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,2], X=150, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.37, n=20, title = NULL, description = NULL)@price
  DEFput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,2], X=150, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.37, n=20, title = NULL, description = NULL)@price
}
V <- -60*100*ABCcall-60*100*ABCput - 40*100*DEFcall - 40*100*DEFput
PLQ2 = V - V0
MCVaRQ2 <- - quantile(PLQ2, 0.05)

# (b) Compute the expected shortfall
ESQ2 = -mean(PLQ2[PLQ2<=(-MCVaRQ2)])




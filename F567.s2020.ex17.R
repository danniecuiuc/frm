#Spring 2020 solution to Daily Exercise 17

library(MASS)     #needed to simulate multivariate Normal rvs

# Simulate fundamental values and prices
# First initialize some of the vectors we will need
Sfund = rep(100,21*1440+1)  #element 1 will be minute 0
Strade = rep(100,21*1440+1)
Vfund = rep(0,1000)          # 1,000 Monte Carlo trials
SDfund = rep(0,1000)
V = rep(0,1000)
SD = rep(0,1000)

minutesigma = 0.03/sqrt(1440)
for(i in 1:1000){
  returnfund = mvrnorm(21*1440, mu = 0.00, Sigma = minutesigma^2)
  Vfund[i] = sum(returnfund * returnfund)/21
  SDfund[i] = sqrt(Vfund[i])
  Sfund[1] = 100
  for(j in 2:(21*1440+1)){
    Sfund[j]=Sfund[j-1]*exp(returnfund[j-1]) #note that Sfund[1] is price at minute 0, but returnfund[1] is minute 1 return
  }
  Sbid = floor(100*Sfund)/100
  Sask = ceiling(100*Sfund)/100

  # Simulate trades
  Trade= rbinom(21*1440, 1, 0.5)
  Strade[2:(21*1440+1)]=Sbid[2:(21*1440+1)]*Trade + Sask[2:(21*1440+1)]*(1-Trade)
  return = log(Strade[2:(21*1440+1)]/Strade[1:(21*1440)])
  V[i] = sum(return * return)/21
  SD[i] = sqrt(V[i])
}

meanSDfund = mean(SDfund)
meanSD = mean(SD)


#Spring 2020 solution to Daily Exercise 16

library(MASS)     #needed to simulate multivariate Normal rvs

# Question 1
V = rep(0,1000)
SD = rep(0,1000)
for(i in 1:1000){
returns = mvrnorm(21, mu = 0.00, Sigma = 0.03^2)
V[i] = sum(returns * returns)/21
SD[i] = sqrt(V[i])
}
meanSDQ1 = mean(SD)
stdSDQ1 = sd(SD)

# Question 2
V = rep(0,1000)
SD = rep(0,1000)
minutesigma = 0.03/sqrt(1440)
for(i in 1:1000){
  returns = mvrnorm(21*1440, mu = 0.00, Sigma = minutesigma^2)
  V[i] = sum(returns * returns)/21
  SD[i] = sqrt(V[i])
}
meanSDQ2 = mean(SD)
stdSDQ2 = sd(SD)
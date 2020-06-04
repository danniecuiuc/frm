# Solution to Fin 567 Homework 2 spring 2020 quesions 2-7
#
#
# Question 2

# read the .csv file containing the returns and put them in a dataframe
returns <- read.csv("returns4tickers.2004-2018.csv")

# I prepared the file returns4tickers.2004-2018.csv in Excel
# instead of using that file, let's use the file WRDS4tickers.2004-2018.csv
# which is in the format downloaded from WRDS
WRDSdata <-read.csv("WRDS4tickers.2004-2018.csv")
permnos <- c(84398,86455,10107,14593)
tickers <- c("SPY","XLF","MSFT","AAPL")
returns2 <- WRDSdata[which(WRDSdata$PERMNO == permnos[1]),c(2,4)]
names(returns2)[names(returns2)=="RET"] <- tickers[1] #change the variable name from "RET" to the ticker
for(i in 2:4){
  returns2 <- merge(returns2, WRDSdata[which(WRDSdata$PERMNO == permnos[i]),c(2,4)], by = "date")
  names(returns2)[names(returns2)=="RET"] <- tickers[i]
}

table(WRDSdata$TICKER)
table(WRDSdata$TICKER)

data1 = subset(WRDSdata, TICKER == "SPY", select = c(RET, date))
names(data1)[names(data1) == "RET"] <- "SPY"


# the two dataframes returns and returns2 have the identical data

# Question 3
# Question 3(a)
Q3amean <- sapply(returns[,2:5],mean)
# the following gives the same result
Q3amean2 <-c(mean(returns[,2]), mean(returns[,3]), mean(returns[,4]), mean(returns[,5]))
# now compute the std. dev. and 5% quantile
Q3asd <- sapply(returns[,2:5],sd)
Q3apercentile <- sapply(returns[,2:5],quantile, probs = 0.05)

# Question 3(b)
returns2008 <- returns[which(returns$date >= 20080101 & returns$date <= 20081231), ]
Q3bmean <- sapply(returns2008[,2:5],mean)
Q3bsd <- sapply(returns2008[,2:5],sd)
Q3bpercentile <- sapply(returns2008[,2:5],quantile, probs = 0.05)


print("Question 3(a):")
print(Q3amean)
print(Q3asd)
print(Q3apercentile)
print("Question 3(b):")
print(Q3bmean)
print(Q3bsd)
print(Q3bpercentile)

# Question 4
# put the position values into a vector v
v <- c(5000000,3000000,1000000,1000000)

#compute portfolio P/L for every date
portfolioPL = data.frame(date = returns$date, 
                         PL = v[1]*returns$SPY+v[2]*returns$XLF
                         +v[3]*returns$MSFT+v[4]*returns$AAPL)


#need to find the indexes of 1/2/2008 and 12/31/2009
begin = which(portfolioPL$date == 20080102)
end = which(portfolioPL$date == 20091231)

Ndays = end-begin+1

# now use a loop to compute HS VaR:
HSVaR = numeric(Ndays)
for(t in 1:Ndays){
  HSVaR[t] = -quantile(portfolioPL[(begin + t - 1000):(begin + t - 1), 2], probs = 0.01)
}

#Question 5
#compute equally-weighted covariance matrix and delta-Normal VaR for each day fron 1/2/2008 to 12/31/2009
# we will use r to hold the most recent 1,000 returns
r = matrix(0, nrow = 1000, ncol = 4)
DNVaR = numeric(Ndays) #create a vector to hold the VaRs
#for each date, put the past 1,000 returns into a the matrix r
for(t in 1:Ndays){
  r[1:1000,1]=returns[(begin + t - 1000):(begin + t - 1), 2]
  r[1:1000,2]=returns[(begin + t - 1000):(begin + t - 1), 3]
  r[1:1000,3]=returns[(begin + t - 1000):(begin + t - 1), 4]
  r[1:1000,4]=returns[(begin + t - 1000):(begin + t - 1), 5]
  equalweightedcov = (t(r) %*% r)/1000  #estimate of cov matrix of returns on date begin + t - 1
  DNVaR[t] = 2.326 * sqrt(v %*% equalweightedcov %*% v)
}


#an alternative calculation using the function cov.wt
#this calculation should agree with the previous calculation
DNVaR2 = numeric(Ndays)
weight = rep(1/nrow(r), nrow(r))  #vector of (equal) weights to be passed to cov.wt
for(t in 1:Ndays){
  r[1:1000,1]=returns[(begin + t - 1000):(begin + t - 1), 2]
  r[1:1000,2]=returns[(begin + t - 1000):(begin + t - 1), 3]
  r[1:1000,3]=returns[(begin + t - 1000):(begin + t - 1), 4]
  r[1:1000,4]=returns[(begin + t - 1000):(begin + t - 1), 5]
  weightedcovoutput = cov.wt(r, weight, cor = FALSE, center = FALSE, method = "ML")
  equalweightedcov2 = weightedcovoutput$cov
  DNVaR2[t] = 2.326 * sqrt(v %*% equalweightedcov2 %*% v)
}

# Question 6
# compute exponentially-weighted covariance matrix and delta-Normal VaR for each day fron 1/2/2008 to 12/31/2009
# similar to calculation above, but with different weight vector
# this calcuation uses 1,000 past returns but fewer could be used, as weights are almost zero more than 200 days in the past
exponentialDNVaR = numeric(Ndays)
#compute the vector of exponentially declining weights to be passed to cov.wt
lambda = 0.94
weight = numeric(1000)
weight[1000] = 1 - lambda  #this is the weight for the most recent return
for(i in 999:1){
  weight[i] = lambda*weight[i+1] #weights decline exponentially
}
#for each date, put the past 1,000 returns into a matrix r
for(t in 1:Ndays){
  r[1:1000,1]=returns[(begin + t - 1000):(begin + t - 1), 2]
  r[1:1000,2]=returns[(begin + t - 1000):(begin + t - 1), 3]
  r[1:1000,3]=returns[(begin + t - 1000):(begin + t - 1), 4]
  r[1:1000,4]=returns[(begin + t - 1000):(begin + t - 1), 5]
  weightedcovoutput = cov.wt(r, weight, cor = FALSE, center = FALSE, method = "ML")
  exponentialcov = weightedcovoutput$cov
  exponentialDNVaR[t] = 2.326 * sqrt(v %*% exponentialcov %*% v)
}

#Question 7
#compute WHS VaR for each day fron 1/2/2008 to 12/31/2009 using eta = 0.995
#this follows VaRIntro.pptx slides 35-37, which in turn follows Christoffersen pp. 24-25
WHSVaR = numeric(Ndays)
#first construct the vectors of weights using eta = 0.995
eta = 0.995
tau = 1000:1
weight = numeric(1000)
weight[1000] = (1-eta)/(1-eta^1000)
for(i in 999:1){
  weight[i] = eta*weight[i+1]
}
cumweight = numeric(1000) #cumulative weights

#for each date, put the 1,000 most recent P/Ls and weights into a dataframe
for(t in 1:Ndays){
  WHS_PL = data.frame(PL = portfolioPL[(begin + t - 1000):(begin + t - 1), 2], tau = tau, 
                      weight = weight)
  sortedPL = WHS_PL[order(WHS_PL$PL),]  #contains P/Ls and weights sorted from largest loss to largest profit
  
  #compute the cumulative weights; stop once they are greater than 0.01
  j = 1
  cumweight[1] = sortedPL$weight[1]
  while(cumweight[j] < 0.01){
    cumweight[j+1] = cumweight[j]+sortedPL$weight[j+1]
    j = j + 1
  }
  #interpolate to find the WHS VaR
  WHSVaR[t] = -((cumweight[j]-0.01)*sortedPL$PL[j-1]+(0.01-cumweight[j-1])*sortedPL$PL[j])/(cumweight[j]-cumweight[j-1])
}

#before plotting the four VaR series find the max and min values so we can set the range for the y axix
upperylim = max(max(HSVaR),max(DNVaR),max(exponentialDNVaR),max(WHSVaR))
lowerylim = min(min(HSVaR),min(DNVaR),min(exponentialDNVaR),min(WHSVaR))

#making the dates column for the plot
dates_plot = portfolioPL$date[which(portfolioPL$date == 20080102):which(portfolioPL$date == 20091231)]
dates_for_plot = as.data.frame(dates_plot)
dates_for_plot = transform(dates_for_plot, 
                           dates_plot = as.Date(as.character(dates_plot), "%Y%m%d"))

#now plot the four VaR series
plot(x = dates_for_plot$dates_plot, y = HSVaR, type = 'l', xaxt = "n",
     ylim = c(lowerylim, upperylim), col = "deepskyblue", 
     xlab = "time", ylab = "VaR")
axis(1, dates_for_plot$dates_plot, format(dates_for_plot$dates_plot, "%b%y"), cex.axis = .7)
lines(x = dates_for_plot$dates_plot, y = DNVaR, col = "darkorange")
lines(x = dates_for_plot$dates_plot, y = exponentialDNVaR, col = "firebrick")
lines(x = dates_for_plot$dates_plot, y = WHSVaR, col = "forestgreen")
legend("topleft",
       legend  = c("Hist. simulation","Delta-Norm","Weighted Delta-Norm", "Weighted Historical"),
       col = c("deepskyblue", "darkorange", "firebrick","forestgreen"), lty = 1, cex=0.55)

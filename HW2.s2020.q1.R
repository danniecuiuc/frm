#Solution of Finance 567 HW2 Question 1 (spring 2020)

#need to have the package fOptions installed
#the command install.packages("fOptions") will install it
library("fOptions") #load the package

SP500 = 3295.47 #initial value of S&P 500
strike = 3300   #strike (same for call and put)
ncalls = -60     # number of calls
nputs = -60      #number of puts
tau = 0.08333   #time to expiration (same for call and put)
rate = 0.01     #interest rate
q = 0.02        #dividend yield
vol = 0.22      #implied volatility1
#Question 7(a)
value = ncalls*100*GBSOption(TypeFlag = "c", S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol)@price +
         nputs*100*GBSOption(TypeFlag = 'p', S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol)@price
delta = ncalls*100*GBSGreeks(Selection = "delta", TypeFlag = "c", S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol) +
         nputs*100*GBSGreeks(Selection = "delta", TypeFlag = 'p', S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol)
gamma = ncalls*100*GBSGreeks(Selection = "gamma", TypeFlag = "c", S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol) +
         nputs*100*GBSGreeks(Selection = "gamma", TypeFlag = 'p', S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol)
theta = ncalls*100*GBSGreeks(Selection = "theta", TypeFlag = "c", S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol) +
         nputs*100*GBSGreeks(Selection = "theta", TypeFlag = 'p', S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol)
vega  = ncalls*100*GBSGreeks(Selection = "vega", TypeFlag = "c", S = SP500, X = strike, Time = tau, r = rate, 
                          b = rate - q, sigma = vol) +
         nputs*100*GBSGreeks(Selection = "vega", TypeFlag = 'p', S = SP500, X = strike, Time = tau, r = rate, 
                   b = rate - q, sigma = vol)

#Question 1(b)
DNVaR.1b = 1.645 * 0.013 *SP500 * delta

#Question 1(c)
variance =  (0.013 *SP500 * delta)^2 + (0.04 *vol * vega)^2 -2*0.8*0.013*0.04*SP500*vol
stdev = sqrt(variance)
DNVaR.1c = 1.645 * stdev


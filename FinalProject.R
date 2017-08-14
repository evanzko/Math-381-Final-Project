###############################
#Code for modeling a Monte Carlo Stock Price Problem

#Variables
#S = Today's Price
#TP = Tomorrow's Price
#meanR = mean of the rate of return
#varR = varience of the rate of return
#sdR = standard Deviation of the rate of return

#Equations used
#TP = S*e^r
#x = Random number between 0 and 1
#r = (u-sig^2/2) + (sdr)* (Z-score(x)
###############################
library(RCurl)

url <- "https://raw.githubusercontent.com/evanzko/Math-381-Final-Project/master/AAPL-2015-2017.csv"

data <- getURL(url) 
#import data from the excel file
MyData <- read.csv(text = data)
#calculate my variables
meanR <- mean(MyData[1:255,'Return'], na.rm = TRUE)
varR <- var(MyData[1:255,'Return'], na.rm = TRUE)
sdR <- sd(MyData[1:255,'Return'], na.rm = TRUE)
drift <- meanR - (varR/2)
ep <- meanR - (sdR^2/2)
lastYr <- MyData[1:255, 'Close']
thisYr <- MyData[256:506,'Close']

#values for the future prices
pred <- numeric(255)

#set the initial value of of future vector as the last value of the closing price 
pred[1] <- lastYr[255]
A = matrix(c(1:255000), nrow = 255, ncol = 1000, byrow = TRUE)

#make a prediction for the next year
for(i in 1:1000){
  for(j in 2:255){
    rand <- runif(1, 0.0, 1.0) #choose a random number between 0-1
    S <- pred[j-1] #get the last closing price
    temp <- exp(drift + sdR*qnorm(rand))
    pred[j] <- S*temp
    A[i,j] <- pred[j]
  }
}

meanPred <- numeric(255)
#get the mean of each row which is the mean of all predictions for that day
for(i in 1:255){
  meanPred[i] <- mean(A[i,])
}

#calculate the difference between the predicted closing price and the actual price.
diff <- numeric(255)
for(i in 1:255){
  diff[i] = thisYr[i] - meanPred[i]
}
x <- c(1:255)
plot(thisYr, type = 'o', col = 'red', xlab = 'Days', ylab = 'closing price', main = 'closing price of Apple Stock')
lines(meanPred, type = 'o', col = 'blue')


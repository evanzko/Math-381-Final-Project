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

url <- "https://raw.githubusercontent.com/evanzko/Math-381-Final-Project/master/AAPL-1year-basic.csv"

data <- getURL(url) 
#import data from the excel file
MyData <- read.csv(text = data)
#calculate my variables
meanR <- mean(MyData[,3], na.rm = TRUE)
varR <- var(MyData[,3], na.rm = TRUE)
sdR <- sd(MyData[,3], na.rm = TRUE)
drift <- meanR - (varR/2)
ep <- meanR - (sdR^2/2)

#values for the future prices
zF <- numeric(30)

#set the initial value of of future vector as the last value of the closing price
zF[1] <- MyData[1,'Close']

for(i in 2:30){
  rand <- runif(1, 0.0, 1.0) #choose a random number between 0-1
  S <- zF[i-1] #get the last closing price
  temp <- exp(drift + sdR*qnorm(rand))
  zF[i] <- S*temp
}
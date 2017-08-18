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
#import data from the excel file
library(RCurl)

url <- "https://raw.githubusercontent.com/evanzko/Math-381-Final-Project/master/AAPL-1year-basic.csv"
#download data
data <- getURL(url) 
#read data from csv file
MyData <- read.csv(text = data)


#calculate statistical variables from the return calculated from 2016-2017
meanR <- mean(MyData[,'Return'], na.rm = TRUE) 
varR <- var(MyData[,'Return'], na.rm = TRUE)
sdR <- sd(MyData[,'Return'], na.rm = TRUE)

#parameters for model
drift <- meanR - (varR/2)
thisYr <- MyData[,'Close'] #real data from 2016-2017 
numPred <- 30 #the number of days the model is predicting
nTrials <- 1000 #the number of trials the model is running

#create a matrix for multiple trials. Each column represents one trial. Each row is a day
A = matrix(numeric(nTrials*numPred), nrow = numPred, ncol = nTrials, byrow = TRUE)
#set the initial prediction value of the matrix as the last value of the closing price
A[1,] = thisYr[length(MyData$Close)]
#for plotting
x <- c(253:282)

plot(MyData$Close,type = "l",xlim = c(200,300), ylim = c(100,200))

#make a prediction for the next year
for(i in 1:nTrials){
  for(j in 2:numPred){
    rand <- runif(1, 0.0, 1.0) #choose a random number between 0-1
    S <- A[j-1,i] #get the last closing price
    shock <- sdR*qnorm(rand) #standard deviation*z-score
    delta <- exp(drift + shock) #calulate the change factor of the stock
    A[j,i] <- S*delta #predict the closing price of the jth day
  }
  lines(x, A[,i], col="blue")
}

#Statistical analysis of predicted price
Price30 <- A[numPred,]
meanP30 <- mean(Price30)
varP30 <- var(Price30)
sdP30 <- sd(Price30)
#95% CI
errorP30 <- qnorm(0.975)*sdP30/sqrt(nTrials)
leftP30 <- meanP30 - error
rightP30 <- meanP30 + error

#Statistical analysis of each predicted day i from 1 to 30
Price <- numeric(30)
meanP <- numeric(30)
varP <- numeric(30)
sdP <- numeric(30)
error <- numeric(30)
left <- numeric(30)
right <- numeric(30)

for (i in 1:30) {
  #for (j in 1:ntrail){
  Price <- A[i,]
  meanP[i] <- mean(Price)
  varP[i] <- var(Price)
  sdP[i] <- sd(Price)
  #95% CI
  error[i] <- qnorm(0.975)*sdP[i]/sqrt(nTrials)
  left[i] <- meanP[i] - error[i]
  right[i] <- meanP[i] + error[i]
}

#Plot each days statistics so we can visualize how this changes
#as our prediction gets further out from last date of data
lines(c(253:282), meanP, col="red")
lines(c(253:282), left, col="green")
lines(c(253:282), right, col="green")

#Plot histogram of closing price distribution for 30th day prediction.
hist(Price30)




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
MyData <- read.csv(file='C:/Users/Evan/Google Drive/Math 381/AAPL-1year.csv', header=TRUE, dec = ".",sep=",",stringsAsFactors=FALSE)

#calculate my variables
meanR <- mean(MyData[,3], na.rm = TRUE)
varR <- var(MyData[,3], na.rm = TRUE)
sdR <- sd(MyData[,3], na.rm = TRUE)
drift <- meanR - (varR/2)
ep <- meanR - (sdR^2/2)

# for(i in 1:252)
#   rand <- runif(1, 0.0, 1.0) #choose a random number between 0-1
#   S <- MyData[i-1, 'close'] #get the last closing price



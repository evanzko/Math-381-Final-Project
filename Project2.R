library(RCurl)

url <- "https://raw.githubusercontent.com/evanzko/Math-381-Final-Project/master/AAPL-1year-basic.csv"

ntrail <- 100

data <- getURL(url) 
#import data from the excel file
MyData <- read.csv(text = data)
#calculate my variables
meanR <- mean(MyData[,3], na.rm = TRUE)
varR <- var(MyData[,3], na.rm = TRUE)
sdR <- sd(MyData[,3], na.rm = TRUE)
drift <- meanR - (varR/2)
ep <- meanR - (sdR^2/2)

fP <- matrix(,ntrail,30)
#values for the future prices
Price30 <- numeric(ntrail)

#set the initial value of of future vector as the last value of the closing price

plot(MyData$Close,type = "l",xlim = c(0,300), ylim = c(100,200))
leng <- length(MyData$Close)

for(j in 1:ntrail){
  zF <- numeric(30)
  zF[1] <- MyData[leng,'Close']
  for(i in 2:30){
    rand <- runif(1, 0.0, 1.0) #choose a random number between 0-1
    S <- zF[i-1] #get the last closing price
    temp <- exp(drift + sdR*qnorm(rand))
    zF[i] <- S*temp
  }
  fP[j,] <- zF
  Price30[j] <- zF[30]
  x <- c(253:282)
  lines(x, zF, col="blue")
}

hist(Price30)



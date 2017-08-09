MyData <- read.csv(file='C:/Users/Evan/Google Drive/Math 381/AAPL-1year.csv', header=TRUE, dec = ".",sep=",",stringsAsFactors=FALSE)
meanR <- mean(MyData[,3], na.rm = TRUE)
varR <- var(MyData[,3], na.rm = TRUE)
sdR <- sd(MyData[,3], na.rm = TRUE)
drift <- meanR - (varR/2)
ep <- meanR - (sdR^2/2) 


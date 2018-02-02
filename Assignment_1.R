length(x)
sum(x)
mean(x)

#1a
a <- seq(1,20)
#1b
b <- seq(20,1,-1)
#1c
c <- c(1:20,19:1)
#1d
tmp <- c(4,6,3)
#1e
rep(tmp, times = 10)
#1f
rep(tmp, length = 31)
#1g
rep(tmp, c(10,20,30))

#2
tmp2 <- seq(3,6,by=0.1)
ans2 <- exp(tmp2)*cos(tmp2)

#3a
ans3 <- (0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3))

#3b
ans3b <- (2^(1:25))/(1:25)

#4a
tmp4 <- seq(10,100)
ans4 <- sum(tmp4^3 + 4*tmp4^2)
#4b
tmp4b <- seq(1,25)
ans4b <- sum((2^tmp4b)/tmp4b + (3^tmp4b)/tmp4b^2) 

#5a
tmp5 <- seq(1,30)
ans5 <- paste("label",tmp5, sep = " ")
#5b
ans5b <- paste("fn",tmp5, sep = "")

#6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
#6a
ans6a <-  yVec[-1] - xVec[-length(xVec)]
#6b
ans6b <- sin(yVec[-length(yVec)]) / cos(xVec[-1])
#6c
xLen <- length(xVec)
ans6c <- xVec[-c(xLen-1,xLen)] + 2*xVec[-c(1,xLen)] - xVec[-c(1,2)]
#6d
ans6d <- sum(exp(-xVec[-1])/(xVec[-xLen]+10))

#7a
ans7a <- (yVec[yVec>600])
#7b
ans7b <- which(yVec>600)
#7c
ans7c <- xVec[yVec>600]
#7d
xMean <- mean(xVec)
ans7d <- sqrt(abs(xVec - xMean))
#7e
yMax <- max(yVec)
ans7e <- sum(yVec > yMax - 200)
#7f
ans7f <- sum(xVec %% 2 == 0)
#7g
ans7g <- xVec[order(yVec)]
#7h
ans7h <- yVec[c(T,F,F)]

#8
ans8 <- 1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))

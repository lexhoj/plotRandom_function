## plotFunction-Module2-Day21
## direction: C:/Users/wangp33/Downloads/STA 308/Lectures/Mar13
## Author: Pei Wang

## Purpose: Explore implementation of functions

## Simple plotting function
##      plotRandom: function to generate a histogram for
##                  a plot of "numpts" random standard normal
##                  deviates - N(0, 1) data
##      input parameter: numpts
##      output produced: histogram

## Notes:to generate normal random numbers, use rnorm() function
##       to make a histogram, use hist() function

## Before making the function, let's figure out what we want
x <- rnorm(25)  
## everyone may have different random numbers, 
##we can use set.seed() to generate the same random numbers
hist(x,breaks = 5)

## Maybe allow the user to determine the number
numpts <- 25
x <- rnorm(numpts)
hist(x)

## Now let's everything to a function
plotRandom <- function(numpts){
  numpts <- 25 # the numpts will be ressigned by value 25 no matter what input is given
  x <- rnorm(numpts)
  hist(x)
}

## comment: parameter can be specified by name or postion
plotRandom(20) #by position
plotRandom(numpts = 30) # by name numpts

plotRandom <- function(numpts){
  x <- rnorm(numpts)
  hist(x)
}
plotRandom(numpts = 30)

## streamlined version
plotRandom <- function(numpts){
  hist(rnorm(numpts))
}

## Define the funtion with Default value 25
plotRandomD <- function(numpts=25){
  hist(rnorm(numpts))
}
plotRandomD()
plotRandomD(50)


## Add second input parameters to function: numbins with default value 10

plotRandom2 <- function(numpts=25,numbins=10){
  hist(rnorm(numpts), breaks = numbins)
}
plotRandom2()
plotRandom2(50)
plotRandom2(numbins = 50)
plotRandom2(numbins = 50, numpts = 1000)

## Add output feature to the function
plotRandom3 <- function(numpts=25,numbins=10){
  ran_x <- rnorm(numpts)
  mean_x <- mean(ran_x)
  hist(ran_x, breaks = numbins)
  abline(v=mean_x, col="navy",lwd = 3)
}
plotRandom3()

## Add two more input parameters to the function, mu(mean of normal), sigma(sd of normal)

plotRandom3 <- function(numpts = 25, numbins = 10){
  ran_x <- rnorm(numpts)
  mean_x <- mean(ran_x)
  mu_x <- mean(ran_x)
  sd_x <- sigma(ran_x)
  hist(ran_x, breaks = numbins)
  abline(v = mean_x, col = "navy", lwd = 3)
}



## 3/15/23 continuation

plotRandom3 <- function(numpts = 25, numbins = 10, mu = 0, sigma = 1){
  ran_x <- rnorm(numpts, mean = mu, sd = sigma)
  mean_x <- mean(ran_x)
  hist(ran_x, breaks = numbins)
  abline(v = mean_x, col = "navy", lwd = 3)
}

plotRandom3()
plotRandom3(numpt = 1000, numbins = 15, mu = 24, sigma = 3)


## Control the output using an input parameter

plotRandom3 <- (function(numpts = 25, numbins = 10, mu = 0, sigma = 1, plot = TRUE){
  ran_x <- rnorm(numpts, mean = mu, sd = sigma)
  mean_x <- mean(ran_x)

  if(plot == TRUE){
  hist(ran_x, breaks = numbins)
  abline(v = mean_x, col = "navy", lwd = 3)
  }
  list(Data = ran_x, Mean = mean_x, SD = sd(ran_x))
})

## Clear R-studio plot window to show this works

dev.off()

out <- plotRandom3(numpt = 1000, numbins = 15, mu = 24, sigma = 3, plot = FALSE)
out <- plotRandom3(numpt = 1000, numbins = 15, mu = 24, sigma = 3, plot = TRUE)
  

  
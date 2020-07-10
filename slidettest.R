slidingttest <- function(dataset,step) {
  
  slidt <- matrix(NA,nrow = (length(x)-step-step-1),ncol = 2)
  k<-1
  x<-dataset[,2]
  for (i in (step+1):(length(x)-step-1)) {
    
    n1 <- step    #n1, n2为滑动补偿，需调整
    n2 <- step
    n11 <- 1 / n1
    n22 <- 1 / n2
    m <- sqrt(n11 + n22)
    
    n1_mean <- mean(x[(i-10):(i-1)])
    n2_mean <- mean(x[(i+1):(i+11)])
    s1 <- var(x[(i-10):i])
    s2 <- var(x[(i+1):(i+11)])
    s <- sqrt((n1 * s1 + n2 * s2) / (n1 + n2 - 2))
    t_value <- (n2_mean - n1_mean) / (s * m)
   
    slidt[k,1] <- dataset[i,1]
    slidt[k,2] <- t_value
   k<-k+1 
  }
  slidt <- as.data.frame(slidt)
 # result = list(t=i,test = t_value)
   names(slidt)[1:2] <- c("T","tvalue")
  return(slidt)
}






source("https://github.com/jsfzihuan/slidingttest/blob/master/slidettest.R")



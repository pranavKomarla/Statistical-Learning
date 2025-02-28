
source("/Users/pranavkomarla/Desktop/Statistical Learning/datday1.pck")


km_estimator <- function(x) {
  
  o1 <- order(x[,1])
  x1 <- x[o1, ]
  
  n <- nrow(x1)
  
  I1 <- x1[,2] == 1
  
  x2 <- x1[I1, ]
  
  n1 <- n
  S <- 1
  Svec <- c(1)
  tvec <- c(0)
  
  
  for (i in x2[,1]) {
    
    if (is.matrix(x1)) {  #WHY WOULD I SAY THIS????
      # You would say this to make sure that x1 is in the right matrix format (multiple rows).
      # It is also important because x1 has rows removed later in the for loop.
      d1 <- sum(x1[,1] <= i)  
    } else {
      d1 <- 1
    }
    
    
    d_event <- sum(x1[,1] == i & x1[,2] == 1)
    
    
    S<-S*(n1-d1)/(n1-d1+1) #What is this assuming?????
    # This is assuming that there is exactly one death at d1
    
    
    #S<-S*(1-1/(n1-d1+1)) #is there a difference between red and blue line??????
    # No, there is mathematically no difference between the red and blue lines. 
    
    Svec <- c(Svec, S)
    tvec <- c(tvec, i)
    
    n1 <- n1 - d1
    
    
    if (length(dim(x1)) == 2) {
      x1 <- x1[-c(1:d1), , drop = FALSE]
    }
    
  }
  
  last_time <- max(x[,1])
  if (last_time > tail(tvec, 1)) {
    tvec <- c(tvec, last_time)
    Svec <- c(Svec, tail(Svec, 1))
  }
  
  return(list(time = tvec, surv = Svec))
}

#2

km_uc1 <- km_estimator(UCdat[[1]])
km_uc2 <- km_estimator(UCdat[[2]])

plot(km_uc1$time, km_uc1$surv, type = "s", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Survival Probability", main = "KM Curves (UCdat)")
lines(km_uc2$time, km_uc2$surv, type = "s", col = "red", lwd = 2)
legend("topright", legend = c("UCdat[[1]]", "UCdat[[2]]"),
       col = c("blue", "red"), lty = 1, lwd = 2)

km_z1 <- km_estimator(z1)
plot(km_z1$time, km_z1$surv, type = "s", col = "green", lwd = 2,
     xlab = "Time", ylab = "Survival Probability", main = "KM Curve (z1)")

#summary(km_uc1$time)
#summary(km_uc2$time)


#quantile(km_uc1$time, probs = seq(0, 1, 0.1))
#quantile(km_uc2$time, probs = seq(0, 1, 0.1))

#3

qq_surv_plot <- function(z1, z2, 
                         mainstr = "Q-Q Plot", 
                         x1lab = "time under condition 1", 
                         y1lab = "time under condition 2") {
  t1 <- z1$time
  t2 <- z2$time
  deg1 <- z1$surv
  deg2 <- z2$surv #What is the purpose of the lines in red, WHY is this done??
  # The purpose of the next 6 lines is: to take both the survival probabilities and sort them
  # And then find the survival probabilities that both the curves share
  deg0 <- sort(c(deg1, deg2))
  
  x1 <- max(min(deg1), min(deg2))
  x2 <- min(max(deg1), max(deg2))
  I1 <- deg0 >= x1
  I2 <- deg0 <= x2
  I3 <- as.logical(I1 * I2)
  deg0 <- deg0[I3]
  
  t1a <- approx(x = deg1, y = t1, xout = deg0)$y #HOW DOES APPROX WORK?
  # approx works by estimating values between two data points
  # in that line, it takes the survival probabilities and associated
  # time points and find estimated times when they reach the survival
  # probabilities in the other dataset
  t2a <- approx(x = deg2, y = t2, xout = deg0)$y
  
  plot(t1a, t2a, xlab = x1lab, ylab = y1lab, main = mainstr)
  lines(t1a, t2a, col = 2, lwd = 2)
}


km_uc1_log <- km_uc1  # Copy all attributes
km_uc1_log$time <- log(km_uc1$time + 1)  # Apply log transformation only to time

km_uc2_log <- km_uc2  # Copy all attributes
km_uc2_log$time <- log(km_uc2$time + 1)  # Apply log transformation only to time

#km_uc1_log <- survfit(Surv(log(km_uc1$time + 1), km_uc1$status) ~ 1, data = km_uc1)
#km_uc2_log <- survfit(Surv(log(km_uc2$time + 1), km_uc2$status) ~ 1, data = km_uc2)



qq_surv_plot(km_uc1_log, km_uc2_log)
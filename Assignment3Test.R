set.seed(123)

my.bootstrapci <-function(vec0,nboot=10000,alpha=0.1)
{
  n0 <- length(vec0)
  mean0 <- mean(vec0)
  sd0 <- sqrt(var(vec0))
  
  # Step 1: Generate bootstrap samples and calculate bootstrap means
  bootvec <- numeric(nboot)
  for (i in 1:nboot) {
    vecb <- sample(vec0, replace = TRUE)
    bootvec[i] <- mean(vecb)
  }
  
  # Bias correction: the difference between the mean of the bootstrap and the observed mean
  bias <- mean(bootvec) - mean0
  
  # Create the 2nd order pivotal interval
  lq <- quantile(bootvec, alpha / 2) - bias
  uq <- quantile(bootvec, 1 - alpha / 2) - bias
  
  # Compute the standard normal-based confidence interval
  NLB <- mean0 - (sd0 / sqrt(n0)) * qt(1 - alpha / 2, n0 - 1)
  NUB <- mean0 + (sd0 / sqrt(n0)) * qt(1 - alpha / 2, n0 - 1)
  
  # Return both intervals
  list(
    bootstrap.confidence.interval = c(lq, uq),
    normal.confidence.interval = c(NLB, NUB)
  )
}

Simfunc<-function(mu.val=3,n=30,nsim=1000,alpha = 0.1)
{
  cvec.boot <- numeric(nsim)
  cvec.norm <- numeric(nsim)
  
  # The true value for the lognormal mean (assuming mu.val is the log-scale mean)
  mulnorm <- exp(mu.val + 1/2)  # This is the true mean of the lognormal distribution
  
  
  for(i in 1:nsim){
    if((i/10)==floor(i/10)){
      print(i)
    }
    # Generate a sample from the lognormal distribution
    vec.sample <- rlnorm(n, meanlog = mu.val, sdlog = 1)  # Specify sdlog for lognormal
    
    # Apply the bootstrap CI function
    boot.list <- my.bootstrapci(vec.sample, alpha = alpha)
    
    # Extract the bootstrap and normal confidence intervals
    boot.conf <- boot.list$bootstrap.confidence.interval
    norm.conf <- boot.list$normal.confidence.interval
    
    # Check coverage for the bootstrap confidence interval
    cvec.boot[i] <- (boot.conf[1] < mulnorm) * (boot.conf[2] > mulnorm)
    
    # Check coverage for the normal confidence interval
    cvec.norm[i] <- (norm.conf[1] < mulnorm) * (norm.conf[2] > mulnorm)
  }
  # Return coverage rates for both confidence intervals
  list(
    boot.coverage = sum(cvec.boot) / nsim,
    norm.coverage = sum(cvec.norm) / nsim
  )
}

run_simulations <- function(mu.val = 3, nsim = 1000, n_values = c(3, 10, 30, 100), alpha_values = c(0.1, 0.05)) {
  results <- data.frame(n = integer(), alpha = numeric(), boot.coverage = numeric(), norm.coverage = numeric())
  
  for (n in n_values) {
    for (alpha in alpha_values) {
      print(paste("Running simulation for n =", n, "and alpha =", alpha))
      
      # Run the simulation
      sim_result <- Simfunc(mu.val = mu.val, n = n, nsim = nsim, alpha = alpha)
      
      # Store the results in the data frame
      results <- rbind(results, data.frame(n = n, alpha = alpha, 
                                           boot.coverage = sim_result$boot.coverage, 
                                           norm.coverage = sim_result$norm.coverage))
    }
  }
  
  return(results)
}

# Define the n and alpha values
n_values <- c(3, 10, 30, 100)
alpha_values <- c(0.1, 0.05)

# Run the simulations
simulation_results <- run_simulations(mu.val = 3, nsim = 1000, n_values = n_values, alpha_values = alpha_values)

# Print the results in a table format
print(simulation_results)

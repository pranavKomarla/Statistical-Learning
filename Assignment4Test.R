library(ISLR)

data(Auto, package = "ISLR")

Auto.mat <- as.matrix(Auto[,-9])

# Subset the data for each country based on the 'origin' column
Auto.mat.japan <- Auto.mat[Auto.mat[, 8] == 3, ]   # Japan: origin == 3
Auto.mat.germany <- Auto.mat[Auto.mat[, 8] == 2, ]  # Germany: origin == 2
Auto.mat.usa <- Auto.mat[Auto.mat[, 8] == 1, ]      # USA: origin == 1

matrix.2ndorder.make<-function(x, only.quad=F){
  x0<-x
  dimn<-dimnames(x)[[2]] #extract the names of the variables
  num.col<-length(x[1,]) # how many columns
  for(i in 1:num.col){
    # if we are doing all 2nd order
    if(!only.quad){
      for(j in i:num.col){
        x0<-cbind(x0,x[,i]*x[,j])
        dimn<-c(dimn,paste(dimn[i],dimn[j],sep=""))
        #create interaction dimnames
      }
    }
    else{
      #in here only if doing only squared terms
      x0<-cbind(x0,x[,i]*x[,i])
      dimn<-c(dimn,paste(dimn[i],"2",sep="")) # squared
      
    }
  }
  dimnames(x0)[[2]]<-dimn
  return(x0)
}

# Function to calculate PRESS (Prediction Error Sum of Squares)
PRESS <- function(x, y) {
  lm_fit <- lsfit(x, y)
  resid <- lm_fit$resid
  hat_values <- hat(x)  # Compute hat values
  
  stud_resid <- resid / (1 - hat_values)  # Compute studentized residuals
  press <- sum(stud_resid^2)  # Sum of squared studentized residuals
  
  return(press)
}

Auto.mat2nd <- matrix.2ndorder.make(Auto.mat[,-1])  # Excluding the first column
Auto.mat.usa2nd <- matrix.2ndorder.make(Auto.mat.usa[,-1])
Auto.mat.germany2nd <- matrix.2ndorder.make(Auto.mat.germany[,-1])
Auto.mat.japan2nd <- matrix.2ndorder.make(Auto.mat.japan[,-1])

# Leaps selection with PRESS calculation
leaps_model_selection <- function(X, y) {
  leaps_result <- regsubsets(y ~ ., data = as.data.frame(cbind(y, X)), nvmax = 10)
  leaps_summary <- summary(leaps_result)
  
  
  # Ensure leaps_summary$which is valid
  if (is.null(leaps_summary$which)) {
    stop("Error: regsubsets() did not return a valid model.")
  }
  
  #print(leaps_summary$which)  # Debugging
  
  press_values <- sapply(1:nrow(leaps_summary$which), function(i) {
    if (i <= nrow(leaps_summary$which)) {
      selected_vars <- leaps_summary$which[i, ]
      X_selected <- X[, selected_vars[-1], drop = FALSE]
      return(PRESS(X_selected, y))
    } else {
      return(NA)
    }
  })
  
  best_model_index <- which.min(press_values)
  best_model_vars <- leaps_summary$which[best_model_index, ]
  return(list(best_model_index = best_model_index, best_model_vars = best_model_vars))
}

# Function to perform LARS selection
lars_model_selection <- function(X, y) {
  lars_model <- lars(X, y, type = "lasso")
  best_lars_index <- which.min(lars_model$Cp)
  best_lars_coef <- coef(lars_model, s = best_lars_index)
  return(best_lars_coef)
}

# Train-Test Split
train_test_split <- function(X, y, train_ratio = 0.7) {
  set.seed(123)
  train_indices <- sample(1:nrow(X), size = floor(train_ratio * nrow(X)), replace = FALSE)
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]
  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}

# Function to run model selection, train & evaluate
run_analysis <- function(X, y, country_name) {
  # Split into train and test sets
  split <- train_test_split(X, y)
  X_train <- split$X_train
  y_train <- split$y_train
  X_test <- split$X_test
  y_test <- split$y_test
  
  # Run model selection
  leaps_result <- leaps_model_selection(X_train, y_train)
  
  lars_result <- lars_model_selection(X_train, y_train)
  
  # Predictions
  X_test_leaps <- X_test[, leaps_result$best_model_vars[-1], drop = FALSE]
  pred_leaps <- predict(lm(y_train ~ ., data = as.data.frame(cbind(y_train, X_train[, leaps_result$best_model_vars[-1], drop=FALSE]))), newdata = as.data.frame(X_test_leaps))
  
  pred_lars <- X_test %*% as.vector(lars_result)  # LARS prediction
  print(pred_lars)
  mean_adjusted_pred_lars <- mean(y_train) - mean(pred_lars) + pred_lars
  
  # Plot comparisons
  par(mfrow=c(1,2))
  par(oma = c(0, 0, 3, 0)) 
  par(mar = c(5, 4, 2, 2)) 
  
  plot(y_test, pred_leaps, main=paste("Leaps Model -", country_name), xlab="Actual MPG", ylab="Predicted MPG")
  abline(0, 1, col="red")
  
  
  plot(y_test, mean_adjusted_pred_lars, main=paste("LARS Model -", country_name), xlab="Actual MPG", ylab="Predicted MPG")
  abline(0, 1, col="blue")
  
  # Correlation title
  title(main = paste("Corr (Leaps):", round(cor(y_test, pred_leaps), 2),
                     "Corr (LARS):", round(cor(y_test, pred_lars), 2)), outer=TRUE)
  
  return(list(leaps_result = leaps_result, lars_result = lars_result)) 
  
}

usa_results <- run_analysis(Auto.mat.usa2nd, Auto.mat.usa[,1], "USA")
germany_results <- run_analysis(Auto.mat.germany2nd, Auto.mat.germany[,1], "Germany")
japan_results <- run_analysis(Auto.mat.japan2nd, Auto.mat.japan[,1], "Japan")

# Check feature selection for interpretation
usa_nonzero <- which(usa_results$lars_result != 0)
germany_nonzero <- which(germany_results$lars_result != 0)
japan_nonzero <- which(japan_results$lars_result != 0)

print("Non-zero coefficients in LARS for USA:")
print(usa_nonzero)

print("Non-zero coefficients in LARS for Germany:")
print(germany_nonzero)

print("Non-zero coefficients in LARS for Japan:")
print(japan_nonzero)



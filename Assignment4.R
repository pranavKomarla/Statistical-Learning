library(ISLR)
library(leaps)
library(lars)
library(ggplot2)
library(dplyr)
library(caret)




# Load and preprocess data
data(Auto, package = "ISLR")
Auto <- Auto %>% select(-name, -origin)  # Remove categorical variables

# Function to fit models and evaluate
analyze_country <- function(country_data, country_name) {
  
  # Create second-order features
  poly_data <- as.data.frame(model.matrix(mpg ~ poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(displacement, 2), data=country_data))
  poly_data$mpg <- country_data$mpg
  
  # Train-test split
  set.seed(42)
  train_idx <- createDataPartition(poly_data$mpg, p=0.7, list=FALSE)
  train_data <- poly_data[train_idx,]
  test_data <- poly_data[-train_idx,]
  
  # Best subset selection using leaps
  regfit.full <- regsubsets(mpg ~ ., data=train_data, nvmax=10, method="exhaustive")
  reg_summary <- summary(regfit.full)
  best_5_models <- order(reg_summary$cp)[1:5]
  
  # Compute PRESS statistic to choose best model
  press_values <- sapply(best_5_models, function(i) {
    model <- lm(as.formula(paste("mpg ~", paste(names(coef(regfit.full, i))[-1], collapse=" + "))), data=train_data)
    sum((residuals(model) / (1 - hatvalues(model)))^2)
  })
  best_leaps_model <- best_5_models[which.min(press_values)]
  
  # Fit LARS model
  x_train <- as.matrix(train_data[,-ncol(train_data)])
  y_train <- train_data$mpg
  lars_model <- lars(x_train, y_train, type="lasso")
  best_lars_step <- which.min(lars_model$Cp)
  best_lars_coefs <- coef(lars_model, s=best_lars_step, mode="step")
  
  # Predict on test data
  best_leaps_formula <- as.formula(paste("mpg ~", paste(names(coef(regfit.full, best_leaps_model))[-1], collapse=" + ")))
  best_leaps_fit <- lm(best_leaps_formula, data=train_data)
  leaps_pred <- predict(best_leaps_fit, newdata=test_data)
  
  lars_pred <- predict(lars_model, as.matrix(test_data[,-ncol(test_data)]), s=best_lars_step, mode="step")$fit
  
  # Plot results
  plot1 <- ggplot(data.frame(True=test_data$mpg, Pred=leaps_pred), aes(x=True, y=Pred)) +
    geom_point() + geom_smooth(method='lm') +
    ggtitle(paste("Leaps Model: Correlation =", round(cor(leaps_pred, test_data$mpg), 2)))
  
  plot2 <- ggplot(data.frame(True=test_data$mpg, Pred=lars_pred), aes(x=True, y=Pred)) +
    geom_point() + geom_smooth(method='lm') +
    ggtitle(paste("LARS Model: Correlation =", round(cor(lars_pred, test_data$mpg), 2)))
  
  print(plot1)
  print(plot2)
  
  # Examine coefficient differences
  list(best_leaps_coefs=coef(best_leaps_fit), best_lars_coefs=best_lars_coefs)
}

# Analyze for each country
countries <- split(Auto, Auto$origin)
results <- lapply(names(countries), function(name) analyze_country(countries[[name]], name))

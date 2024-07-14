data<-read.csv("C:/Users/thara/Downloads/Calories.csv")
summary(data)
colnames(data)
library(dplyr)
install.packages("caret")
library(caret)
library(dplyr)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$Calories, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Overall model with all predictors
full_model <- lm(Calories ~ ., data = trainData)
summary(full_model)

# New model with specific predictors
new_model <- lm(Calories ~ Total.Fat..g. + Saturated.Fat..g. + Carbs..g. + Protein..g., data = trainData)
summary(new_model)

# R² for the full model
train_r2_full <- summary(full_model)$r.squared
test_r2_full <- summary(lm(Calories ~ ., data = testData))$r.squared

# R² for the new model
train_r2_new <- summary(new_model)$r.squared
test_r2_new <- summary(lm(Calories ~ Total.Fat..g. + Saturated.Fat..g. + Carbs..g. + Protein..g., data = testData))$r.squared

# Print R² values
cat("Full Model Train R²:", train_r2_full, "\n")
cat("Full Model Test R²:", test_r2_full, "\n")
cat("New Model Train R²:", train_r2_new, "\n")
cat("New Model Test R²:", test_r2_new, "\n")


full_modellll <- lm(Calories ~ Serving.Size..g.+Total.Fat..g.+Saturated.Fat..g.+Trans.Fat..g.+Sodium..mg.+Carbs..g.+Sugars..g.+Protein..g., data = trainData)
summary(full_modellll)

library(car)
vif_values <- vif (full_model)
print(vif_values)


#stepwise regression

# Predict using the stepwise model
stepwise_predictions <- predict(stepwise_model, newdata = testing)

# Fit individual models
protein_model <- lm(Calories ~ Protein..g., data = training)
print(summary(protein_model))

fat_model <- lm(Calories ~ Total.Fat..g., data = training)
print(summary(fat_model))

transfat_model <- lm(Calories ~ Trans.Fat..g., data = training)
print(summary(transfat_model))

carb_model <- lm(Calories ~ Carbs..g., data = training)
print(summary(carb_model))

# Fit combined models
combined_model1 <- lm(Calories ~ Protein..g. + Total.Fat..g., data = training)
print(summary(combined_model1))

combined_model2 <- lm(Calories ~ Protein..g. + Trans.Fat..g., data = training)
print(summary(combined_model2))

combined_model3 <- lm(Calories ~ Protein..g. + Carbs..g., data = training)
print(summary(combined_model3))

combined_model4 <- lm(Calories ~ Trans.Fat..g. + Total.Fat..g., data = training)
print(summary(combined_model4))

combined_model5 <- lm(Calories ~ Trans.Fat..g. + Carbs..g., data = training)
print(summary(combined_model5))

combined_model6 <- lm(Calories ~ Carbs..g. + Total.Fat..g., data = training)
print(summary(combined_model6))

  


#ridge


# Load necessary library
library(glmnet)

# Ensure the data is consistent
data <- na.omit(data)  # Remove any rows with missing values

# Prepare predictors and response variable
x <- model.matrix(Calories ~ . - 1, data = data)  # Exclude intercept
y <- data$Calories

# Print dimensions of x and y before splitting
cat("Dimensions of x:", dim(x), "\n")
cat("Length of y:", length(y), "\n")

# Ensure x and y have the same number of observations
if (nrow(x) != length(y)) {
  stop("Number of observations in y does not match the number of rows in x")
}

# Split the data
set.seed(123)  # For reproducibility
part <- sample(2, nrow(x), replace = TRUE, prob = c(0.7, 0.3))
x_train <- x[part == 1, ]  # Training set
y_train <- y[part == 1]    # Response for training set
x_test <- x[part == 2, ]   # Test set
y_test <- y[part == 2]     # Response for test set

# Print dimensions of training and test sets
cat("Dimensions of x_train:", dim(x_train), "\n")
cat("Length of y_train:", length(y_train), "\n")
cat("Dimensions of x_test:", dim(x_test), "\n")
cat("Length of y_test:", length(y_test), "\n")

# Ensure the training and test sets have the correct dimensions
if (nrow(x_train) != length(y_train) || nrow(x_test) != length(y_test)) {
  stop("Mismatch between x and y in training or test sets")
}

# Fit Ridge regression with cross-validation
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)

# Get the best lambda
best_lambda <- ridge_model$lambda.min

# Predictions
train_predictions <- predict(ridge_model, s = best_lambda, newx = x_train)
test_predictions <- predict(ridge_model, s = best_lambda, newx = x_test)

# Calculate R² for training
train_r2_ridge <- 1 - sum((y_train - train_predictions)^2) / sum((y_train - mean(y_train))^2)

# Calculate R² for testing
test_r2_ridge <- 1 - sum((y_test - test_predictions)^2) / sum((y_test - mean(y_test))^2)

# Print R² values
cat("Ridge Model Train R²:", train_r2_ridge, "\n")
cat("Ridge Model Test R²:", test_r2_ridge, "\n")

# Get the predicted values as a vector
train_predictions_vector <- as.vector(train_predictions)
test_predictions_vector <- as.vector(test_predictions)

# Calculate Mean Squared Error for training
train_mse_ridge <- mean((y_train - train_predictions_vector)^2)

# Calculate Mean Squared Error for testing
test_mse_ridge <- mean((y_test - test_predictions_vector)^2)

# Print MSE values
cat("Ridge Model Train MSE:", train_mse_ridge, "\n")
cat("Ridge Model Test MSE:", test_mse_ridge, "\n")


#LASSO:

# Load necessary library
library(glmnet)

# Ensure the data is consistent
data <- na.omit(data)  # Remove any rows with missing values

# Prepare predictors and response variable
x <- model.matrix(Calories ~ . - 1, data = data)  # Exclude intercept
y <- data$Calories

# Split the data
set.seed(123)  # For reproducibility
part <- sample(1:2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
x_train <- x[part == 1, ]  # Training set
y_train <- y[part == 1]    # Response for training set
x_test <- x[part == 2, ]   # Test set
y_test <- y[part == 2]     # Response for test set

# Fit Lasso regression with cross-validation
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)

# Get the best lambda
best_lambda <- lasso_model$lambda.min

# Predictions
train_predictions <- predict(lasso_model, s = best_lambda, newx = x_train)
test_predictions <- predict(lasso_model, s = best_lambda, newx = x_test)

# Calculate R² for training
train_r2_lasso <- 1 - sum((y_train - train_predictions)^2) / sum((y_train - mean(y_train))^2)

# Calculate R² for testing
test_r2_lasso <- 1 - sum((y_test - test_predictions)^2) / sum((y_test - mean(y_test))^2)

# Print R² values
cat("Lasso Model Train R²:", train_r2_lasso, "\n")
cat("Lasso Model Test R²:", test_r2_lasso, "\n")

# Get the predicted values as a vector
train_predictions_vector <- as.vector(train_predictions)
test_predictions_vector <- as.vector(test_predictions)

# Calculate Mean Squared Error for training
train_mse_lasso <- mean((y_train - train_predictions_vector)^2)

# Calculate Mean Squared Error for testing
test_mse_lasso <- mean((y_test - test_predictions_vector)^2)

# Print MSE values
cat("Lasso Model Train MSE:", train_mse_lasso, "\n")
cat("Lasso Model Test MSE:", test_mse_lasso, "\n")







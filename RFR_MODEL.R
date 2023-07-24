#----Load Packages----
library(dplyr)
library(stats)
library(tidyverse)
library(sf)
library(rgdal)
library(sp)
library(raster)
library(randomForest)
library(caret)

#----Train Test Split with k-fold Cross-validation----
#Set Seed
set.seed(123)

#Split the dataset into features and target variable
features <- rasvalue[, -ncol(rasvalue)]  # Exclude the last column as target variable
target <- rasvalue[, ncol(rasvalue)]     # Last column as target variable

#----Set up k-fold cross-validation----
k <- 10  # Number of folds for cross-validation
folds <- createFolds(target, k = k, returnTrain = TRUE)

# Initialize a list to store the results from each fold
accuracy_results <- list()

# Loop through each fold and perform model training and evaluation
for (fold in 1:k) {
  # Split data into training and test sets for this fold
  train_indices <- folds[[fold]]
  train_features <- features[train_indices, ]
  train_target <- target[train_indices]
  test_indices <- setdiff(1:nrow(features), train_indices)
  test_features <- features[test_indices, ]
  test_target <- target[test_indices]
  
  #----Build train control variable for this fold
  control <- trainControl(method="repeatedcv", 
                          repeats=10,
                          number=10,
                          search="grid",
                          allowParallel = FALSE)
  
  #----Define the parameter grid for hyperparameter tuning----
  grid <- expand.grid(.mtry=c(1,3,5,7,10),
                      .ntree=c(50,100,400,750,1000), 
                      .nodesize=c(2,5,7,9,11))
  
  #----Perform hyperparameter tuning using k-fold cross-validation on the training set----
  rf_model <- train(x = train_features,
                    y = train_target, 
                    method=customRF, 
                    metric="Rsquared", 
                    tuneGrid=grid, 
                    trControl=control)
  
  #----Fit the final model using the best hyperparameters on the full training set----
  final_model <- randomForest(
    x = train_features,
    y = train_target,
    mtry = rf_model$bestTune$mtry,
    ntree = rf_model$bestTune$ntree,        # Set desired number of trees
    nodesize = rf_model$bestTune$nodesize   # Set desired minimum size of terminal nodes
  )
  
  #----Predict on the test data using the final model----
  predictions <- predict(final_model, test_features)
  
  # Evaluate the model performance for this fold and store the results
  rmse <- sqrt(mean((predictions - test_target)^2))
  n <- length(test_target)
  p <- length(final_model$forest$forest$ntree)
  r2 <- 1 - sum((test_target - predictions)^2) / sum((test_target - mean(test_target))^2)
  adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  
  accuracy_results[[fold]] <- c(RMSE = rmse, R2 = r2, `Adjusted R-squared` = adj_r2)
}

# Combine the results from all folds
accuracy_results <- do.call(rbind, accuracy_results)

# Calculate the mean and standard deviation of evaluation metrics over all folds
mean_RMSE <- mean(accuracy_results[, "RMSE"])
mean_R2 <- mean(accuracy_results[, "R2"])
mean_adj_R2 <- mean(accuracy_results[, "Adjusted R-squared"])

std_RMSE <- sd(accuracy_results[, "RMSE"])
std_R2 <- sd(accuracy_results[, "R2"])
std_adj_R2 <- sd(accuracy_results[, "Adjusted R-squared"])

# Print the mean and standard deviation of evaluation metrics
cat("Mean RMSE:", mean_RMSE, "\n")
cat("Mean R-squared:", mean_R2, "\n")
cat("Mean Adjusted R-squared:", mean_adj_R2, "\n")
cat("Standard Deviation of RMSE:", std_RMSE, "\n")
cat("Standard Deviation of R-squared:", std_R2, "\n")
cat("Standard Deviation of Adjusted R-squared:", std_adj_R2, "\n")

#----Map The Model----
map.RF.r1 <- predict(raster.stack, final_model, datatype = "FLT4S", overwrite = TRUE)
plot(map.RF.r1, main = "RFR Model Mean 0-30cm Soil Sand")

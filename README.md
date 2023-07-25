# Hyperspectral Soil Texture Data Mapping with Random Forest Regression

This repository contains R code for mapping soil texture data using hyperspectral Hyperion EO-1 imagery and random forest regression. The code leverages the power of random forest, a versatile machine learning algorithm, to predict soil texture based on hyperspectral data captured by the Hyperion EO-1 satellite sensor.

# Dataset
The dataset used in this project consists of hyperspectral imagery data obtained from the Hyperion EO-1 satellite sensor. The dataset contains a set of spectral bands (features) and corresponding soil texture labels (target variable). These labels represent different soil texture classes such as sand, silt, and clay percentages.

# Methodology
Data Preprocessing: The hyperspectral data is processed and prepared for model training. Features and target variables are extracted from the dataset.

k-Fold Cross-Validation: To ensure robust model evaluation and mitigate overfitting, k-fold cross-validation is employed. The dataset is split into k folds, and the model is trained and evaluated k times using different train-test splits.

Random Forest Regression: Random forest is applied to model the relationship between the hyperspectral features and soil texture labels. Hyperparameter tuning is performed using the caret package to find the best combination of hyperparameters.

Model Evaluation: Evaluation metrics, including Root Mean Squared Error (RMSE), R-squared, and Adjusted R-squared, are calculated to assess the performance of the final model.

![Clay](https://github.com/ilmirmdh/Digital_Soil_Mapping_Texture_RF/assets/82806657/66aba7e2-2353-43cc-9408-2ec77c50ca01)

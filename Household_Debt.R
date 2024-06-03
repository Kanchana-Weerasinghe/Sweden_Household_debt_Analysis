library(corrplot)
library(ggplot2)
library(reshape2)
library(glmnet)

current_dir <- getwd()
file_path <- file.path(current_dir, "SwedishHouseholdDebt.csv")

# Read the dataset
data <- read.csv(file_path)


### Data exploratory analysis ############################################################
print("First few rows of the dataset:")
print(head(data))

# Variable information
print("Variable information:")
print(str(data))


print("Summary statistics:")
print(summary(data))
print("")

################ General summary ########################################################

# # Column names
all_column_names <- names(data)

types <- sapply(data, class)

# Count
counts <- sapply(data, length)

# Number of unique values
unique_counts <- sapply(data, function(x) length(unique(x)))

# Missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))

# Create a data frame to combine all information
general_summary_table <- data.frame(
  column=all_column_names,
  Data_Type = types,
  Row_Count = counts,
  Unique_counts = unique_counts,
  Missing_values = missing_values
)

# Print the summary table
print("General Summary Statistics:")
print(general_summary_table)


#################### Numerical features understanding ####################
numerical_features <- sapply(data, is.numeric)
numerical_data <- data[, numerical_features]
# Column names
column_names <- names(numerical_data)

# Minimum
min_values <- sapply(numerical_data, min, na.rm = TRUE)

# Max
max_values <- sapply(numerical_data, max, na.rm = TRUE)

# Mean
mean_values <- colMeans(numerical_data, na.rm = TRUE)

# STD
std_values <- apply(numerical_data, 2, sd, na.rm = TRUE)

# Coefficient of Variation
cv_values <- apply(numerical_data, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))

num_types <- sapply(numerical_data, class)

# Count
num_counts <- sapply(numerical_data, length)

# Number of unique values
num_unique_counts <- sapply(numerical_data, function(x) length(unique(x)))

# Missing values
num_missing_values <- sapply(numerical_data, function(x) sum(is.na(x)))


# Create a data frame to combine all information
summary_table <- data.frame(
  Column=column_names,
  Minimum = sprintf("%.2f", min_values),
  Maximum = sprintf("%.2f", max_values),
  Mean = sprintf("%.2f", mean_values),
  STD = sprintf("%.2f", std_values),
  CV = sprintf("%.2f", cv_values),
  Data_Types=num_types,
  Uniq_Counts=num_unique_counts,
  Missing_Conut=num_missing_values
)

# Print the summary table
print("Summary Statistics:")
print(summary_table)

# Normality and spread
# Distribution (Histogram)
par(mfrow = c(3, 3))
for (col in colnames(numerical_data)) {
  hist(numerical_data[[col]], main = paste("Histogram of", col), xlab = col)
}
par(mfrow = c(1, 1))

#
# ################ Correlations Analysis ########################################################

plot(data)

# Compute the correlation matrix
correlation_matrix <- cor(data, use = "complete.obs")

# Visualize the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", type = "upper",
         order = "hclust", addrect = 2)

correlation_matrix
#
# #################################Analyse inflation (CPI)  vs Household Dept #############################

# Plot CPI vs. Household Debt
# Create the scatter plot with a trend line
ggplot(data, aes(x = cpi, y = debthhr)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear trend line
  labs(x = "Consumer Price Index (CPI)",
       y = "Household Debt (% of GDP)",
       title = "Relationship between CPI and Household Debt") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white"))



# Calculate correlation between CPI and household debt
correlation <- cor(data$cpi, data$debthhr)
print(paste("Correlation between CPI and Household Debt:", correlation))

# Calculate correlation between CPI and grwoth
correlation <- cor(data$cpi, data$debthhr)
print(paste("Correlation between CPI and Household Debt:", correlation))



ggplot(data, aes(x = year)) +
  geom_line(aes(y = cpi, color = "CPI"), size = 0.7) +
  geom_line(aes(y = debthhr, color = "Household Debt"), size = 0.7) +
  labs(x = "Year", y = "Value", color = "Variable") +
  scale_color_manual(values = c("CPI" = "#1974D2", "Household Debt" = "#ff4000")) +
  ggtitle("Trend of CPI and Household Debt Over Time") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white"))+

  scale_x_continuous(limits = c(1980, 2008))


head(data)
#
# ################### Feature Scaling ##############################
#
variables_to_scale <- c("rgd", "pop", "school", "cpi", "debthhr", "growth")

scaled_data <- scale(data[, variables_to_scale])

# Add the scaled variables to the original dataset
data <- cbind(data, scaled_data)

# Rename the columns of the scaled variables
colnames(data)[(ncol(data) - length(variables_to_scale) + 1):ncol(data)] <- paste(variables_to_scale, "scaled", sep = "_")

# Print the dataset with scaled variables
print(data)

ggplot(data, aes(x = year)) +
  geom_line(aes(y = cpi_scaled, color = "cpi_scaled"), size = 0.7) +
  geom_line(aes(y = growth_scaled, color = "growth_scaled"), size = 0.7) +
  geom_line(aes(y = rgd_scaled, color = "cpi_scaled"), size = 0.7) +
  geom_line(aes(y = debthhr_scaled, color = "debthhr_scaled"), size = 0.7) +
  geom_line(aes(y = pop_scaled, color = "cpi_scaled"), size = 0.7) +
  labs(x = "Year", y = "Value", color = "Variable") +
  scale_color_manual(values = c("cpi_scaled" = "#1974D2","growth_scaled"="green" ,"debthhr_scaled" = "#ff4000")) +
  ggtitle("Trend of CPI and Household Debt Over Time") +
  theme_minimal()


#################################Household Dept Prediction #############################

library(caret)
library(Metrics)

# #################################### Check the Household Debt behavior over the time ########

ggplot(data, aes(x = year)) +
  geom_line(aes(y = debthhr, color = "Household Debt"), size = 0.7) +
  labs(x = "Year", y = "Value", color = "Variable") +
  scale_color_manual(values = c("CPI" = "#1974D2", "Household Debt" = "#ff4000")) +
  ggtitle("Trend of CPI and Household Debt Over Time") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white"))+

  scale_x_continuous(limits = c(1980, 2008))

ggplot(data, aes(x = year)) +
  geom_point(aes(y = debthhr, color = "Household Debt"), size = 2) +
  labs(x = "Year", y = "Value", color = "Variable") +
  scale_color_manual(values = c("Household Debt" = "#ff4000")) +
  ggtitle("Trend of Household Debt Over Time") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(limits = c(1980, 2008))

# 
# ################ Calculate the Gini Index to see the most significant Features #########


# Define the training control
train_control <- trainControl(method = "cv", number = 10)

# Train a Random Forest model
rf_model <- train(debthhr ~ ., data = data, method = "rf", trControl = train_control)

# Get variable importance
var_imp <- varImp(rf_model)
var_imp
# Plot variable importance
plot(var_imp)

# 
# ######################### Apply Linear Regression #####################################################


# Define the number of folds for cross-validation
num_folds <- 5

# Define the training control
train_control <- trainControl(method = "cv", number = num_folds)

# Define the model formula
formula <- debthhr ~ rgd + growth+cpi

# Create an empty dataframe to store model performance
model_performance <- data.frame(Model = character(),
                                RMSE = numeric(),
                                R_squared = numeric(),
                                p_valu= numeric(),
                                stringsAsFactors = FALSE)

# Linear Regression (lm)
initial_lr_model  <- train(formula, data = data, method = "lm", trControl = train_control)

recent_data <- data.frame(rgd = 25716, growth = -1.6914871253498,pop=-8668.112,school=9.51,cpi=-2.28164777520989,dep=-56.3032537942203,crisis=1)

predictions <- predict(initial_lr_model, newdata = recent_data)

# View the predictions
print(predictions)
summary(initial_lr_model)

############################ Model evaluation  ######################

initial_model_summary <- summary(initial_lr_model)
initial_model_summary
# Extract relevant metrics from the summary

rmse <- initial_model_summary$sigma
rsquared <- initial_model_summary$r.squared
mae <- mean(abs(residuals(initial_lr_model)))

# Create a data frame with the metrics
summary_df <- data.frame(
  Model = "Linear Regression",
  Metric = c("RMSE", "R-squared", "MAE"),
  Value = c( rmse, rsquared, mae,p_value)
)
summary_df

### Multicollinearity testing ######################
library(car)
# Calculate VIF for each predictor variable
vif_values <- car::vif(lm_model$finalModel)

# Print VIF values
print(vif_values)

#################### Backward Selection #####################
# Backward selection loop

selected_features <- c("rgd", "growth", "pop", "school", "cpi", "dep", "crisis")


model_performance <- data.frame(Model = character(),
                                RMSE = numeric(),
                                R_squared = numeric(),
                                stringsAsFactors = FALSE)




for (i in seq_along(selected_features)[-1]) {
  # Fit the model without one feature
 
  updated_formula <- reformulate(selected_features[-i], response = "debthhr")
  # Fit the model using updated formula
  updated_model <- train(updated_formula, data = data, method = "lm", trControl = train_control)
  # Calculate performance metrics for the updated model
  updated_predictions <- predict(updated_model, data)
  updated_rmse <- RMSE(updated_predictions, data$debthhr)
  updated_r_squared <- R2(updated_predictions, data$debthhr)
  
  
  # Select the feature with the highest p-value for removal
  p_values <- summary(updated_model)$coef[, "Pr(>|t|)"]
  feature_to_remove <- names(p_values)[which.max(p_values)]
  
  # Store the performance metrics and selected features
  model_performance <- rbind(model_performance, data.frame(
    Model = paste("Without", selected_features[-i]),
    RMSE = updated_rmse,
    R_squared = updated_r_squared
  ))
  
  # Remove the selected feature from the list of selected features
  selected_features <- selected_features[selected_features != feature_to_remove]
}

# Print the final model performance after backward selection
print(model_performance)


###############################################################################
# Define the tuning grid for Lasso model

############## Apply best Alpha and Lambda 
num_folds <- 10

# Define the training control for cross-validation
train_control <- trainControl(method = "cv", number = num_folds)

# Define the model formula
formula <- debthhr ~ rgd + growth + pop + school + cpi + dep + crisis

# Define the tuning grid for the Lasso model
lasso_tune_grid <- expand.grid(alpha = 1, lambda = 1)

# Train the Lasso model with K-fold cross-validation
lasso_model <- train(formula, 
                     data = data, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneGrid = lasso_tune_grid)



recent_data <- data.frame(rgd = 35225, growth = -5.40940654815029,pop=-9045,school=11.41,cpi=-0.278428708107441,dep=-52.8205953482255,crisis=0)

predictions <- predict(lasso_model, newdata = recent_data)

print(predictions)




# ###############  bootstrap
num_resamples <- 10000

# Define the training control for bootstrap resampling
train_control <- trainControl(method = "boot", number = num_resamples)

# Define the model formula
formula <- debthhr ~ rgd + growth + pop + school + cpi + dep + crisis

# Define the tuning grid for the Lasso model
lasso_tune_grid <- expand.grid(alpha = 1, lambda = 1)

# Train the Lasso model with bootstrap resampling
lasso_model <- train(formula, 
                     data = data, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneGrid = lasso_tune_grid)

# recent_data <- data.frame(rgd = 30647, growth = -5.40940654815029,pop=-9045,school=11.41,cpi=-0.278428708107441,dep=-52.8205953482255,crisis=0)
recent_data <- data.frame(rgd = 35225, growth = -5.40940654815029,pop=-9045,school=11.41,cpi=-0.278428708107441,dep=-52.8205953482255,crisis=0)

predictions <- predict(lasso_model, newdata = recent_data)

print(predictions)

## K Fold cross validation ##############


# Define the number of folds for cross-validation



lasso_tune_grid <- expand.grid(alpha = 1, lambda = 1)

# Train the Lasso model with the correct tuning grid
lasso_model <- train(formula, 
                     data = data, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneGrid = lasso_tune_grid)

best_lambda <- lasso_model$bestTune$lambda

recent_data <- data.frame(rgd = 30647, growth = -5.40940654815029,pop=-9045,school=11.41,cpi=-0.278428708107441,dep=-52.8205953482255,crisis=0)

predictions <- predict(lasso_model, newdata = recent_data)

print(predictions)

# Extract coefficients at the best lambda
coefficients <- coef(lasso_model$finalModel, s = best_lambda)

# Convert to a more readable format (removing zero coefficients)
nonzero_coefficients <- coefficients[coefficients[,1] != 0, , drop = FALSE]

# Print the non-zero coefficients
print(nonzero_coefficients)


# Extract performance metrics from the lasso_model
lasso_rmse <- sqrt(mean((predict(lasso_model, data) - data$debthhr)^2))
lasso_r_squared <- cor(predict(lasso_model, data), data$debthhr)^2
lasso_mae <- mean(abs(predict(lasso_model, data) - data$debthhr))

# Create a new dataframe to store the summary statistics
lasso_summary_df <- data.frame(
  Model = "Lasso Regression",
  Metric = c("Intercept", "RMSE", "R-squared", "MAE"),
  Value = c("TRUE", lasso_rmse, lasso_r_squared, lasso_mae)
)

lasso_summary_df

# Ridge Regression
ridge_model <- train(formula, 
                     data = data, 
                     method = "glmnet", 
                     trControl = train_control, 
                     tuneGrid = expand.grid(alpha = 0, lambda = 1))


# Extract performance metrics from the ridge_model
ridge_rmse <- sqrt(mean((predict(ridge_model, data) - data$debthhr)^2))
ridge_r_squared <- cor(predict(ridge_model, data), data$debthhr)^2
ridge_mae <- mean(abs(predict(ridge_model, data) - data$debthhr))

# Create a new dataframe to store the summary statistics for Ridge Regression
ridge_summary_df <- data.frame(
  Model = "Ridge Regression",
  Metric = c("Intercept", "RMSE", "R-squared", "MAE"),
  Value = c("TRUE", ridge_rmse, ridge_r_squared, ridge_mae)
)

summary_df <- rbind(summary_df," ", lasso_summary_df," ",ridge_summary_df)
summary_df

# Define the recent household debt value
recent_debt <- 87  # Assuming this is the recent household debt value in percent

recent_data <- data.frame(rgd = 30647, growth = 3.87381093235382,pop=-8909.79,school=11.41,cpi=-1.03725464876037,dep=-55.3437900351411,crisis=0)


# Make predictions using each model
lm_prediction <- predict(initial_lr_model, newdata = recent_data) 
lasso_prediction <- predict(lasso_model, newdata = recent_data)
ridge_prediction <- predict(ridge_model, newdata = recent_data)






# setting to the current working directory
setwd("F:\\BIA\\final proj\\BI")

# get the names of all csv files in the data
all_files <- list.files(pattern = "*.csv")
all_files

# read each csv file
data_list <- lapply(all_files, read.csv)

# merge the data
merged_data <- do.call("rbind", data_list)

head(merged_data, 5)

tail(merged_data, 5)

data_size <- object.size(merged_data)
print(format(data_size, units = "auto"))

data_shape <- dim(merged_data)
print(paste("Number of rows:", data_shape[1]))
print(paste("Number of columns:", data_shape[2]))

column_names <- names(merged_data)
print(column_names)

str(merged_data)

column_types <- sapply(merged_data, function(x) class(x))
print(column_types)

nan_counts <- colSums(is.na(merged_data))
print(nan_counts)

# drop columns CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, Unnamed..27
columns_to_drop <- c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "Unnamed..27")
clean_data <- merged_data[, !(names(merged_data) %in% columns_to_drop)]
nan_counts <- colSums(is.na(clean_data))
print(nan_counts)

# drop all nan rows
clean_data <- na.omit(clean_data)
nan_counts <- colSums(is.na(clean_data))
print(nan_counts)

column_names <- names(clean_data)
print(column_names)

# converting FL_DATE CRS_DEP_TIME DEP_TIME CRS_ARR_TIME ARR_TIME ARR_DELAY "CRS_ELAPSED_TIME"    "ACTUAL_ELAPSED_TIME" "AIR_TIME"
# time / date format respectively

column_types1 <- sapply(clean_data, function(x) class(x))
print(column_types1)
head(clean_data)

#converting FL_DATE into date format

library(dplyr)

clean_data <- clean_data %>%
  mutate(FL_DATE = as.Date(FL_DATE))

column_types2 <- sapply(clean_data, function(x) class(x))
print(column_types2)

# EDA

#Split the data 

# creating the data set 

# divide the matrix into training set 70% and 
# testing 30% respectively with replacement 
sample <- sample(c(TRUE,FALSE), nrow(clean_data), 
                 replace=TRUE, prob=c(0.7,0.3)) 

# creating training dataset 
train_df <- clean_data[sample, ] 

# creating testing dataset 
test_df <- clean_data[!sample, ] 

print (train_df) 
print (test_df)

#XGBOOST
# Install and load necessary packages
#install.packages(c("caret", "xgboost"))
library(caret)
library(xgboost)

# Load your dataset (assuming 'clean_data' is your data frame)
# Replace 'cancelled' with your actual target variable name

# Set up the control for cross-validation
ctrl <- trainControl(
  method = "cv",        # Cross-validation method
  number = 5,           # Number of folds
  verboseIter = TRUE    # Print progress during training
)

# Define the parameter grid for hyperparameter tuning
param_grid <- expand.grid(
  nrounds = 100,         # Number of boosting rounds
  max_depth = c(3, 6, 9), # Maximum depth of a tree
  eta = c(0.01, 0.1),     # Learning rate
  gamma = 0,             # Minimum loss reduction to make a further partition
  colsample_bytree = 1,  # Subsample ratio of columns when constructing each tree
  min_child_weight = 1   # Minimum sum of instance weight (hessian) needed in a child
)

colnames(train_df)
# Train the XGBoost model with hyperparameter tuning using caret
xgb_model <- train(
  CANCELLED ~ .,              # Assuming all other columns are predictor variables
  data = train_df,
  method = "xgbTree",         # XGBoost algorithm
  trControl = ctrl,           # Cross-validation control
  tuneGrid = param_grid,      # Hyperparameter grid
  metric = "Accuracy"         # Metric to optimize (you can choose other metrics)
)

# Print the best parameters
cat("Best parameters:", xgb_model$bestTune, "\n")

# Access the trained XGBoost model
final_model <- xgb_model$finalModel

predictions <- predict(xgb_model, newdata = test_df)

print(predictions)

y_true <- test_data$cancelled

# Display the true values
cat("True Values (y_true):", y_true, "\n")

# Display the predicted values
cat("Predicted Values (y_pred):", predictions, "\n")



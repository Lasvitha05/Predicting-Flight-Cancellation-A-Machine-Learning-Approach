
install.packages(c("caret", "pROC")) 
library(caret)
library(pROC)

evaluate_classification_model <- function(model_name, Y_true, Y_pred) {
  print(model_name)
  Y_true <- factor(Y_true, levels = levels(factor(Y_pred)))
  Y_pred <- factor(Y_pred, levels = levels(factor(Y_true)))
  # Confusion Matrix
  confusion_mat <- confusionMatrix(Y_pred, Y_true)
  
  # Accuracy
  accuracy <- confusion_mat$overall["Accuracy"]
  
  # Recall
  recall <- confusion_mat$byClass["Sensitivity"]
  
  # Precision
  precision <- confusion_mat$byClass["Pos Pred Value"]
  
  # F1-Score
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # AUC-ROC
  roc_obj <- roc(Y_true, as.numeric(Y_pred))
  auc_roc <- auc(roc_obj)
  
  # Print and return results
  cat("Model Name:", model_name, "\n")
  cat("Confusion Matrix:\n", confusion_mat$table, "\n")
  cat("Accuracy:", accuracy, "\n")
  cat("Recall:", recall, "\n")
  cat("Precision:", precision, "\n")
  cat("F1-Score:", f1_score, "\n")
  cat("AUC-ROC:", auc_roc, "\n")
  
  # Plot ROC Curve
  plot(roc_obj, main = paste("ROC Curve -", model_name), col = "blue", lwd = 2)
  lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2, lwd = 2)
  
  return(list(
    confusion_matrix = confusion_mat$table,
    accuracy = accuracy,
    recall = recall,
    precision = precision,
    f1_score = f1_score,
    auc_roc = auc_roc
  ))
}



--------------------------------------------------------------------------------
# KNN with hyperparameter tuning
--------------------------------------------------------------------------------
  
  # Define the training control
  ctrl <- trainControl(method = "cv", number = 5)
  # Create a grid of hyperparameters to tune
  knn_grid <- expand.grid(k = seq(5, 20, by = 5))
  # Train the KNN model with hyperparameter tuning
  knn_model <- train(CANCELLED ~ ., data = train_data, method = "knn",
                     trControl = ctrl, tuneGrid = knn_grid)
  # Print the best hyperparameters
  print(knn_model)
  # plot
  plot(knn_model)
  # Make predictions on the test set
  predictions <- predict(knn_model, newdata = test_data)
  y_pred=predictions
  y_true=test_data$CANCELLED
  evaluate_classification_model("knn_model",y_true,y_pred)
  
--------------------------------------------------------------------------------
# Random Forest with hyperparameter tuning
--------------------------------------------------------------------------------
    
# Define the training control
ctrl <- trainControl(method = "cv", number = 5)
  # Create a grid of hyperparameters to tune
  rf_grid <- expand.grid(
    ntree = seq(100, 500, by = 100)  # Number of trees in the forest
  )
  # Train the Random Forest model with hyperparameter tuning
  rf_model <- train(
    CANCELLED ~ ., 
    data = train_data, 
    method = "rf",
    trControl = ctrl, 
    tuneGrid = rf_grid
  )
  # Print the best hyperparameters
  print(rf_model)
  # Make predictions on the test set
  predictions <- predict(rf_model, newdata = test_data)
  y_pred=predictions
  y_true=test_data$CANCELLED
  evaluate_classification_model("RandomForest_model",y_true,y_pred)
  









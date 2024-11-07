library(randomForest)
library(ggplot2)
library(caret)  # For stratified sampling
library(pROC)   # For ROC curve and AUC calculation

# Load the data
MyData <- read.csv("Class_Data_2.csv")

# Convert 'Enhancement' to a factor (target variable)
MyData$Enhancement <- as.factor(MyData$Enhancement)

# Remove the 'ID' column for the analysis
MyData <- MyData[ , -1]  # Removes the first column (ID)

# Check the structure of your data
str(MyData)

# Initialize variables to store results
n_iterations <- 20  # Set how many times you want to repeat the testing
set.seed(123)  # Set seed for reproducibility

# Create vectors to store accuracy, confusion matrices, and AUC for each iteration
all_accuracies <- numeric(n_iterations)
all_confusion_matrices <- list()
all_auc <- numeric(n_iterations)

# Initialize lists to store predicted probabilities and true labels across all iterations
all_pred_probs <- c()  # This will hold all predicted probabilities for the positive class
all_true_labels <- c() # This will hold all true labels

# Loop through multiple iterations to split and train/test
for (i in 1:n_iterations) {
  # Set seed for each iteration to ensure reproducibility
  set.seed(123 + i)  # Changing the seed slightly each time
  
  # Stratified sampling: create a partition keeping an even distribution of the target variable
  train_index <- createDataPartition(MyData$Enhancement, p = 0.7, list = FALSE)  # 70% training
  train_data <- MyData[train_index, ]
  test_data <- MyData[-train_index, ]
  
  # Train the Random Forest model
  rf_model <- randomForest(Enhancement ~ ., data = train_data, ntree = 500, mtry = 4, importance = TRUE, proximity = TRUE)
  
  # Predict probabilities on the test data
  pred_prob <- predict(rf_model, newdata = test_data, type = "prob")[,2]  # Get probabilities for the positive class
  
  # Store predicted probabilities and true labels for this iteration
  all_pred_probs <- c(all_pred_probs, pred_prob)
  all_true_labels <- c(all_true_labels, as.numeric(test_data$Enhancement))  # Convert factor to numeric
  
  # Predict class labels
  predictions <- predict(rf_model, newdata = test_data)
  
  # Confusion matrix to assess the model's performance
  confusion_matrix <- table(test_data$Enhancement, predictions)
  
  # Store the confusion matrix
  all_confusion_matrices[[i]] <- confusion_matrix
  
  # Calculate accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  all_accuracies[i] <- accuracy
  
  # Calculate AUC
  roc_obj <- roc(test_data$Enhancement, pred_prob)
  all_auc[i] <- auc(roc_obj)
}

# Print all accuracies
print(all_accuracies)

# Calculate the mean accuracy over all iterations
mean_accuracy <- mean(all_accuracies)
print(paste("Mean Accuracy:", round(mean_accuracy * 100, 2), "%"))

# Calculate standard deviation of accuracies
std_accuracy <- sd(all_accuracies)
print(paste("Standard Deviation of Accuracy:", round(std_accuracy, 4)))

# Coefficient of Variation for Accuracy
cv_accuracy <- (std_accuracy / mean(all_accuracies)) * 100
print(paste("Coefficient of Variation (Accuracy):", round(cv_accuracy, 2), "%"))

# Calculate the mean AUC over all iterations
mean_auc <- mean(all_auc)
print(paste("Mean AUC:", round(mean_auc, 4)))

# Calculate standard deviation of AUC
std_auc <- sd(all_auc)
print(paste("Standard Deviation of AUC:", round(std_auc, 4)))

# Coefficient of Variation for AUC
cv_auc <- (std_auc / mean(all_auc)) * 100
print(paste("Coefficient of Variation (AUC):", round(cv_auc, 2), "%"))

# Plot average ROC curve using all aggregated predictions and labels
average_roc <- roc(all_true_labels, all_pred_probs)

# Plotting with ggplot
# Extract ROC data
roc_data <- data.frame(
  FPR = 1 - average_roc$specificities,  # False Positive Rate (1 - Specificity)
  TPR = average_roc$sensitivities   # True Positive Rate (Sensitivity)
)

Figure_roc <- ggplot(data = roc_data,
       mapping = aes(x = FPR,
                     y = TPR))+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))+
  geom_path(color = "blue")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  labs(x = "Specificity", y = "Sensitivity", title = "Receiver Operating Characteristic (ROC) Curve")+
  theme_light()+
  annotate("label", x = 0.65, y = 0.15, label = paste("AUC =", round(auc(average_roc), 4)), 
           color = "black", size = 10, hjust = 0)+
  theme(axis.title.x = element_text(size = 24),  # Adjust x-axis label size
        axis.title.y = element_text(size = 24),
        axis.text.x = element_text(size = 18, color = "black"),   # Adjust x-axis tick label size
        axis.text.y = element_text(size = 18, colour = "black"),   # Adjust y-axis tick label size
        plot.title = element_text(size = 20, hjust = 0.5))  # Adjust y-axis label size

# Plot variable importance from the last model (or you can choose to average across iterations)
varImpPlot(rf_model)

# Extract the importance values from the last trained model
importance_values <- importance(rf_model)

# Create a data frame for plotting Gini
gini_df <- data.frame(Feature = rownames(importance_values),
                            MeanDecreaseGini = importance_values[, "MeanDecreaseGini"])

# Plot Mean Decrease Gini
Figure_Gini <- ggplot(gini_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +  # Flip the axes for better visibility
  labs(x = "", y = "Mean Decrease Gini") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 24),  # Adjust x-axis label size
        axis.text.x = element_text(size = 18, color = "black"),   # Adjust x-axis tick label size
        axis.text.y = element_text(size = 18, colour = "black"),   # Adjust y-axis tick label size
        plot.title = element_text(size = 20, hjust = 0.5))  # Adjust y-axis label size

# Create a data frame for plotting Accuracy
accuracy_df <- data.frame(Feature = rownames(importance_values),
                      MeanDecreaseAccuracy = importance_values[, "MeanDecreaseAccuracy"])

# Plot Mean Decrease Accuracy
Figure_acc <- ggplot(accuracy_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 24),  # Adjust x-axis label size
        axis.text.x = element_text(size = 18, color = "black"),   # Adjust x-axis tick label size
        axis.text.y = element_text(size = 18, colour = "black"),   # Adjust y-axis tick label size
        plot.title = element_text(size = 20, hjust = 0.5))  # Adjust y-axis label size

# Export Images
ggsave("Figure_Gini_2.jpg", plot = Figure_Gini, dpi = 300, width = 10, height = 6, units = "in")
ggsave("Figure_acc_2.jpg", plot = Figure_acc, dpi = 300, width = 10, height = 6, units = "in")
ggsave("Figure_roc.jpg", plot = Figure_roc, dpi = 300, width = 10, height = 6, units = "in")


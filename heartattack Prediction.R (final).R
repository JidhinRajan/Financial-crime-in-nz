# Load The DataSet Heart attack data 

setwd("C:/rclass")
heartattack.df <- read.csv("heartattack.csv")



names(heartattack.df)
str(heartattack.df)


summary(heartattack.df)                     #To find the summary statistics of all the coloumns


heartattack.df[is.na(heartattack.df)] <- 0  #checking for missing values. If missing values are found then
#replaced with zeros

missing_values <- colSums(is.na(heartattack.df)) # to create summary of the missing values
print(missing_values)

print(heartattack.df)                       # checking colonms in the heart attack data
print(colnames(heartattack.df))             # All colomns are printed

#EDA ANALYSIS ON DATA 
names(heartattack.df)                       #Getting the names of all features                                   
str(heartattack.df)                         #Understand the data types(num,chr)
#Checking for Null Values in Data
any(is.na(heartattack.df))                  #code the check any missing values present
summary(heartattack.df)


[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


#Install Packages

install.packages("dplyr")                   #Install dplyr package. Cleaning and preparing data
library(dplyr)
install.packages("readr")                   #Import data sets
library(readr)
install.packages("ggplot2")                 #To make graphs and charts
library(ggplot2)
install.packages("caret")                   #Training and comparing predictive models
library(caret)
install.packages("corrplot")
library(corrplot)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)


# Check for missing values
colSums(is.na(heartattack.df))


# Drop irrelevant columns and convert important variables from string to values
heartattack.df <- heartattack.df[-c(1, 3, 23, 24, 25)]  # Dropped coloumns that are in string which are pretty irrelevent for the prediction

heartattack.df <- heartattack.df %>%
  mutate(Diet = case_when(
    Diet == 'Healthy' ~ 1,
    Diet == 'Unhealthy' ~ 2,
    Diet == 'Average'~ 3
  )) # Converted Diet to 1, 2, 3 to aid for prediction.


# Split the Blood_Pressure column into Systolic_BP and Diastolic_BP for prediction, 
# we need to consider how blood pressure pressure is typically reported
heartattack.df <- heartattack.df %>%
  separate(Blood.Pressure, into = c("Systolic_BP", "Diastolic_BP"), sep = "/") %>%
  mutate(Systolic_BP = as.numeric(Systolic_BP),
         Diastolic_BP = as.numeric(Diastolic_BP))

# Verify the columns have been dropped
colnames(heartattack.df)


# Perform descriptive statistics
install.packages("reshape2")
library(reshape2)
install.packages("psych")
library(psych)
descriptive_stats <- describe(heartattack.df)
print(descriptive_stats)

# Visualize age distribution by Heart Attack Risk
ggplot(heartattack.df, aes(x = Exercise.Hours.Per.Week, fill = Heart.Attack.Risk)) +
  geom_histogram(binwidth = 5, alpha = 0.7) +
  labs(title = "Age Distribution by Heart Attack Risk", x = "Age", y = "Heart.Attack.Risk")

# Convert Heart.Attack.Risk to a factor
heartattack.df$Heart.Attack.Risk <- as.factor(heartattack.df$Heart.Attack.Risk)

# Visualize Exercise Hours Per Week distribution by Heart Attack Risk
ggplot(heartattack.df, aes(x = Exercise.Hours.Per.Week, fill = Heart.Attack.Risk)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge") +
  labs(title = "Exercise Hours Per Week Distribution by Heart Attack Risk", 
       x = "Exercise Hours Per Week", 
       y = "Count") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Heart Attack Risk", 
                    labels = c("No Risk", "Risk")) +
  theme_minimal()


ggplot(heartattack.df, aes(x = Heart.Attack.Risk, y = Exercise.Hours.Per.Week, fill = Heart.Attack.Risk)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Box Plot of Exercise Hours Per Week by Heart Attack Risk", 
       x = "Heart Attack Risk", 
       y = "Exercise Hours Per Week") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Heart Attack Risk", 
                    labels = c("No Risk", "Risk")) +
  theme_minimal()



ggplot(heartattack.df, aes(x = Age, fill = Heart.Attack.Risk)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  theme_minimal() +
  ggtitle("Age vs. Heart Attack Risk") +
  xlab("Age") +
  ylab("Count") +
  scale_fill_manual(values = c("blue", "red"))



ggplot(heartattack.df, aes(x = Cholesterol, fill = Heart.Attack.Risk)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  theme_minimal() +
  ggtitle("Cholesterol vs. Heart Attack Risk") +
  xlab("Cholesterol") +
  ylab("Count") +
  scale_fill_manual(values = c("blue", "red"))




# Group by Heart_Attack_Risk
grouped_stats <- heartattack.df %>%
  group_by(Heart.Attack.Risk) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max))

print("Descriptive Statistics grouped by Heart Attack Risk:")
print(grouped_stats)

# Assuming heartattack.df is your data frame
# Define the numeric variables
numeric_vars <- c("Cholesterol", "Heart.Rate", "Income", "BMI", "Triglycerides", "Diet",
                  "Exercise.Hours.Per.Week", "Sedentary.Hours.Per.Day", "Sleep.Hours.Per.Day", 
                  "Systolic_BP", "Diastolic_BP", "Stress.Level")



# Select the numeric columns from the data frame
numeric_data <- heartattack.df %>%
  select(all_of(numeric_vars))

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Convert categorical variables to dummy variables
heartattack.df <- heartattack.df %>%
  mutate_at(vars(Diabetes, Family.History, Smoking, Obesity, Alcohol.Consumption, 
                 Previous.Heart.Problems, Medication.Use), as.numeric)


# Calculate VIF
install.packages("car")
library(car)      # For calculating VIF
vif_values <- vif(model)

print("Variance Inflation Factors (VIF):")
print(vif_values)



[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


# Install and load necessary packages

library(caret)

library(pROC)

# Set seed for reproducibility
set.seed(123)

# Create a model excluding the target variable
model <- lm(as.numeric(Heart.Attack.Risk) ~ ., data = heartattack.df)

# Convert categorical variables to factors  for the model
heartattack.df <- heartattack.df %>%
  mutate(Diabetes = as.factor(Diabetes),
         Family.History = as.factor(Family.History),
         Smoking = as.factor(Smoking),
         Obesity = as.factor(Obesity),
         Alcohol.Consumption = as.factor(Alcohol.Consumption),
         Previous.Heart.Problems = as.factor(Previous.Heart.Problems),
         Medication.Use = as.factor(Medication.Use),
         Heart.Attack.Risk = as.factor(Heart.Attack.Risk))

# Split the data into training and test sets
trainIndex <- createDataPartition(heartattack.df$Heart.Attack.Risk, p = .8, 
                                  list = FALSE, 
                                  times = 1)
heart_train <- heartattack.df[trainIndex,]
heart_test <- heartattack.df[-trainIndex,]

# Train a logistic regression model
model <- train(Heart.Attack.Risk ~ ., data = heart_train, method = "glm", family = "binomial")

# Evaluate the model on the test set
predictions <- predict(model, heart_test)
conf_matrix <- confusionMatrix(predictions, heart_test$Heart.Attack.Risk)

# Print the confusion matrix and other evaluation metrics
print(conf_matrix)


[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


# Create new_data by selecting the first 5 rows as an example
new_data <- heartattack.df[1:5, ]


# Convert categorical variables in new_data to factors
new_data <- new_data %>%
  mutate(Diabetes = as.factor(Diabetes),
         Family.History = as.factor(Family.History),
         Smoking = as.factor(Smoking),
         Obesity = as.factor(Obesity),
         Alcohol.Consumption = as.factor(Alcohol.Consumption),
         Previous.Heart.Problems = as.factor(Previous.Heart.Problems),
         Medication.Use = as.factor(Medication.Use))


[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



install.packages("PRROC")
library(PRROC)


# Use the trained model to predict the heart attack risk
predicted_risk <- predict(model, new_data, type = "prob")

# Print the predicted probabilities
print(predicted_risk)

# Define training control
train_control <- trainControl(method = "cv", number = 5, savePredictions = TRUE)

#-1- Logistic Regression
# Train a logistic regression model
model_lr <- train(Heart.Attack.Risk ~ ., data = heart_train, method = "glm", family = "binomial", trControl = train_control)

# Summarize the model
summary(model_lr)

# Plot Prediction, Confusion, ROC & PR Curve for LR

# ROC Curve
pred_probs <- predict(model_lr, heart_test, type = "prob")[,2]
roc_obj <- roc(heart_test$Heart.Attack.Risk, pred_probs)
plot(roc_obj, col = "blue", main = "ROC Curve for Logistic Regression")

# Confusion Matrix
pred_labels <- predict(model_lr, heart_test)
conf_matrix <- confusionMatrix(predictions, heart_test$Heart.Attack.Risk)
print(conf_matrix)
fourfoldplot(conf_matrix$table, color = c("orange", "green"), 
             conf.level = 0, margin = 1, main = "Confusion Matrix - LR")

# Precision-Recall Curve
pr_obj <- pr.curve(scores.class0 = pred_probs, weights.class0 = as.numeric(heart_test$Heart.Attack.Risk) - 1, curve = TRUE)
plot(pr_obj, col = "red", main = "Precision-Recall Curve for Logistic Regression")


[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


#-2- Random Forest Test

install.packages('randomForest')
library(randomForest)

# Train a random forest model
model_rf <- randomForest(Heart.Attack.Risk ~ ., data = heart_train, ntree = 100, trControl = train_control)

# Summarize the model
print(model_rf)




# ROC Curve
pred_probs_rf <- predict(model_rf, heart_test, type = "prob")[,2]
roc_obj_rf <- roc(heart_test$Heart.Attack.Risk, pred_probs_rf)
plot(roc_obj_rf, col = "blue", main = "ROC Curve for Random Forest")

# Confusion Matrix
pred_labels_rf <- predict(model_rf, heart_test)
conf_matrix_rf <- confusionMatrix(pred_labels_rf, heart_test$Heart.Attack.Risk)
print(conf_matrix_rf)
fourfoldplot(conf_matrix_rf$table, color = c("orange", "green"), 
             conf.level = 0, margin = 1, main = "Confusion Matrix- Random Forest")

# Precision-Recall Curve
pr_obj_rf <- pr.curve(scores.class0 = pred_probs_rf, weights.class0 = as.numeric(heart_test$Heart.Attack.Risk) - 1, curve = TRUE)
plot(pr_obj_rf, col = "red", main = "Precision-Recall Curve for Random Forest")



[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


#-3- Gradient Boosting Machine (GBM)

install.packages('gbm')
library(gbm)

# Train a GBM model
model_gbm <- train(Heart.Attack.Risk ~ ., data = heart_train, method = "gbm",
                   trControl = train_control, verbose = FALSE)

# Summarize the model
print(model_gbm)


# ROC Curve
pred_probs_gbm <- predict(model_gbm, heart_test, type = "prob")[,2]
roc_obj_gbm <- roc(heart_test$Heart.Attack.Risk, pred_probs_gbm)
plot(roc_obj_gbm, col = "blue", main = "ROC Curve for GBM")

# Confusion Matrix
pred_labels_gbm <- predict(model_gbm, heart_test)
conf_matrix_gbm <- confusionMatrix(pred_labels_gbm, heart_test$Heart.Attack.Risk)
print(conf_matrix_gbm)
fourfoldplot(conf_matrix_gbm$table, color = c("orange", "green"), 
             conf.level = 0, margin = 1, main = "Confusion Matrix- GBM")

# Precision-Recall Curve
pr_obj_gbm <- pr.curve(scores.class0 = pred_probs_gbm, weights.class0 = as.numeric(heart_test$Heart.Attack.Risk) - 1, curve = TRUE)
plot(pr_obj_gbm, col = "red", main = "Precision-Recall Curve for GBM")



[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



#-4- k-NN Model

set.seed(123)

# Train the k-NN model

install.packages("e1071")
library(e1071)

# k-Nearest Neighbors
model_knn <- train(Heart.Attack.Risk ~ ., data = heart_train, method = "knn",
                   trControl = train_control, tuneLength = 10)

# Print the best value of k
print(model_knn$bestTune)


# ROC Curve
pred_probs_knn <- predict(model_knn, heart_test, type = "prob")[,2]
roc_obj_knn <- roc(heart_test$Heart.Attack.Risk, pred_probs_knn)
plot(roc_obj_knn, col = "blue", main = "ROC Curve for k-NN")

# Confusion Matrix
pred_labels_knn <- predict(model_knn, heart_test)
conf_matrix_knn <- confusionMatrix(pred_labels_knn, heart_test$Heart.Attack.Risk)
print(conf_matrix_knn)
fourfoldplot(conf_matrix_knn$table, color = c("orange", "green"), 
             conf.level = 0, margin = 1, main = "Confusion Matrix")

# Precision-Recall Curve
pr_obj_knn <- pr.curve(scores.class0 = pred_probs_knn, weights.class0 = as.numeric(heart_test$Heart.Attack.Risk) - 1, curve = TRUE)
plot(pr_obj_knn, col = "red", main = "Precision-Recall Curve for k-NN")



[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



# Train an XGBoost model with hyperparameter tuning
# Final model training with refined hyperparameter grid
# Define hyperparameter grid for tuning

install.packages("xgboost")
library(xgboost)


tune_grid <-expand.grid(
  eta = 0.1,
  max_depth = 5,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = c(1,5)*200,
  gamma = 0)

set.seed(123)

# Train the XGBoost model with hyperparameter tuning
model_xgb <- train(
  Heart.Attack.Risk ~ ., 
  data = heart_train, 
  method = "xgbTree", 
  trControl = trainControl(method = "cv", number = 5), 
  tuneGrid = tune_grid,
  metric = "Accuracy")

# Print the best tuning parameters
print(model_xgb)




[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



# neural networks

install.packages("NeuralNetTools")
install.packages("neuralnet")
library(NeuralNetTools)
library(neuralnet)
install.packages("nnet")
library(nnet)

model_nn <- train(
  Heart.Attack.Risk ~ ., 
  data = heart_train, 
  method = "nnet", 
  trControl = train_control, 
  linout = FALSE, 
  trace = FALSE,
  maxit = 100,
  tuneLength = 5
)

print(model_nn)

# Predict probabilities and classes
nn_prob <- predict(model_nn, heart_test, type = "prob")
nn_pred <- predict(model_nn, heart_test)

# Confusion Matrix
confusion_mat <- confusionMatrix(nn_pred, heart_test$Heart.Attack.Risk)
print(confusion_mat)

# Generate ROC curve
roc_obj <- roc(heart_test$Heart.Attack.Risk, nn_prob[, 2])
plot(roc_obj, col = "blue", main = "ROC Curve for Neural Network")

# Generate Precision-Recall curve
pr_obj <- pr.curve(scores.class0 = nn_prob[, 2], weights.class0 = as.numeric(heart_test$Heart.Attack.Risk) - 1, curve = TRUE)
plot(pr_obj, col = "red", main = "Precision-Recall Curve for Neural Network")

# Confusion Matrix Plot
confusion_mat_df <- as.data.frame(confusion_mat$table)
colnames(confusion_mat_df) <- c("Reference", "Prediction", "Freq")

ggplot(confusion_mat_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  ggtitle("Confusion Matrix for Neural Network") +
  theme_minimal()




[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]





# Feature Importance of LR
coefficients <- coef(model_lr$finalModel)
coefficients <- as.data.frame(coefficients)
coefficients$Feature <- rownames(coefficients)
rownames(coefficients) <- NULL

ggplot(coefficients, aes(x = reorder(Feature, coefficients), y = coefficients)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance for Logistic Regression",
       x = "Feature",
       y = "Coefficient") +
  theme_minimal()




# Feature Importance GBM
importance_gbm <- varImp(model_gbm, scale = FALSE)
importance_gbm_df <- data.frame(Feature = rownames(importance_gbm$importance), Importance = importance_gbm$importance[,1])
rownames(importance_gbm_df) <- NULL

ggplot(importance_gbm_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance for GBM",
       x = "Feature",
       y = "Importance") +
  theme_minimal()




# Feature Importance Random Forest
importance_rf <- importance(model_rf)
importance_rf_df <- data.frame(Feature = rownames(importance_rf), Importance = importance_rf[,1])
rownames(importance_rf_df) <- NULL



ggplot(importance_rf_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance for Random Forest",
       x = "Feature",
       y = "Importance") + 
  theme_minimal()



[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[=============]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



# Install the 'class' package (if it's not already installed)

  install.packages("class")

# Load the package
library(class)


# Define training control
train_control <- trainControl(method = "cv", number = 5, savePredictions = TRUE)



# Predict on the test set and calculate RMSE for each model
predictions_lr <- predict(model_lr, heart_test)
predictions_gbm <- predict(model_gbm, heart_test)
predictions_rf <- predict(model_rf, heart_test)
predictions_knn <- predict(model_knn, heart_test)

prediction_xgb <- predict(model_xgb, heart_test)
prediction_nn <- predict(model_nn, heart_test)
# Compute confusion matrices for each model
conf_matrix_lr <- confusionMatrix(predictions_lr, heart_test$Heart.Attack.Risk)
conf_matrix_gbm <- confusionMatrix(predictions_gbm, heart_test$Heart.Attack.Risk)
conf_matrix_rf <- confusionMatrix(predictions_rf, heart_test$Heart.Attack.Risk)
conf_matrix_knn <- confusionMatrix(predictions_knn, heart_test$Heart.Attack.Risk)

conf_matrix_xgb <- confusionMatrix(prediction_xgb, heart_test$Heart.Attack.Risk)
conf_matrix_nn <- confusionMatrix(nn_pred, heart_test$Heart.Attack.Risk)

# Extract Accuracy for each model
accuracy_lr <- conf_matrix_lr$overall['Accuracy']
accuracy_gbm <- conf_matrix_gbm$overall['Accuracy']
accuracy_rf <- conf_matrix_rf$overall['Accuracy']
accuracy_knn <- conf_matrix_knn$overall['Accuracy']

accuracy_xgb <- conf_matrix_xgb$overall['Accuracy']
accuracy_nn <- conf_matrix_nn$overall['Accuracy']

# Create a data frame for accuracy values
accuracy_values <- data.frame(
  Model = c("Logistic Regression", "Gradient Boosting", "Random Forest", "k-NN","XGBoost", "Nural network"),
  Accuracy = c(accuracy_lr, accuracy_gbm, accuracy_rf, accuracy_knn, accuracy_gbm, accuracy_nn)
)

# Plot the accuracy values
ggplot(accuracy_values, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Accuracy Comparison of Different Models") +
  xlab("Model") +
  ylab("Accuracy") +
  theme(legend.position = "none")

# Determine the best model based on Accuracy
best_model <- accuracy_values[which.max(accuracy_values$Accuracy), ]
cat("The best model is:", best_model$Model, 
    "with Accuracy:", best_model$Accuracy, "\n")




# Train an XGBoost model with hyperparameter tuning
# Final model training with refined hyperparameter grid
# Define hyperparameter grid for tuning
tune_grid <-expand.grid(
  eta = 0.1,
  max_depth = 5,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = c(1,5)*200,
  gamma = 0)

set.seed(123)

# Train the XGBoost model with hyperparameter tuning
model_xgb <- train(
  Heart.Attack.Risk ~ ., 
  data = heart_train, 
  method = "xgbTree", 
  trControl = trainControl(method = "cv", number = 5), 
  tuneGrid = tune_grid,
  metric = "Accuracy")

# Print the best tuning parameters
print(model_xgb)








# Extract the variable importance from the neural network model
importance_nn <- model_nn$variable.importance

# Convert importance to a data frame
importance_nn_df <- data.frame(Feature = names(importance_nn), Importance = importance_nn)

pr_obj <- pr.curve(scores.class0 = prediction_nn, weights.class0 = as.numeric(heart_test$Heart.Attack.Risk) - 1, curve = TRUE)
plot(pr_obj, col = "red", main = "Precision-Recall Curve for Neural Network")

# Plot the feature importance
ggplot(importance_nn_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance for Neural Network (Simple Method)",
       x = "Feature",
       y = "Importance") + 
  theme_minimal()




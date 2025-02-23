# Replace 'path_to_your_file.csv' with the actual file path
financial_crime_data <- read.csv("C:/rclass2/financialcrime.r.csv")
names(financial_crime_data)
print(financial_crime_data)
str(financial_crime_data)

financial_crime_data 
# Preview the data to ensure it's loaded correctly
head(financial_crime_data)
summary(financial_crime_data)

# Check for missing values in each column
colSums(is.na(financial_crime_data))

# Load the psych package for descriptive statistics
install.packages("psych")
library(psych)

# Calculate descriptive statistics for all variables
describe(financial_crime_data)


# Load ggplot2 for visualizations
install.packages("ggplot2")
library(ggplot2)


# Load corrplot for visualization
install.packages("corrplot")
library(corrplot)

# Display descriptive statistics for the dataset
descriptive_stats <- describe(financial_crime_data)
print(descriptive_stats)

# Calculate Pearson correlation matrix
correlation_matrix <- cor(financial_crime_data, method = "pearson", use = "complete.obs")

# Display the correlation matrix
print(correlation_matrix)

# Optionally, visualize the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


# Calculate correlation matrix
correlation_matrix <- cor(financial_crime_data, use = "complete.obs")
print(correlation_matrix)


# Visualize correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Check for multicollinearity using Variance Inflation Factor (VIF)
install.packages("car")
library(car)

# Load necessary libraries
library(tidyverse)
library(car)  # For VIF calculation

# all ecconomic indicators
financial_crime_m1<-lm(total_court_region_financial_crime~inflation_percentage + unemployment_rate_percentage + cost_of_living + 
                         household_disposable_income + household_debt + migration_data + 
                         real_exp_percentage_GDP, 
                       data = financial_crime_data) #model 1 development
summary(financial_crime_m1)

#Financial Hardship influence on financial crime
financial_crime_m2<-lm(total_court_region_financial_crime~inflation_percentage + unemployment_rate_percentage + cost_of_living + 
                         household_disposable_income + household_debt, 
                       data = financial_crime_data)
summary(financial_crime_m2) #model 2 development

#Influence of migration and GDP on crime
financial_crime_m3<-lm(total_court_region_financial_crime~  
                           migration_data + 
                         real_exp_percentage_GDP, 
                       data = financial_crime_data)
summary(financial_crime_m3) #model 3 development




par(mfrow = c(2, 2))
# Residuals vs Fitted Plot
plot(financial_crime_m2$fitted.values, residuals(financial_crime_m2),
     main = "Residuals vs Fitted Values for Model 2",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q Plot of Residuals
qqnorm(residuals(financial_crime_m2), main = "Q-Q Plot of Residuals for Model 2")
qqline(residuals(financial_crime_m2), col = "red")

# Scale-Location Plot
plot(financial_crime_m2$fitted.values, sqrt(abs(residuals(financial_crime_m2))),
     main = "Scale-Location Plot for Model 2",
     xlab = "Fitted Values", ylab = "Square Root of |Residuals|")
abline(h = 0, col = "blue")

# Residuals Over Time (assuming data has a time order)
plot(residuals(financial_crime_m2), type = "l", col = "blue",
     main = "Residuals Over Time for Model 2",
     xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))
# Cook's Distance Plot
cooksd <- cooks.distance(financial_crime_m2)
plot(cooksd, type="h", main="Cook's Distance for Model 2", ylab="Cook's Distance")
abline(h = 4/(nrow(financial_crime_data)-length(financial_crime_m2$coefficients)-2), col="red", lty=2)



library(car)
crPlots(financial_crime_m2, main = "Partial Residual Plots for Model 2")


# Load necessary libraries
library(forecast)  # for STL decomposition and time series analysis
library(ggplot2)   # for plotting
library(tseries)   # for additional time series functions


# Convert all relevant variables to time series with quarterly frequency
financial_crime_ts <- ts(financial_crime_data$total_court_region_financial_crime, frequency = 4)
inflation_ts <- ts(financial_crime_data$inflation_percentage, frequency = 4)
unemployment_ts <- ts(financial_crime_data$unemployment_rate_percentage, frequency = 4)
cost_of_living_ts <- ts(financial_crime_data$cost_of_living, frequency = 4)
household_income_ts <- ts(financial_crime_data$household_disposable_income, frequency = 4)
household_debt_ts <- ts(financial_crime_data$household_debt, frequency = 4)

# Check ACF and PACF for total_court_region_financial_crime
acf(financial_crime_ts, main = "ACF of Financial Crime Data (Quarterly)")
pacf(financial_crime_ts, main = "PACF of Financial Crime Data (Quarterly)"

     
#ADF___________________________________________________________________________

# Function to perform ADF test, apply differencing, and ensure stationarity
make_stationary <- function(series, name) {
  diff_count <- 0
  while (TRUE) {
    adf_result <- adf.test(series, alternative = "stationary")
    cat("ADF Test for", name, "with", diff_count, "differencing\n")
    print(adf_result)
    cat("\n")
    
    # Check if series is stationary (p-value < 0.05)
    if (adf_result$p.value < 0.05) {
      cat(name, "is stationary with", diff_count, "differencing.\n\n")
      return(series)  # Return the stationary series
    } else {
      # Apply differencing and increase counter
      series <- diff(series)
      diff_count <- diff_count + 1
      
      # Break if differencing exceeds 2
      if (diff_count > 2) {
        cat(name, "could not be made stationary with up to 2 differencing.\n\n")
        return(series)
      }
    }
  }
}

# Apply the make_stationary function to each variable
financial_crime_stationary <- make_stationary(financial_crime_ts, "Total Court Region Financial Crime")
inflation_stationary <- make_stationary(inflation_ts, "Inflation Percentage")
unemployment_stationary <- make_stationary(unemployment_ts, "Unemployment Rate Percentage")
cost_of_living_stationary <- make_stationary(cost_of_living_ts, "Cost of Living")
household_income_stationary <- make_stationary(household_income_ts, "Household Disposable Income")
household_debt_stationary <- make_stationary(household_debt_ts, "Household Debt")

# Function to perform ADF test, apply differencing, and ensure stationarity
# Function to perform ADF test, apply differencing, and log all results
make_stationary <- function(series, name) {
  diff_count <- 0
  adf_results <- list()  # To store each ADF result for logging
  stationary_series <- series  # To store the final stationary series
  
  while (TRUE) {
    adf_result <- adf.test(stationary_series, alternative = "stationary")
    cat("ADF Test for", name, "with", diff_count, "differencing\n")
    print(adf_result)
    adf_results[[paste(name, "Differencing:", diff_count)]] <- adf_result  # Store result in the list
    
    # Check if series is stationary (p-value < 0.05)
    if (adf_result$p.value < 0.05) {
      cat(name, "is stationary with", diff_count, "differencing.\n\n")
      return(list("Stationary Series" = stationary_series, "ADF Results" = adf_results))  # Return stationary series and all results
    } else {
      # Apply differencing and increase counter
      stationary_series <- diff(stationary_series)
      diff_count <- diff_count + 1
      
      # Break if differencing exceeds 2
      if (diff_count > 2) {
        cat(name, "could not be made stationary with up to 2 differencing.\n\n")
        return(list("Stationary Series" = stationary_series, "ADF Results" = adf_results))  # Return last differenced series and results
      }
    }
  }
}


# Apply the make_stationary function to each variable
financial_crime_stationary <- make_stationary(financial_crime_ts, "Total Court Region Financial Crime")
inflation_stationary <- make_stationary(inflation_ts, "Inflation Percentage")
unemployment_stationary <- make_stationary(unemployment_ts, "Unemployment Rate Percentage")
cost_of_living_stationary <- make_stationary(cost_of_living_ts, "Cost of Living")
household_income_stationary <- make_stationary(household_income_ts, "Household Disposable Income")
household_debt_stationary <- make_stationary(household_debt_ts, "Household Debt")

_____________________________________________
#STL
# Convert the 'total_court_region_financial_crime' column to a quarterly time series object
financial_crime_ts <- ts(financial_crime_data$total_court_region_financial_crime, frequency = 4)

# Perform STL Decomposition
stl_decomposition <- stl(financial_crime_ts, s.window = "periodic")

# Plot the STL decomposition
plot(stl_decomposition, main = "STL Decomposition of Total Court Region Financial Crime (Quarterly)")
_____________________________________________
#Lag Analysis for Exogenous Variables
# Define a function to plot CCF and identify optimal lags
analyze_ccf <- function(dependent_var, exogenous_var, exog_name) {
  ccf_result <- ccf(dependent_var, exogenous_var, main = paste("CCF of Financial Crime with", exog_name))
  
  # Identify the lag with the maximum absolute correlation
  optimal_lag <- which.max(abs(ccf_result$acf)) - (length(ccf_result$lag)/2)  # Adjust for zero-centered lags
  
  # Output the optimal lag
  cat("Optimal lag for", exog_name, "is:", optimal_lag, "\n")
  return(optimal_lag)
}
# Define a function to plot CCF for each exogenous variable
plot_ccf <- function(dependent_var, exogenous_var, exog_name) {
  ccf(dependent_var, exogenous_var, main = paste("CCF of Financial Crime with", exog_name))
}
par(mfrow = c(2, 2))
# Run CCF analysis for each exogenous variable
plot_ccf(financial_crime_ts, inflation_ts, "Inflation Percentage")
plot_ccf(financial_crime_ts, unemployment_ts, "Unemployment Rate Percentage")
plot_ccf(financial_crime_ts, cost_of_living_ts, "Cost of Living")
plot_ccf(financial_crime_ts, household_income_ts, "Household Disposable Income")
plot_ccf(financial_crime_ts, household_debt_ts, "Household Debt")

________________________________________________________
_______________________________________
_________________________________________________________
  
#check which is best forcasting technique before moving further
  
# Load necessary libraries
library(forecast)


# Convert the main variable and exogenous variables to time series objects
financial_crime_ts <- ts(financial_crime_data$total_court_region_financial_crime, frequency = 4)
inflation_ts <- ts(financial_crime_data$inflation_percentage, frequency = 4)
unemployment_ts <- ts(financial_crime_data$unemployment_rate_percentage, frequency = 4)
cost_of_living_ts <- ts(financial_crime_data$cost_of_living, frequency = 4)
household_income_ts <- ts(financial_crime_data$household_disposable_income, frequency = 4)
household_debt_ts <- ts(financial_crime_data$household_debt, frequency = 4)

# Combine exogenous variables into a matrix
exogenous_vars <- cbind(inflation_ts, unemployment_ts, cost_of_living_ts, household_income_ts, household_debt_ts)

# Step 1: Fit ARIMA Model (without exogenous variables)
arima_model <- auto.arima(financial_crime_ts)
print("ARIMA Model Summary:")
print(summary(arima_model))

# Step 2: Fit ARIMAX Model (with exogenous variables)
arimax_model <- auto.arima(financial_crime_ts, xreg = exogenous_vars)
print("ARIMAX Model Summary:")
print(summary(arimax_model))

# Compare AIC and BIC for both models
cat("ARIMA Model - AIC:", arima_model$aic, "BIC:", arima_model$bic, "\n")
cat("ARIMAX Model - AIC:", arimax_model$aic, "BIC:", arimax_model$bic, "\n")

# Step 3: Check Residuals of Each Model
# Plot residuals for ARIMA
checkresiduals(arima_model, main = "ARIMA Model Residuals")

# Plot residuals for ARIMAX
checkresiduals(arimax_model, main = "ARIMAX Model Residuals")

# Step 4: Evaluate Out-of-Sample Prediction Accuracy (if you have test data)
# Generate forecast for both models (adjust horizon as needed)
arima_forecast <- forecast(arima_model, h = 4)
arimax_forecast <- forecast(arimax_model, xreg = exogenous_vars, h = 4)

# Plot forecasts
plot(arima_forecast, main = "ARIMA Model Forecast")
plot(arimax_forecast, main = "ARIMAX Model Forecast")


install.packages("FinTS")
library(FinTS)        # For ArchTest function

____________________________________________________________________________________
_______________________
_________________________________________________________________________________
#R Code for ARIMAX Model with Exogenous Variables and ARCH Test

install.packages("FinTS")
library(FinTS)        # For ArchTest function

# Convert the main variable and exogenous variables to quarterly time series objects
financial_crime_ts <- ts(financial_crime_data$total_court_region_financial_crime, frequency = 4)
inflation_ts <- ts(financial_crime_data$inflation_percentage, frequency = 4)
unemployment_ts <- ts(financial_crime_data$unemployment_rate_percentage, frequency = 4)
cost_of_living_ts <- ts(financial_crime_data$cost_of_living, frequency = 4)
household_income_ts <- ts(financial_crime_data$household_disposable_income, frequency = 4)
household_debt_ts <- ts(financial_crime_data$household_debt, frequency = 4)

# Combine exogenous variables into a matrix
exogenous_vars <- cbind(inflation_ts, unemployment_ts, cost_of_living_ts, household_income_ts, household_debt_ts)

# Step 1: Fit an ARIMAX model with exogenous variables
# Use auto.arima with xreg to automatically select ARIMA parameters with exogenous variables
arimax_model <- auto.arima(financial_crime_ts, xreg = exogenous_vars)

# Extract residuals from the ARIMAX model
residuals_arimax <- residuals(arimax_model)

# Step 2: Perform the ARCH Test on ARIMAX residuals
arch_test_result <- ArchTest(residuals_arimax, lags = 12)  # Test with 12 lags
print("ARCH Test Results:")
print(arch_test_result)

# Step 3: Visualize Residuals to Assess Heteroskedasticity
plot(residuals_arimax, type = "l", main = "Residuals Over Time (ARIMAX Model - Heteroskedasticity check)", 
     ylab = "Residuals", xlab = "Time")
abline(h = 0, col = "red", lty = 2)


_____________________________________________________________________________________
______________________________________________
____________________________________________________________________________________
#Arimax model

# Combine exogenous variables into a matrix
exogenous_vars <- cbind(inflation_ts, unemployment_ts, cost_of_living_ts, household_income_ts, household_debt_ts)

# Step 1: Fit the ARIMAX Model
arimax_model <- auto.arima(financial_crime_ts, xreg = exogenous_vars)

# Print the model summary to interpret coefficients
print("ARIMAX Model Summary:")
print(summary(arimax_model))

# Step 2: Forecast using the ARIMAX Model
# Define forecast horizon (e.g., forecast for the next 10 periods)
forecast_horizon <- 10
arimax_forecast <- forecast(arimax_model, xreg = exogenous_vars, h = forecast_horizon)

# Plot the forecast
plot(arimax_forecast, main = "ARIMAX Model Forecast for Financial Crime")

# Display the forecasted values
print("Forecasted Values:")
print(arimax_forecast)


________________________________________________________________________________________
______________________________________________________________
_______________________________________________________________________________________



# Load necessary libraries
library(forecast)
library(tseries)

# Assuming `arimax_model` is already fitted

# Extract residuals from the ARIMAX model
residuals_arimax <- residuals(arimax_model)

# Step 1: Residual Diagnostics
# ACF and PACF plots for residuals
par(mfrow = c(1, 2)) # Set up plot area for side-by-side plots
acf(residuals_arimax, main = "ACF of Residuals (ARIMAX Model)")
pacf(residuals_arimax, main = "PACF of Residuals (ARIMAX Model)")

# Q-Q plot for normality check
par(mfrow = c(1, 1))
qqnorm(residuals_arimax, main = "Q-Q Plot of Residuals")
qqline(residuals_arimax, col = "red")

# Step 2: Ljung-Box Test
# Test if residuals are white noise
ljung_box_test <- Box.test(residuals_arimax, lag = 10, type = "Ljung-Box")
print("Ljung-Box Test Results:")
print(ljung_box_test)

# Step 3: Shapiro-Wilk Test for Normality
shapiro_test <- shapiro.test(residuals_arimax)
print("Shapiro-Wilk Test for Normality:")
print(shapiro_test)

# Step 4: Out-of-Sample Validation
# Define a training and test dataset if not already done
# Assuming you have time series data up to a certain point for training and later data for testing

# Split data into training and test sets (e.g., last 10 observations for testing)
n_test <- 10
training_data <- head(financial_crime_ts, -n_test)
test_data <- tail(financial_crime_ts, n_test)
training_exogenous <- head(exogenous_vars, -n_test)
test_exogenous <- tail(exogenous_vars, n_test)

# Fit ARIMAX model to the training data
arimax_model_train <- auto.arima(training_data, xreg = training_exogenous)

# Forecast using the test exogenous data
arimax_forecast <- forecast(arimax_model_train, xreg = test_exogenous, h = n_test)

# Calculate forecast accuracy
accuracy_metrics <- accuracy(arimax_forecast$mean, test_data)
print("Forecast Accuracy Metrics:")
print(accuracy_metrics)

# Plot actual vs forecasted values
plot(test_data, type = "l", col = "black", lwd = 2, main = "Out-of-Sample Validation", ylab = "Financial Crime")
lines(arimax_forecast$mean, col = "blue", lwd = 2)
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1, lwd = 2)

_____________________________________________________________________________________________
____________________________________________________________________________
____________________________________________________________________________________________


# Step 2: Calculate Forecast Evaluation Metrics
# Forecasted values and actual values
forecasted_values <- arimax_forecast$mean
actual_values <- test_data

# MAE
mae <- mean(abs(actual_values - forecasted_values))

# RMSE
rmse <- sqrt(mean((actual_values - forecasted_values)^2))

# MAPE
mape <- mean(abs((actual_values - forecasted_values) / actual_values)) * 100

# MdAPE
mdape <- median(abs((actual_values - forecasted_values) / actual_values)) * 100

# MASE
naive_forecast <- mean(abs(diff(training_data)))  # Naive forecast error
mase <- mean(abs(actual_values - forecasted_values)) / naive_forecast

# Display the results
cat("Forecast Evaluation Metrics:\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
cat("Median Absolute Percentage Error (MdAPE):", mdape, "%\n")
cat("Mean Absolute Scaled Error (MASE):", mase, "\n")

_______________________________________________________________________________________
#visuaize
***********************************************************************************
  *******************************************************************
# Step 3: Forecast vs. Actual Plot
plot(arimax_forecast, main = "Forecast vs. Actual for Financial Crime", xlab = "Time", ylab = "Financial Crime Level")
lines(test_data, col = "red", lwd = 2)  # Add actual values for comparison
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)

# Step 4: Confidence Intervals
# The `forecast` function already plots 80% and 95% confidence intervals by default in blue and light blue shades.
# Ensure that these are clearly visible in your plot above.

# Step 5: Residuals Over Time
residuals_arimax <- residuals(arimax_model_train)
par(mfrow = c(2, 2))  # Set up plotting space for multiple plots

# Plot residuals over time
plot(residuals_arimax, type = "l", main = "Residuals Over Time", ylab = "Residuals", xlab = "Time")
abline(h = 0, col = "red", lty = 2)


*****************************_______________________**********************************
______________________________________________________________________________________

# Step 2: Sensitivity Analysis with Adjusted Exogenous Variables
# Example adjustments to test sensitivity:
# 1. Increase inflation by 10%
# 2. Increase unemployment by 15%
# 3. Decrease household disposable income by 10%

# Adjusted exogenous variables for sensitivity tests
adjusted_inflation <- inflation_ts * 1.10  # Increase inflation by 10%
adjusted_unemployment <- unemployment_ts * 1.15  # Increase unemployment by 15%
adjusted_household_income <- household_income_ts * 0.90  # Decrease income by 10%
adjusted_cost_living <- cost_of_living_ts * 1.20 #increase by 20%

# Create new exogenous matrices for each scenario
exogenous_inflation_adjusted <- cbind(adjusted_inflation, unemployment_ts, cost_of_living_ts, household_income_ts, household_debt_ts)
exogenous_unemployment_adjusted <- cbind(inflation_ts, adjusted_unemployment, cost_of_living_ts, household_income_ts, household_debt_ts)
exogenous_income_adjusted <- cbind(inflation_ts, unemployment_ts, cost_of_living_ts, adjusted_household_income, household_debt_ts)
exogenous_cost_living_adjusted <- cbind(inflation_ts, unemployment_ts, cost_of_living_ts, adjusted_household_income, household_debt_ts)
# Ensure column names of adjusted exogenous matrices match the original `exogenous_vars` matrix
colnames(exogenous_inflation_adjusted) <- colnames(exogenous_vars)
colnames(exogenous_unemployment_adjusted) <- colnames(exogenous_vars)
colnames(exogenous_income_adjusted) <- colnames(exogenous_vars)
colnames(exogenous_cost_living_adjusted) <- colnames(exogenous_vars)

# Forecast with each adjusted scenario
forecast_inflation_adjusted <- forecast(arimax_model_train, xreg = tail(exogenous_inflation_adjusted, n_test), h = n_test)
forecast_unemployment_adjusted <- forecast(arimax_model_train, xreg = tail(exogenous_unemployment_adjusted, n_test), h = n_test)
forecast_income_adjusted <- forecast(arimax_model_train, xreg = tail(exogenous_income_adjusted, n_test), h = n_test)
forecast_cost_living_adjusted <- forecast(arimax_model_train, xreg = tail(exogenous_cost_living_adjusted, n_test), h = n_test)
# Step 3: Plot the Original and Adjusted Forecasts for Comparison
plot(arimax_forecast, main = "Sensitivity Analysis of ARIMAX Forecasts", xlab = "Time", ylab = "Financial Crime Level", col = "blue")
lines(test_data, col = "red", lwd = 2)  # Actual values for comparison
lines(forecast_inflation_adjusted$mean, col = "green", lwd = 2, lty = 2)
lines(forecast_unemployment_adjusted$mean, col = "purple", lwd = 2, lty = 3)
lines(forecast_income_adjusted$mean, col = "orange", lwd = 2, lty = 4)
lines(forecast_cost_living_adjusted$mean, col = "pink", lwd = 2, lty = 5)
legend("topleft", legend = c("arimax_forecast", "Actual", "Inflation +10%", "Unemployment +15%", "Income -10%" , "Cost of living +20%"),
       col = c("blue", "red", "green", "purple", "orange", "pink"), lty = c(1, 1, 2, 3, 4, 5), lwd = 2)

#Print forecasted values for each scenario
cat("arimax_forecast:\n")
print(arimax_forecast$mean)
cat("Forecast with Inflation +10%:\n")
print(forecast_inflation_adjusted$mean)
cat("Forecast with Unemployment +15%:\n")
print(forecast_unemployment_adjusted$mean)
cat("Forecast with Income -10%:\n")
print(forecast_income_adjusted$mean)
  
























______________________________________________________________________________________
_______________________________________________________________________________________
______________________________________________________________________________________
_______________________________________________________________________________________

# Convert Model 3 variables to time series format with quarterly frequency
financial_crime_ts <- ts(financial_crime_data$total_court_region_financial_crime, frequency = 4)
migration_ts <- ts(financial_crime_data$migration_data, frequency = 4)
real_exp_gdp_ts <- ts(financial_crime_data$real_exp_percentage_GDP, frequency = 4)

# Differencing each variable to achieve stationarity
financial_crime_diff <- diff(financial_crime_ts)
migration_diff <- diff(migration_ts)
real_exp_gdp_diff <- diff(real_exp_gdp_ts)

# Fit MLR Model 3 with differenced variables
mlr_model3 <- lm(financial_crime_diff ~ migration_diff + real_exp_gdp_diff)
summary(mlr_model3)

# Adjust Model 3 variables for sensitivity analysis
adjusted_migration_diff <- migration_diff * 1.10  # Increase migration by 10%
adjusted_real_exp_gdp_diff <- real_exp_gdp_diff * 1.05  # Increase real_exp_percentage_GDP by 5%
adjusted_inflation <- inflation_ts * 1.10  # Increase inflation by 10%
adjusted_unemployment <- unemployment_ts * 1.15  # Increase unemployment by 15%
adjusted_cost_living <- cost_of_living_ts * 0.85  # Decrease income by 15%
adjusted_household_income <- household_income_ts * 0.80 # Decrease household income by 20%

# Find the minimum length across all adjusted variables
min_length <- min(length(adjusted_migration_diff), length(adjusted_real_exp_gdp_diff),
                  length(adjusted_inflation), length(adjusted_unemployment),
                  length(adjusted_cost_living), length(adjusted_household_income))

# Trim each variable to the minimum length
adjusted_migration_diff <- adjusted_migration_diff[1:min_length]
adjusted_real_exp_gdp_diff <- adjusted_real_exp_gdp_diff[1:min_length]
adjusted_inflation <- adjusted_inflation[1:min_length]
adjusted_unemployment <- adjusted_unemployment[1:min_length]
adjusted_cost_living <- adjusted_cost_living[1:min_length]
adjusted_household_income <- adjusted_household_income[1:min_length]



# Create a data frame for the sensitivity test
sensitivity_data_2 <- data.frame(
  migration_diff = adjusted_migration_diff,
  real_exp_gdp_diff = adjusted_real_exp_gdp_diff,
  inflation_percentage_diff1 = adjusted_inflation,
  unemployment_rate_percentage_diff2 = adjusted_unemployment,
  cost_of_living_diff1 = adjusted_cost_living,
  household_income_diff = adjusted_household_income
)

# Make predictions with adjusted values
sensitivity_prediction_2 <- predict(mlr_model3, newdata = sensitivity_data_model3)
cat("MLR Model 3 Sensitivity Predictions:\n")
print(sensitivity_prediction_2)




# Plot ARIMAX forecast with original exogenous variables (Assuming arimax_forecast is defined)
plot(arimax_forecast, main = "Sensitivity Analysis: ARIMAX vs. MLR Model 3", 
     xlab = "Time", ylab = "Financial Crime Level", xlim = c(forecast_start - 5, forecast_start + 25), ylim = c(0, max(arimax_forecast$mean) * 1.1))


# Define the start point of the forecast, one period after the end of the actual data
forecast_start <- length(test_data) + 1

# Plot the original ARIMAX forecast and the actual data
plot(arimax_forecast$mean, type = "l", col = "blue", main = "Sensitivity Analysis: ARIMAX vs. MLR Model 3",
     xlab = "Time", ylab = "Financial Crime Level", xlim = c(forecast_start - 5, forecast_start + 25), ylim = c(0, max(arimax_forecast$mean) * 1.1))

# Add actual data in red
lines(1:length(test_data), test_data, col = "red", lwd = 2)


# Ensure all sensitivity predictions have the same length as `arimax_forecast`
forecast_length <- length(arimax_forecast$mean)
sensitivity_prediction_inflation <- head(sensitivity_prediction_inflation, forecast_length)
sensitivity_prediction_unemployment <- head(sensitivity_prediction_unemployment, forecast_length)
sensitivity_prediction_income <- head(sensitivity_prediction_income, forecast_length)
sensitivity_prediction_cost_living <- head(sensitivity_prediction_cost_living, forecast_length)
sensitivity_prediction_migration <- head(sensitivity_prediction_migration, forecast_length)
sensitivity_prediction_real_exp_gdp <- head(sensitivity_prediction_real_exp_gdp, forecast_length)




# Ensure that all sensitivity predictions have been generated correctly for the adjusted variables.
# Define the start point of the forecast, one period after the end of the actual data
forecast_start <- length(test_data) + 1

# Plot the ARIMAX forecast mean as the main forecast line
plot(arimax_forecast$mean, type = "l", col = "blue", main = "Sensitivity Analysis: ARIMAX vs. MLR Model 3",
     xlab = "Time", ylab = "Financial Crime Level", xlim = c(forecast_start - 5, forecast_start + 25), ylim = c(0, max(arimax_forecast$mean) * 1.1))

# Overlay the actual data for comparison
lines(1:length(test_data), test_data, col = "red", lwd = 2)

# Add sensitivity test predictions with specified colors and line types
lines(forecast_start:(forecast_start + length(sensitivity_prediction_inflation) - 1), sensitivity_prediction_inflation, col = "green", lwd = 2, lty = 2)
lines(forecast_start:(forecast_start + length(sensitivity_prediction_unemployment) - 1), sensitivity_prediction_unemployment, col = "purple", lwd = 2, lty = 3)
lines(forecast_start:(forecast_start + length(sensitivity_prediction_cost_living) - 1), sensitivity_prediction_cost_living, col = "pink", lwd = 2, lty = 5)
lines(forecast_start:(forecast_start + length(sensitivity_prediction_migration) - 1), sensitivity_prediction_migration, col = "brown", lwd = 2, lty = 6)
lines(forecast_start:(forecast_start + length(sensitivity_prediction_real_exp_gdp) - 1), sensitivity_prediction_real_exp_gdp, col = "cyan", lwd = 2, lty = 7)

# Add legend for all lines (excluding income)
legend("topleft", legend = c("ARIMAX Forecast", "Actual", "Inflation +10%", "Unemployment +15%", "Cost of Living -15%", "Migration +10%", "Real Exp GDP +5%"),
       col = c("blue", "red", "green", "purple", "pink", "brown", "cyan"), lty = c(1, 1, 2, 3, 5, 6, 7), lwd = 2)

  
  
  
  
  
  
  
  
  
  
  





  
  
  
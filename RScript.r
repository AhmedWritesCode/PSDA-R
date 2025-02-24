# Loading our dataset
data <- read.csv("processed_dataset.csv")

# A. CHECK IF The provided data is normally distributed using Normal Probability Plot (Q-Q plot)
# Create Q-Q plot for unemployment rate
qqnorm(data$Unemployment_Rate, main = "Normal Probability Plot (Q-Q Plot) - Unemployment Rate")
qqline(data$Unemployment_Rate)

# Create Q-Q plot for violent crimes rate
qqnorm(data$Total_Rate_of_Violent_Crimes, main = "Normal Probability Plot (Q-Q Plot) - Violent Crimes Rate")
qqline(data$Total_Rate_of_Violent_Crimes)

# B. CHECK IF The relationship between the variables is linear using Scatter Plot Graph
# Create a scatter plot
plot(data$Unemployment_Rate, data$Total_Rate_of_Violent_Crimes, main = "Scatter Plot of Unemployment Rate vs. Violent Crimes Rate",
     xlab = "Unemployment Rate", ylab = "Violent Crimes Rate")



# 1. Hypothesis Testing ----------------------------------------------------
cat("\n")
print("------------------------------------------------------------------------------")
print("1. Hypothesis Testing")
print("------------------------------------------------------------------------------")
cat("\n")

# Stating the Null, Alternative Hypotheses, significance level
print("Null Hypothesis: There is no correlation between the unemployment rate and total rate of violent crimes in the 51 states in 2014.")
print("Alternative Hypothesis: There is a correlation between the unemployment rate and total rate of violent crimes in the 51 states in 2014.")
cat("\n")
print("The Significance Level (α) is chosen to be 0.05 for this test.")
cat("\n")
print("H0: p = 0 (Null Hypothesis)")
print("H1: p != 0 (Alternative Hypothesis)")
cat("\n")

# Perform a t-test for the regression coefficient
model <- lm(Total_Rate_of_Violent_Crimes ~ Unemployment_Rate, data = data)
coef_test <- coef(summary(model))

# Extract the p-value
p_value <- coef_test["Unemployment_Rate", "Pr(>|t|)"]

# Set the significance level (e.g., α = 0.05)
significance_level <- 0.05

# Print the p-value
print("P-Value: ")
print(p_value)

# Compare p-value to the significance level
if (p_value <= significance_level) {
  print("The correlation is statistically significant.")
} else {
  print("The correlation is not statistically significant.")
}
cat("\n")


# 2. Correlation -------------------------------------------------------------
cat("\n")
print("------------------------------------------------------------------------------")
print("2. Correlation Testing")
print("------------------------------------------------------------------------------")
cat("\n")

# Assuming you have two vectors, unemployment rates and violent crimes rates, named 'unemployment' and 'violent_crimes' respectively.




# Calculate correlation coefficient
correlation <- cor(data$Unemployment_Rate, data$Total_Rate_of_Violent_Crimes)

# Print the correlation coefficient
print("Correlation Coefficient: ")
print(correlation)

# Perform the correlation test (Pearson Correlation Coefficient)
cor_test <- cor.test(data$Unemployment_Rate, data$Total_Rate_of_Violent_Crimes)

# Print the test results
print(cor_test)

# Create a scatter plot
plot(data$Unemployment_Rate, data$Total_Rate_of_Violent_Crimes,
     xlab = "Unemployment Rate", ylab = "Total_Rate_of_Violent_Crimes",
     main = "Correlation Scatter Plot Graph",
     type = "p")


cat("\n")

# 3. Regression Testing AND Goodness of Fit Test

cat("\n")
print("------------------------------------------------------------------------------")
print("3. Regression Testing (+ Including The Goodness of Fit Test)")
print("------------------------------------------------------------------------------")
cat("\n")

# Perform a linear regression
model <- lm(Total_Rate_of_Violent_Crimes ~ Unemployment_Rate, data = data)

# Check the regression summary
summary(model)

# Predict the values using the regression model
predicted <- predict(model)

# Create a scatter plot of observed vs. predicted values
plot(data$ Unemployment_Rate, data$ Total_Rate_of_Violent_Crimes,
     xlab = "Unemployment Rate", ylab = "Total Rate of Violent Crimes",
     main = "Regression Scatter Plot Graph",
     pch = 16, col = "blue")  # Use blue color and solid circles for observed values

points(data$Unemployment_Rate, predicted,
       pch = 15, col = "red")  # Use red color and open circles for predicted values

legend("topright", legend = c("Observed", "Predicted"),
       pch = c(16, 15), col = c("blue", "red"),
       bty = "n")  # Add legend

abline(model,col='red')


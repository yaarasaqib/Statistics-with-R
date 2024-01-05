####################################################
########### Lab 4 | Linear Regression and Anova  ###########
###############################

## Problem 1

library(openxlsx)

# Specify the file path
file_path <- "C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab/DeliveryTime.xlsx"

# Read the Excel file
data <- read.xlsx(file_path)


X <- cbind(1, data$x1, data$x2)  # Create a design matrix
beta <- solve(t(X) %*% X) %*% t(X) %*% data$y  # Calculate regression coefficients

# Calculate predicted values
predicted <- X %*% beta

# Calculate residuals
residuals <- data$y - predicted

res_df <- length(residuals)-ncol(X)

sd_residual  <- t(residuals) %*% residuals /res_df

# Calculate standardized residuals
standardized_residuals <- residuals / sd_residuals

# Print the standardized residuals
print(standardized_residuals)

## part b  | Studentise residual

#Assuming you have your data in a data frame named 'data'
# with columns 'y', 'x1', and 'x2'

# Fit your regression model (replace with your actual model)
X <- cbind(1, data$x1, data$x2)  # Create a design matrix
beta <- solve(t(X) %*% X) %*% t(X) %*% data$y  # Calculate regression coefficients

# Calculate predicted values
predicted <- X %*% beta

# Calculate residuals
residuals <- data$y - predicted

# Calculate estimated standard errors of residuals
hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
standard_errors <- sqrt(diag(hat_matrix) * sum(residuals^2) / (length(residuals) - ncol(X)))

# Calculate studentized residuals
studentized_residuals <- residuals / standard_errors

# Print the studentized residuals
print(studentized_residuals)

## part c | Press residual
n <- length(data$y)
H <- diag(n) - X %*% solve(t(X )%*% X) %*% t(X)

h <- diag(H)

pres_res<- residuals/(1-h)

## part c


n <- length(data$y)
H <- diag(n) - X %*% solve(t(X )%*% X) %*% t(X)

h <- diag(H)

pres_res<- residuals/(1-h)

cat("The press residuals are") 
print(pres_res)

## part d

# Fit a linear model
model <- lm(y~x1+x2,data=data)

# Calculate studentized residuals
rX <- rstudent(model)
print(rX)


##### problem 2

## part a

# Plot fitted values vs R-Student
plot(fitted(model), rX, main = "Fitted Values vs R-Student", xlab = "Fitted Values", ylab = "R-Student")
abline(h = 0, col = "red", lty = 2)  # Add a reference line at y = 0

## part b

# Create a QQ plot of residuals
qqnorm(residuals(model), main = "Normal Probability Plot (QQ Plot) of Residuals")
qqline(residuals(model), col = "red")  # Add a reference line


## part c
# Assuming you have loaded your data into the 'data' dataframe
library(car)
# Create partial regression plots
crPlots(model)

## part d
# Assuming you have loaded your data into the 'data' dataframe
# Also, assuming you have already performed linear regression and named the model 'model'
# Replace 'model' with your actual model

# Create partial residual plots
avPlots(model)


######## problem 3 ########

# Load required libraries
library(car) # For Box-Tidwell transformation
library(MASS) # For Box-Cox transformation
library(openxlsx)
# Load your electricity data
electricity_data <- read.xlsx("Electricity_Data.xlsx")

head(electricity_data)

# Fit a linear regression model
modele <- lm(Y ~ X, data = electricity_data)

# Generate residual plots before transformations
par(mfrow = c(2, 2))
plot(modele)

# Apply Box-Cox transformation to variable X
transformed_X <- boxcox(modele, lambda = seq(-2, 2, by = 0.1))
best_lambda <- transformed_X$x[which.max(transformed_X$y)]
electricity_data$X_transformed <- ifelse(best_lambda == 0, log(electricity_data$X), (electricity_data$X^best_lambda - 1) / best_lambda)

# Refit the model with transformed data
modele_transformed <- lm(Y ~ X_transformed, data = electricity_data)

# Generate residual plots after transformations
par(mfrow = c(2, 2))
plot(modele_transformed)



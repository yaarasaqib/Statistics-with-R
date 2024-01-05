#####################################
########### Lab 5| LR and ANOVA ##########
  
##### Problem 1 #####

# part a 

# For electricity data

# Load required libraries
library(car) # For Box-Tidwell transformation
library(openxlsx)
library(MASS) # For the Box-Cox transformation
library(ggplot2) # For plotting

# Load your electricity data
electricity_data <- read.xlsx("electricity_correct.xlsx")


# Fit a linear model 
model <- lm(Y~ X, data = electricity_data)

# Calculate residuals
residuals <- residuals(model)

# Create a residual plot
plot(fitted(model), residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,  # Point shape
     col = "blue"  # Point color
)




# part b 
# Perform the Box-Cox transformation to find the optimal lambda
bc_result <- boxcox(Y~X,data=electricity_data)
(lambda <- bc_result$x[which.max(bc_result$y)])


# Box tidwell Transformation
electricity_data$Y_transformed <- ((electricity_data$Y^lambda)-1)/lambda


#with the transformed data
# Fit a linear model with the transformed data
model_t <- lm(Y_transformed ~ X, data = electricity_data)
residual_t <- residuals(model_t)

# part c residual plot of transformed model 

# Create a residual plot

plot(fitted(model_t), residual_t,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,  # Point shape
     col = "blue"  # Point color
)

####### Problem 2 #########


# part a 

# For  windmill 


windmill_data <- read.xlsx("Wind_Mill_Data.xlsx")


# Fit a linear model 
model <- lm(Y~ X, data = windmill_data)

# Calculate residuals
residuals <- residuals(model)

# Create a residual plot
plot(fitted(model), residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,  # Point shape
     col = "blue"  # Point color
)




# part b 
# Perform the Box-Cox transformation to find the optimal lambda
bc_result <- boxcox(Y~X,data=windmill_data)
(lambda <- bc_result$x[which.max(bc_result$y)])


# Box tidwell Transformation
windmill_data$Y_transformed <- (windmill_data$Y^lambda)


#with the transformed data
# Fit a linear model with the transformed data
model_t <- lm(Y_transformed ~ X, data = windmill_data)
residual_t <- residuals(model_t)

# part c residual plot of transformed model 

# Create a residual plot

plot(fitted(model_t), residual_t,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16,  # Point shape
     col = "blue"  # Point color
)



  
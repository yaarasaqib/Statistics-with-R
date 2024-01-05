#######################################################
############ Lab 6 | Linear Regression and ANOVA ##########
# Load required libraries
library(openxlsx)
library(tidyverse)

# Load the data
DT <- read.xlsx("DeliveryTime.xlsx")

head(DT)

# Fit a linear regression model
lm_model <- lm(y~x1+x2, data = DT)

# Calculate the leverage points
leverage <- hatvalues(lm_model)

# Calculate COOK'S D
cooksd <- cooks.distance(lm_model)

# Calculate DFBETAS for each coefficient
dfbetas <- dfbetas(lm_model)

# Calculate DFFITS
dffits <- dffits(lm_model)

# Calculate COVRATIO
covratio <- covratio(lm_model)


## outliers
# Assuming you have already calculated the measures as mentioned in your previous code

# Set threshold values for each measure (you can adjust these as needed)
leverage_threshold <- 2 * ((ncol(DT) + 1) / nrow(DT)) # Adjust as needed
cooksd_threshold <- 1 # Adjust as needed
dfbetas_threshold <- 2 / sqrt(nrow(DT)) # Adjust as needed
dffits_threshold <- 2 * sqrt(ncol(DT) / nrow(DT)) # Adjust as needed
covratio_threshold <- 1 + 2 / nrow(DT) # Adjust as needed

# Identify outliers based on each measure
outliers_leverage <- which(leverage > leverage_threshold)
outliers_cooksd <- which(cooksd > cooksd_threshold)
outliers_dfbetas <- apply(dfbetas, 1, function(row) any(abs(row) > dfbetas_threshold))
outliers_dffits <- which(abs(dffits) > dffits_threshold)
outliers_covratio <- which(abs(covratio - 1) > covratio_threshold)

# Combine all outliers into one vector
all_outliers <- unique(c(outliers_leverage, outliers_cooksd, which(outliers_dfbetas), outliers_dffits, outliers_covratio))

# Display or work with the identified outliers
print(all_outliers)


####### Problem 2 ##########

# Load the data
DA <- read.xlsx("Acetylene.xlsx")

head(DA)

# Function to normalize a vector
normalize <- function(x) {
  mean_x <- mean(x)
  sd_x <- sqrt(var(x) * ((length(x) - 1) / length(x)))
  normalized_x <- (x - mean_x) / sd_x
  return(normalized_x)
}

# Apply the normalization function to each column of the data frame
DA_normalized <- as.data.frame(lapply(DA, normalize))

head(DA_normalized)


# part b 

# Assuming you already have DA_normalized dataset
cor_matrix <- cor(DA_normalized)

# Print the correlation matrix
print(cor_matrix)


## part c 

# Assuming you have already performed a linear regression with interactions and second-order terms
# Let's assume the model is called "model"

# Load the necessary library for VIF calculation
library(car)

# Create the model with interactions and second-order terms
model <- lm(Y ~ (X1 + X2 + X3)^2, data = DA_normalized)

# Calculate VIF for all regressors, including interactions and second-order terms
vif_values <- vif(model, type = 'predictor')

# Display VIF values
print(vif_values)

#########################################################
########### Lab 3 | Linear Regression And Anova #########
#########################################################

#### Problem 1  ########
# Data
y <- c(1, 4, 8, 9, 3, 8, 9)
x1 <- c(-1, 1, -1, 1, 0, 0, 0)
x2 <- c(-1, -1, 1, 1, 0, 1, 2)


X <- cbind(1,x1,x2,x1^2)
Xt <- t(X)

beta_h <- solve(Xt %*% X) %*% Xt %*% y

err_vec <- (y-X %*% beta_h)

n <- length(y)
p <- ncol(X)

sig <- as.numeric(t(err_vec) %*% err_vec/(n-p))

Vbeta <- sig * solve(Xt %*% X)

### let nul hypo R*beta = q

R <- matrix(c(1,rep(0,3),0,1,-1,0),byrow=T, nrow=2)
q <- matrix(c(0,0), byrow= T, nrow =2)

j <- nrow(R)    # number of restriction

F_stat <- t(R %*% beta_h-q) %*% solve(R %*% Vbeta %*% t(R)) %*% (R %*% beta_h-q)/j


# Load the required library
library(car)

x3 <- x1^2
# Fit the regression model
model <- lm(y ~ x1 + x2 + x3)

# Define the hypothesis matrix and vector
hypothesis_matrix <- R # Hypothesis: β0 = 0, β1 - β2 = 0
hypothesis_vector <- q # Values for the hypotheses

# Test the hypothesis
hypothesis_test <- linearHypothesis(model, hypothesis_matrix, hypothesis_vector)

# Print the hypothesis test results
print(hypothesis_test)
print(hypothesis_test$F[2])
# both look equals
#we don't have enough evidence to suggest that (Intercept) - I(x1^2) is significantly different from 0.

##### Problem 2  ########

# Data
y <-  c(1, 4, 8, 9, 3, 8, 9)
x1 <- c(-1, 1, -1, 1, 0, 0, 0)
x2 <- c(-1, -1, 1, 1, 0, 1, 2)
x3  <- x1^2

# part a  (LSE restricted model)

n <- length(y)
Xr <- matrix(rep(1,n),nrow=n)

beta_r <- solve(t(Xr)%*%Xr) %*% t(Xr)%*% y  # Restricted

# part b (RSS restricted and un restricted)

Xu <- cbind(1,x1,x2,x3)

beta_u<- solve(t(Xu)%*% Xu) %*% t(Xu) %*% y

RSS_u <- t(y-Xu%*%beta_u) %*% (y-Xu%*%beta_u)

RSS_r <- t(y-Xr%*%beta_r) %*% (y-Xr%*%beta_r)


## Part d
# Fit the unrestricted regression model
unres_model <- lm(y ~ x1 + x2 +x3)  

# Fit the linear regression model with an intercept-only (null) hypothesis
res_model <- lm(y ~ 1)

# Print the summary of the model
summary(unres_model)
summary(res_model)

# part a 
res_model$coefficients

# part b
RSS_res <- sum(res_model$residuals^2)
RSS_unres <- sum(unres_model$residuals^2)

print(RSS_res)
print(RSS_unres)
# RSS of Restricted model is higher

# part c

#F = (RSS_restricted - RSS_unrestricted) / (p - k) / (RSS_unrestricted / (n - p))

n<- length(y)
p<- 4     # no of parameters in unrestricted model
k <- 1     # No of parameters in restricted model
F_null <- ((RSS_res-RSS_unres)*(n-p))/((RSS_unres)*(p-k))

cat("The F statistics to test the null hypothesis is",F_null,".\n")

# Degrees of freedom
df_num <- p - k
df_denom <- n - p
alpha <- 0.05
# Calculate critical F-value
critical_f <- qf(1 - alpha / 2, df_num, df_denom)

cat("Critical F-value:", critical_f, "\n")

# F statistics is greater than the critical value we reject Ho


###### Problem 3  ##########
# lack of fit test

#install.packages("olsrr")

library(olsrr)

# Create the data
data <- data.frame(
  x = c(1.0, 1.0, 2.0, 3.3, 3.3, 4.0, 4.0, 4.0, 4.7, 5.0, 5.6,5.6, 5.6, 6.0, 6.0, 6.5, 6.9),
  y = c(10.84, 9.30, 16.35, 22.88, 24.35, 24.56, 25.86, 29.16, 24.59, 22.25, 25.90, 27.20, 25.61, 25.45, 26.56, 21.03, 21.46)
)


# Fit a linear regression model
model <- lm(y ~ x, data = data)
ols_pure_error_anova(model)

#######  problem 4   ########
# Given data
# Load the openxlsx package if not already installed
# install.packages("openxlsx")
library(openxlsx)

# Read the Excel file
RP_data <- read.xlsx("C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab/Rocket_Propulsion_Data.xlsx")
# Perform linear regression
model4 <- lm(y ~ x, data = RP_data)

k <- ncol(RP_data)   # Number of columns in RP_data

alpha <- 0.05
alpha_k <- alpha / k

# Estimated coefficients from the model
beta <- coef(model4)

# Standard error of coefficients
SE <- summary(model4)$coefficients[, "Std. Error"]

# Calculate Bonferroni-adjusted confidence intervals
CI_lower <- beta - qt(1 - alpha_k / 2, df = model4$df.residual) * SE
CI_upper <- beta + qt(1 - alpha_k / 2, df = model4$df.residual) * SE

# Print Bonferroni-adjusted confidence intervals
for (i in 1:length(beta)) {
  cat("Bonferroni-Adjusted Confidence Interval for parameter", i, ":", CI_lower[i], "to", CI_upper[i], "\n")
}


# Calculate t-values
t_values <- beta / SE

# Calculate Maximum Modulus confidence intervals
CI_lower <- beta - abs(qt(0.975, df = model4$df.residual)) * SE
CI_upper <- beta + abs(qt(0.975, df = model4$df.residual)) * SE

# Print Maximum Modulus confidence intervals
for (i in 1:length(beta)) {
  cat("Maximum Modulus Confidence Interval for parameter", i, ":", CI_lower[i], "to", CI_upper[i], "\n")
}


#### Scheffe Method

# Confidence level
conf_level <- 0.95

# Number of coefficients being compared
k <- length(coef(model4))
# Number of observations
n <- length(RP_data$y)

# F-distribution critical value for Scheffé's method
alpha_scheffe <- 1 - conf_level
F_critical_scheffe <- qf(1 - alpha_scheffe, df1 = k - 1, df2 = n - k)

# Scheffé's confidence intervals
CI_lower <- coef(model4) - sqrt(k * F_critical_scheffe) * SE
CI_upper <- coef(model4) + sqrt(k * F_critical_scheffe) * SE

# Print Scheffé's confidence intervals
for (i in 1:k) {
  cat("Scheffé's Confidence Interval for parameter", i, ":", CI_lower[i], "to", CI_upper[i], "\n")
}


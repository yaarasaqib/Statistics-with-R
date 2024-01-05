##### Lab 2 | Regression ANOVA  ####

getwd()
setwd("C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab")
set.seed(123)

####### Problem 1 ######

# y denoting the shear strength(psi)
# x : age of propellant(weeks)
Sno <- 1:20
y <- c(2158.70,1678.15,2316.00,2061.30,2207.5,1708.30,1784.70,2575.00,
       2357.90,2256.70,2165.20,2399.55,1779.8,2336.75,
       1765.30,2053.50,2414.40,2200.50,2654.20,1753.70)
x <- c(15.50,23.75,8.00,17.00,5.50,19.00,24.00,2.50,
       7.5,11.00,13.00,3.75,25.00,9.75,22.00,18.00,6.00,
       12.50,2.00,21.50)

# saving the data as xlsx
library(openxlsx)

# Combine data into a data frame
RP_data <- data.frame(Sno = Sno, y = y, x = x)

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, "Sheet1")


# Write data to the worksheet
writeData( wb, "Sheet1", RP_data)

# Save the workbook
#saveWorkbook(wb, "Rocket_Propulsion_Data.xlsx", overwrite = TRUE)

## using lm function 

# Fit the linear regression model
model <- lm(y ~ x)

# Print the coefficients
coefficients <- coef(model)
print(coefficients)

# Display the equation of the line
intercept <- coefficients["(Intercept)"]
slope <- coefficients["x"]
cat("Equation of the line: y =", intercept, "+", slope, "* x\n")

### without using library

one <- rep(1,length(y))
X <- cbind(as.matrix(one),x)
Xt <- t(X)

beta <- solve(Xt%*%X)%*% Xt %*%y
print(beta)

cat("Equation of the line: y =", beta[1], "+", beta[2], "* x\n")

#################################
####### Problem 2   ##########


Sno <- 1:25

# y denoting the delivery time

y <- c(16.68,11.50,12.03,14.88,13.75,18.11,8.00,17.83,79.24,
       21.50,40.33,21,13.50,19.75,24.00,29.00,15.35,19.00,9.50,
       35.10,17.90,52.32,18.75,19.83,10.75)

# x1 denoting the number of cases

x1<- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)

# x2 denoting the distance in feet

x2 <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,
        448,776,200,132,36,770,140,810,450,635,150)

DelTime<- data.frame(Serial= Sno, y=y , x1=x1, x2=x2)

# create a new workbook
wb2 <- createWorkbook()

# create a new worksheet
addWorksheet(wb2,"Sheet1")

# write data
writeData(wb2,"Sheet1",DelTime)

# savedata
#saveWorkbook(wb2, "DeliveryTime.xlsx", overwrite = TRUE)

DT_data<- read.xlsx("C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab/DeliveryTime.xlsx")

# Using Library
# Load the required library (if not already loaded)
library(stats)
# Perform linear regression using lm function
lm_model <- lm(y ~ x1 + x2, data = DT_data)

# Print the summary of the linear regression model
summary(lm_model)


##### without using library

# Create a matrix of predictors (X) and a vector of the dependent variable (Y)
X <- as.matrix(DT_data[, c("x1", "x2")])
Y <- as.vector(DT_data$y)

# Add a column of ones to the predictor matrix to account for the intercept term
X <- cbind(1, X)

# Calculate the coefficients (B) using the formula: B = (X'X)^-1 X'Y
coef <- solve(t(X) %*% X) %*% t(X) %*% Y

# Print the coefficients
print(coef)

cat("Equation of the line: y =", coef[1], "+", coef[2], "* x1","+",coef[3],"* x2")

######### Problem3  #########

#part a

RP_data <- read.xlsx("C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab/Rocket_Propulsion_Data.xlsx")
p=2
n=length(RP_data$y)
y <- RP_data$y
X <- as.matrix(cbind(as.matrix(rep(1,n)),RP_data[,3]))
Xt <- t(X)
H <- diag(n)- X%*%solve(Xt %*%X) %*% Xt

sigma2 <- t(y)%*% H %*%y/(n-p)

sqrt(sigma2[1,1])


# partb 

sig_mat <-  sigma2[1,1] *solve(Xt%*%X)

t1 <- coef[1]/sqrt(sig_mat[1,1])
t2 <- coef[2]/sqrt(sig_mat[2,2])

cat("the value of t1 and t2 are", t1, "and",t2,"respectively.")

####### Problem 4 ######


# part 1 #
## Generating random numbers ##
n <- 5000
w <- numeric(n)

# Generate the values of w
for (i in 1:n) {
  w[i] <- sum(rnorm(3, 0, 1)^2)
}

# Plot the histogram of w
hist(w, freq = FALSE, breaks = 50, main = "Histogram of w (Chi-square 3df)", xlab = "w")

# Generate values for the theoretical Chi-square distribution with 3 degrees of freedom
x <- seq(0, max(w), length.out = 500)
chi_square_theoretical <- dchisq(x, df = 3)

# Plot the theoretical Chi-square distribution curve
lines(x, chi_square_theoretical, col = "red")

## part3 ##
# Comparing theoretical and actual mean and variance 

mean(w)     #  simulated mean = 2.996054   theory mean= 3
var(w)      #  simulated var = 6.029171      theory var = 6

# On Comparrison b/w theoretical and simulated we find they are nearly
# close

<<<<<<< HEAD
###########################################

###### problem5 ######

## part 1 ### 
# Set the seed for reproducibility
set.seed(123)

# Part 1: Create the matrix X
X <- matrix(rchisq(8 * 5, df = 5), nrow = 8, ncol = 5)

# Step 3: Compute PX
PX <- X %*% solve(t(X) %*% X) %*% t(X)

# Verify if PX is idempotent
is_idempotent <- all.equal(PX, PX %*% PX)

# Print the result
print(is_idempotent)

# Part 2: Generate samples of u
library(MASS)  # Load the MASS package for mvrnorm function

# Create the mean vector µ
mu <- rep(0, 8)

# Create the 8x8 identity matrix I
I <- diag(8)

# Generate 5000 samples of Y from a multivariate normal distribution
n_samples <- 5000
u <- numeric(n_samples)
for (i in 1:n_samples) {
  Y <- mvrnorm(1, mu, I)
  u[i] <- t(Y) %*% PX %*% (Y)
}

# Print the first few values of u
print(head(u))

# Part 3: Visualize the results
hist(u, breaks = 30, col = "lightblue", main = "Histogram of u", xlab = "u")

# Generate x values for the theoretical chi-square distribution curve
x_values <- seq(0, max(u), length.out = 100)

# Compute the corresponding chi-square density values
chi_square_density <- dchisq(x_values, df = 5)

# Plot the theoretical chi-square distribution curve
lines(x_values, chi_square_density * n_samples * diff(hist(u)$breaks)[1], col = "red", lwd = 2)

# Print the sample mean and sample variance
print(paste("Sample Mean:", sample_mean))
print(paste("Sample Variance:", sample_variance))


######### Problem 6 #########
library(MASS)
mu <- rep(0, 8)
# Part 5: Generating sample from a Chi-square random variable
set.seed(123)

# Create a full rank 8x5 matrix X
X <- matrix(rnorm(8 * 5), nrow = 8, ncol = 5)
PX <- X %*% solve(t(X) %*% X) %*% t(X)

# Verify if PX is idempotent
is_idempotent <- all.equal(PX, PX %*% PX)
print(is_idempotent)
# Assuming you have already defined PX, mu, and other necessary variables

# Part 6: Generating sample from an F-distribution
PX1 <- diag(8) - PX
PX2 <- PX

df1 <- qr(PX1)$rank # Effective rank of PX2
df2 <- qr(PX2)$rank   # Effective rank of PX1

n_samples <- 5000
f_samples <- numeric(n_samples)
for (i in 1:n_samples) {
  Y <- mvrnorm(1, mu, diag(8))
  f_numerator <-   t(Y) %*% PX1 %*% Y / df1
  f_denominator <-  t(Y) %*% PX2 %*% Y / df2 
  f_samples[i] <- f_numerator / f_denominator
}

# Part 3: Plot histogram of f_samples
hist(f_samples, breaks = 30, col = "lightblue", main = "Histogram of F-distribution", xlab = "f")
sample_mean_f <- mean(f_samples)
sample_variance_f <- var(f_samples)
# Print the sample mean and sample variance of f
print(paste("Sample Mean of F-distribution:", sample_mean_f))
print(paste("Sample Variance of F-distribution:", sample_variance_f))
=======

 
>>>>>>> e7cc8591e0f0de8d23400ebc0e0228a5a07033df

# Part 4: Compute theoretical mean and variance of F-random variable
theoretical_mean_f <- df2 / (df2 - 2)
theoretical_variance_f <- (2 * df2^2 * (df2 + df1 - 2)) / (df1 * (df2 - 2)^2 * (df2 - 4))
# Print theoretical mean and variance of F-random variable
print(paste("Theoretical Mean of F-distribution:", theoretical_mean_f))
print(paste("Theoretical Variance of F-distribution:", theoretical_variance_f))


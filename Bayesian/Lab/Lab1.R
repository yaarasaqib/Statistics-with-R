############################################
######### Bayesian Analysis| Lab1 ##########
############################################

#### Qsn 1 | Generating Random Sample from known pdf

# Set seed for reproducibility
set.seed(221348)

# Generate random samples from different distributions
samples_bernoulli <- rbinom(50, 1, 0.6)  # Bernoulli(0.6)

samples_binomial <- rbinom(50, 10, 0.6)  # Binomial(10, 0.6)

samples_poisson <- rpois(50, 1.5)  # Poisson(1.5)

samples_normal <- rnorm(50, mean = 0, sd = 2)  # Normal(0, 4)

samples_uniform <- runif(50, min = -2, max = 2)  # Uniform(-2, 2)

samples_beta <- rbeta(50, 2, 1)  # Beta(2, 1)

samples_gamma <- rgamma(50, shape = 2, rate = 0.5)  # Gamma(2, 0.5)

samples_inverse_gamma <- 1/rgamma(50, shape = 2, scale = 0.5)  # Inverse-gamma(2, 0.5)


##### Qsn2 | Generating sample from discrete pdf

set.seed(221348)  # Setting seed for reproducibility

# Define the values and their respective probabilities
values <- c(1, 2, 3, 4)
probabilities <- c(0.2, 0.4, 0.3, 0.1)

# Generate 50 random samples from the specified distribution
samples <- sample(values, 50, replace = TRUE, prob = probabilities)



#inverse Sampling 

sample <- NULL

for(i in 1:50){
  x <- runif(0,1)
  if (x <= 0.2){
    sample <- c(sample,1)
  }
  if (0.2 < x <=0.6){
    sample <- c(Sample,2)
  }
  if (0.6 < x <=0.9){
    sample <- c(Sample,3)
  }
  if (0.9 < x <=1){
    sample <- c(Sample,4)
  }
}

####  Qsn 3 | generating using conditional

set.seed(123456)  # Setting seed for reproducibility

# Generating X values (Bernoulli distribution)
X <- rbinom(50, 1, 0.6)

# Initializing an empty vector to store the generated samples
samples <- numeric(length = 50)

# Generating Y values based on the values of X
for (i in 1:50) {
  if (X[i] == 0) {
    samples[i] <- rgamma(1, shape = 2, rate = 0.5)
  } else {
    samples[i] <- 1/rgamma(1, shape = 2, scale = 0.5)
  }
}


# Combine X and Y into a matrix representing the joint distribution
joint_samples <- cbind(X, Y)

#### Aliter 

Y <- rep(NA,50)
X <- rbinom(50,1,0.6)
Y[X==0] <- rgamma(n=sum(X==0), shape=2,rate=0.5)
Y[X==1] <- 1/rgamma(n=sum(X==1), shape=2,rate=0.5)
Y[1:5]


#### Aliter 2
X <- rbinom(50,size=1, prob= 0.5)

Y <- sapply(X,function(xx){
  ifelse(xx==0,rgamma(n=1,shape = 2,rate=0.5),
         1/rgamma(n=1,shape = 2,rate=0.5))
})

Mat <- cbind(X,Y)

Mat[1:5,]


### Qsn 4 |   Beta-Binomial

# Set parameters
n_samples <- 50
n_trials <- 10
alpha <- 2
beta_param <- 4

# Generate samples from the beta distribution for Y
Y_samples <- rbeta(n_samples, alpha, beta_param)

# Generate samples from the beta-binomial distribution
X_samples <- rbinom(n_samples, n_trials, Y_samples)

# Print generated samples
print(X_samples)



##### Qsn 5 | Draw 50 rs from BVN(2, 2, 1, 1, 0.5).

# Define covariance matrix and mean vector
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, byrow = TRUE)
mu <- c(2, 2)

# Generate standard normal random numbers for 50 samples
X <- matrix(rnorm(100), nrow = 50, byrow = TRUE)

# Transform standard normal random numbers to bivariate normal
Y <- mu + X %*% chol(Sigma)

# Print generated samples
print(Y)

### Aliter 

library(mvtnorm)

rmvnorm(n=50,mean=c(2,2), sigma= matrix(c(1,0.5,0.5,1),2,2))

#### Qsn 6 |  Draw Random Sample from a unit Square

# Set the number of samples
n_samples <- 1000

# Generate random samples within a unit square
x_values <- runif(n_samples, min = 0, max = 1)
y_values <- runif(n_samples, min = 0, max = 1)

# Combine x and y values into coordinates
coordinates <- cbind(x_values, y_values)


# Print the first 5 generated coordinates
print(coordinates[1:5,])

# Create a scatter plot of the generated points
plot(x_values, y_values, 
     xlab = "X values", ylab = "Y values",
     main = "Random Samples in a Unit Square",
     xlim = c(0, 2), ylim = c(0, 2),
     col = "blue", pch = 19)  # Adjust color, point type, etc. as needed


### Aliter 1
coord <- matrix(runif(100,min=0,max=1),50,2)

#### Qsn 7 | Draw  random sample  from a unit circle

n <- 5000
count <- 0
sample <- matrix(NA, nrow = 2, ncol = n)

while (count < n) {
  x <- runif(2, -1, 1)
  if (x[1]^2 + x[2]^2 <= 1) {
    count <- count + 1
    sample[, count] <- x
  }
}

# Plot generated points within a unit circle
plot(sample[1,], sample[2,], 
     xlab = "X values", ylab = "Y values",
     main = "Random Samples in a Unit circle",
     xlim = c(-1, 1), ylim = c(-1, 1),
     col = "green", pch = 19)


## Aliter 

count <- 0
Y <- NULL

while(count<5000){
  X <- runif(2,-1,1)
  if(sum(X^2) <= 1){
    Y <- rbind(Y,X)
  }
  count <- count+1
}

##### Qsn8 | finding a normalising constant

# Define the integrand function
f <- function(x) exp(-x^4)

n=10000; x<- rnorm(n,0,1)

c <- 1/(mean(f(x)/dnorm(x)))

print(c)


## Aliter

f <- function(x) exp(-x^4)

c<- 1/integrate(f, lower = -Inf, upper = Inf)$value
c

#### Qsn 9 | Finding the normalising constant using R

# Define the individual PDFs
f_x <- function(x) exp(-x^4)
f_y <- function(y) exp(-y^3)

# Approximate the individual integrals using integrate function
integral_fx <- integrate(f_x, lower = -Inf, upper = Inf)$value
integral_fy <- integrate(f_y, lower = 0, upper = Inf)$value

# Calculate c as the reciprocal of the product of the individual integrals
c_value <- 1 / (integral_fx * integral_fy)

# Print the value of c
print(c_value)


## USing Monte Carlo Simulation

# Define the joint PDF function
f_xy <- function(x, y) exp(-x^4 - y^3)

# Set the number of random samples
n_samples <- 100000

# Generate random samples for x and y from distributions covering the domain of interest
x_samples <- rnorm(n_samples)
y_samples <- rexp(n_samples)

# Evaluate the function for the generated samples
f_x_val <- exp(-x_samples^4)  # Evaluating f(x) for x samples
f_y_val <- exp(-y_samples^3)  # Evaluating f(y) for y samples

# Estimate c using Monte Carlo simulation by multiplying f(x) and f(y) averages
c_est <- 1 / (mean(f_x_val/dnorm(x_samples)) * mean(f_y_val/dexp(y_samples)))

# Print the estimated value of c
print(c_est)

### Qsn 10 | Find Bivariate normalising constants

# Define the joint PDF function
f_xy <- function(x, y) exp(-x^4 + x^2 * y^2 - y^4)

# Set the number of random samples
n_samples <- 100000

# Generate random samples from distributions covering the domain of interest
x_samples <- rnorm(n_samples)
y_samples <- rnorm(n_samples)

# Evaluate the function for the generated samples
f_values <- f_xy(x_samples, y_samples)

# Estimate c using Monte Carlo simulation, considering PDFs of x and y distributions
c_estimate <- 1 / mean(f_values/(dnorm(x_samples)*dnorm(y_samples)))
# Print the estimated value of c
print(c_estimate)

### Aliter  

# Install and load the pracma package
# install.packages("pracma")
library(pracma)

# Calculate the double integral
result <- 1 / integral2(f_xy, xmin = -50, xmax = 50, ymin = -50, ymax = 50)$Q

# Print the result
print(result)


getwd()
setwd("C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab")
set.seed(123)

################## problem 1 ##############
### Importing the data ###

# Load the openxlsx package
library(openxlsx)
#install.packages("openxlsx")

# Define your roll number
roll_number <- 221348

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, "Sheet1")

# Sample data
data <- data.frame(
  Y = runif(n=5,min=0,max=100),
  X1 = runif(n=5,min=0,max=10),
  X2 = runif(n=5,min=0,max=10000)
)

# Write the data to the worksheet
writeData(wb, sheet = "Sheet1", x = data)

# Save the workbook to a file
file_name <- paste0("LRA441_", roll_number, ".xlsx")
saveWorkbook(wb, file = file_name)

# part 2

# Install the xlsx package (if not installed already)
# install.packages("xlsx")

# Load the xlsx package
library("xlsx")
# Install the openxlsx package (if not installed already)
#install.packages("openxlsx")

# Load the openxlsx package
library("openxlsx")

# Read the data from the Excel file
file_path <- "C:/Users/hp/OneDrive/Desktop/Sem_3/MTH441A_LinearANOVA/Lab/LRA441_221348.xlsx"
Data_exm <- read.xlsx(file_path)

# Display the contents of the data frame
print(Data_exm)

# part3 
print(Data_exm$Y)
print(Data_exm$X1)
print(Data_exm$X2)


######### Problem 2  #####
# Assuming you have already installed and loaded the 'openxlsx' package.
# If not, you can install it using: install.packages("openxlsx")
# And then load it with: library(openxlsx)

# Assuming you have 'Data_exm' as a data frame containing the original data.

# Create squared variables Y1s, X1s, and X2s
Y1s <- Data_exm$Y^2
X1s <- Data_exm$X1^2
X2s <- Data_exm$X2^2

# Create a data frame E_data with squared variables
E_data <- data.frame(Y1s, X1s, X2s)

# Saving as an Excel file
write.xlsx(E_data, "lab1_file1.xlsx", sheetName = "Sheet1", colNames = TRUE, rowNames = FALSE, append = FALSE)

# Reading the data from the Excel file back into R
read_data <- read.xlsx("lab1_file1.xlsx")

# Print the data read from the Excel file
print(read_data)



######## Problem 3  ###########
### To find the LS estimator of part 1###

# model y= beta0+beta1*x1+ beta2*x2+eps

X <- cbind(1, as.matrix(data[,2:3]))
y<- as.matrix(data[,1])
Xt <- t(X)   # transpose of X

coeff <- solve(Xt%*%X)%*%Xt%*%y

lm(y~X)

# we see the library as well as the matrix formula provide the same coeff
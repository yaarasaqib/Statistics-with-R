### Chapter 8  Analysis of Variance and Covariance  ########

# One way Anova

##  Steps 

#1 Read the data
#2 represent the data
#3  Analyse the significance of the data
#4 Conduct the residual analysis
#5 interpret the coefficients

###1 reading part
getwd()
setwd("C:/Users/hp/OneDrive/Desktop/R For Statistics")
ozone <- read.table("ozone.txt",header=T)

ozone[,"wind"] <- as.factor(ozone[,"wind"])       # converting from characcter to factor
  
summary(ozone[,c("maxO3","wind")])

###2 Represnting part
plot(maxO3~wind,data=ozone,pch=15,cex=0.5)  # gives vertical boxplot 


###3 Analysing the significance of the factor

# null hypothesis H0 : alpha_i =0 for every i

reg.aov1 <- lm(maxO3~wind,data=ozone)
anova(reg.aov1)       # gives analysis of variance table

# here the p_value is Pr(>F)= 0.02074  less than 5%, H0 is rejected, 
#so we can accept the significance of the wind

# Conducting residual analysis

# residuals are available using residual function but not have the same variance

# in order to plot according to level wind we use lattice function

res.aov1 <- rstudent(reg.aov1)
library("lattice")
mypanel <- function(...){
  panel.xyplot(...)
  panel.abline(h=c(-2,0,2),lty=c(3,2,3), ...)
}
trellis.par.set(list(fontsize= list(point=5,text=8)))
xyplot(res.aov1~I(1:112)| wind, data=ozone,pch=20,ylim=c(-3,3),panel=mypanel,
       ylab="Residuals",xlab=" ")

# In theory 95% of studentised residual can be found in [-2,2]
# here there are 9 residuals outside of the interval

####### 5 Interpreting coefficient

summary(reg.aov1)

# We obtain a coeff matrix, for each parameter (each line) has four columns

# Create dummy variables for 'wind' manually
dummy_wind <- model.matrix(~ wind - 1, data = ozone)  # Exclude intercept with '- 1'

# Fit the linear model using dummy variables
model <- lm(maxO3 ~ dummy_wind, data = cbind(ozone, dummy_wind))

# Summary of the model
summary(model)

# annother constraint sum alpha i is 0

summary(lm(maxO3~C(wind,sum),data=ozone))

options(contrasts=c("contr.sum","contr.sum"))

summary(lm(maxO3~wind,data=ozone))

##############################################
#### Multiple-way ANOVA with interaction  ####


# step 1: Reading the data

ozone <- read.table("ozone.txt",header=T)

colnames(ozone)
summary(ozone[,c("maxO3","wind","rain")])

# Step 2 : Representing the data

boxplot(maxO3~wind*rain,data=ozone)

par(mfrow=c(1,2))
with(ozone,interaction.plot(wind,rain,maxO3))
with(ozone,interaction.plot(rain,wind,maxO3))

## Step 3 | Choosing the model

mod.int <- lm(maxO3~wind*rain,data=ozone)
anova(mod.int)

# We infer that model has no interaction

mod.without.int<-  lm(maxO3~wind+rain,data=ozone)
anova(mod.without.int)

# The two factors are significant so we keep the model


# Step 4 | Interpreting the coefficient


# Assuming 'ozone' is your data frame
ozone$wind <- as.factor(ozone$wind)
ozone$rain <- as.factor(ozone$rain)

# Now you can use C() to specify the contrasts if needed
model <- lm(maxO3 ~ C(wind, sum) + C(rain, sum), data = ozone)

# Summarize the model
summary(model)

options(contrasts=c("contr.sum","contr.sum"))

# The model are therefore written as

summary(lm(maxO3~wind+rain+wind:rain,data=ozone))
summary(lm(maxO3~wind+rain,data=ozone)) # without interaction


## Analysis of Covariance

##Step 1 Read The data

# Load the mtcars dataset
data(mtcars)

# View the structure of the dataset
str(mtcars)

dat <- mtcars[,c("mpg","cyl","wt")]

summary(dat)

## Step 2 : Representing the data

# Convert 'cyl' column to a factor with appropriate levels
dat$cyl <- factor(dat$cyl, levels = c(4, 6, 8))  # Assuming these are the possible levels for 'cyl'

# Check the levels of 'cyl' column after conversion
levels(dat$cyl)

# Creating a vector of colors for each 'cyl'
cyl_colors <- c("red", "blue", "green")

# Plotting mpg against wt using different point shapes and colors for each level of 'cyl'
plot(mpg ~ wt, pch = as.numeric(cyl), col = cyl_colors[as.numeric(dat$cyl)], data = dat, 
     xlim = range(dat$wt), ylim = range(dat$mpg))

# Adding a legend with colors
legend("topright", legend = levels(dat$cyl), pch = 1:nlevels(dat$cyl), col = cyl_colors, title = "Cylinders",cex=0.8)

library(lattice)
xyplot(mpg~wt|cyl, data=dat)       


# step3 | Choosing the model

global <- lm(mpg~-1+cyl+cyl:wt,data=dat)

# we remove intercept by writing -1, but it is necessary to calculate the
# intercept for each type of cyl by adding the factor cyl


slopeU <- lm(mpg~-1+cyl+wt,data=dat)

# model with only one intercept

interceptU <- lm(mpg~cyl:wt,data=dat)


#a in order to choose the best model we can conduct multiple tests

anova(slopeU,global)

anova(interceptU,global)


# as we can see of the first test of nested models is greater than
# 5% we can consider slope can be considered equal as the pvalue of
# second test is  less than 5% we conclude intercept are not equal

# b| null slope or intercept 

simple <-lm(mpg~wt,data=dat)

anova(simple,slopeU)

anova1 <- lm(mpg~cyl,data=dat)
anova(anova1,slopeU)

# both the above model are rejected as p value less than 0.05

# we only retain the model slopeU


# Step4| conducting the residual analysis

xyplot(rstudent(slopeU)~wt|cyl,ylab="res",data=dat)




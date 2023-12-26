##### Hypothesis Test #####

# Confidence interval for the mean

# Steps
#1 Read the data
#2 Calculate the descriptive statistics (mean,sd)
#3 construct a histoogram
#4 (optional) Test the normality of the data

library("readxl")
s1 <-read_excel("C:/Users/hp/OneDrive/Desktop/R For Statistics/New Data.xlsx",1)
s2 <-read_excel("C:/Users/hp/OneDrive/Desktop/R For Statistics/New Data.xlsx",2)

# For NH1 nasal height year 1
summary(s1$`Nasal Height of 1 years, mm`)

mean(s1$`Nasal Height of 1 years, mm`)
sd(s1$`Nasal Height of 1 years, mm`)

hist(s1$`Nasal Height of 1 years, mm`,main="Hist of NH1",freq=FALSE,ylab="Density",xlab="length in mm")
#prob= TRUE equivalent to frequency=FALSE

#Testing normality
# informal qqnorm
# or use shapiro wilk test
# here n=50 so test not necessary
shapiro.test(s1$`Nasal Height of 1 years, mm`)

# here  the p value is less than 0.05 so we cant assume it to normal

# Constructing CI
t.test(s1$`Nasal Height of 1 years, mm`,conf.level = 0.95)$conf.int

### confidence interval by hand

# mean is unknown and popln normal given

n=900  # sample size
x_bar= 3.4  # mean
sd= 2.61
p_mu= 3.25 # popln mean

# H0 mu=3.25
# H1 mu != 3.25
qnorm(1-0.025)
Z= (x_bar-p_mu)/(sd/sqrt(n))
# abs(z) is less than  1.96 approx = qnorm(1-0.025)

# 95% fiducial limit are

x_bar+qnorm(1-0.025)*(sd/sqrt(n)) # upper limit

x_bar-qnorm(1-0.025)*(sd/sqrt(n))   # lower limit

##################################
### Pearsons Chi square test of independance

# Aim : Test color is indep of sex or not

Boys<- c(592, 119 ,849 ,504 , 36)
Girls <- c( 544,97,677,451,14)
color <- rbind("Boys","Girls")
rownames(color)<- c("Boys","Girls")
colnames(color)<- c("Fair","Red","Medium","Dark","Jet Black")
color

# Step1 : Input the data
# step2 : Visualise the data
#step3 :(optional) calculate row and column profile
# step4:construct the chi square test
# step5:calculate the contriutions to the chi square statistics

#Visualising the data
par(mfrow=c(1,2))
barplot(color[1,],main="Boys",xlab="Hair color",ylab="Observe frequency")
barplot(color[2,], main="Girls",xlab="Hair color",ylab="Observe frequency")

#(optional) Row and col profile

round(100*color/sum(color),1) # joint freq

# row profile using prop.table func

round(100*prop.table(color,margin=1),1) # row profile
round(100*prop.table(color,margin=2),1) # Col prof

# Constructing test

results <- chisq.test(color)
#X-squared = 10.467, df = 4, p-value = 0.03325

# we reject the null hypo as fur bug obs value has pvalue
# less than 0.05  thereshold

# Contribution as percentage
round(100*results$residuals^2/results$stat,1)

round(results$residuals,3)

#####################################

## Comparison of two means

# Step1 : Read the data
# Step 2: Compare two  sub popln graphically
# Step 3: Calculate the descriptive statistics( mean,sd and quantile)
# step 4:(optional) test the normality of data of each sub popln
# step 5: Test the equality of variance
# step 6: Test the equality of mean

dietA<- c(25,32,30,34,24,14,32,24,30,31,35,25)
dietB <- c(44,34,22,10,47,31,40,30,32,35,18,21,35,29,22)

# H0: mu_A= mu_B
# H1: mu_A != mu_B

label=c("Diet A", "Diet B")
boxplot(dietA,dietB, xlab="Diet",ylab="weight",names=label)

# Calculating descriptive statistics

# Diet A
mean(dietA)
sd(dietA)
quantile(dietA)

# Diet A
mean(dietB)
sd(dietB)
quantile(dietB)

# Tesing normality

# for dietA
qqnorm(dietA)
qqline(dietA,col="grey")
shapiro.test(dietA) # gives p_val=0.1095

# as p val is greater than 5%  the normality of diet  A is accepted.

# simillarly for diet B

# If normality assumption is violated then the test can be
# conducted using non parametric like (wilcoxon) (wilcox.test)
# or Krukshal_Wallis test (krukshal.test)

# Testing of equality of variance

var.test(dietA,dietB,conf.level = 0.95)

# p valiue greater than 5% thus var are equal


# Testing the equality of means

t.test(dietA,dietB,alternative = 'two.sided',conf.level = 0.95,
       var.equal = TRUE)
# gives p value greater than 5% so  indicates that mean are not sinificantly diff
# and the null hypothesis is not rejected

###########################################
## Testing Conformity of proportion

# Step: Test the equality of the proportion to 50%
# with a type-one error rate 5%

binom.test(18,n=20,p=0.85,alternative ="greater")

#as p value= 0.4049 greater than 5%  so  we accept the null hypo
# i.e p=0.85 at  5% level of significance


### Comparing several proportion

Boys_col<- c(592,119,849,504,36)
total_pop<- c(1136,216,1526,955,50)

prop.test(Boys_col,total_pop)
# as p value is less than 5%  thus we reject the null hypo
# significant diff is confirmed
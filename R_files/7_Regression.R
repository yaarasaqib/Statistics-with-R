### Regression ###

### Simple linear Regression

# step1: read the data
# step2 : represent the scatter plot
# step3 : estimate the parameter
# step4 : draw the regression line
# step5 : conduct a residual analysis
# step6 : predict a new value

data(cars)

head(cars,n=10) # to view top 10 rows

str(cars)   # description of variables

summary(cars) # summary of basics statistics and info

table(is.na(cars)) # to check missing values

plot(cars,col="blue",pch=16,cex=0.75,main="Reationship between speed and stopping distance for 50 cars",
     xlab="speed in mph", ylab="stopping distance in feet ")
abline(m0,col="red")
# to check correlation

cor(cars$speed,cars$dist)  # good correlation

m0 <- lm(formula= dist~speed,data=cars)
m0

summary(m0)

names(m0)  # comp of the list

m0$coefficients  # gives coeff

coef(m0)    # func coef gives the same output

## conduct a residual analysis

res.m0 <- rstudent(m0)


plot(res.m0,pch=15,cex=0.5,ylab="Residual")
abline(h=c(-2,0,2),lty=c(2,1,2))
# in theory 95% of studentised residuals can
# be found in  interval [-2,2], here only three
# outside.
View(cars)
## Predicting a new value

xnew <-16
xnew <- as.data.frame(xnew)
colnames(xnew)<- "speed"
predict(m0,xnew,interval="pred")

#note: Argument of predict func must be data frame in which 
# explanatoryy variable must have the same name as orijinal


gridx<- data.frame(speed=seq(min(cars$speed),max(cars$speed),length=100))
CIline <- predict(m0,new=gridx,interval="conf",level=0.95)
CIpred <- predict(m0,new=gridx,interval="pred",level=0.95)
plot(dist~speed,data=cars,pch=15,cex=.5)
matlines(gridx,cbind(CIline,CIpred[,-1]),lty=c(1,2,2,3,3),col=1)
legend("topleft",lty=2:3,c("pred","conf"))

### Multiple Linear Regression

#step1 : Read the data.
#step2 : Represent the variables
#step3 : Estimate the parameter
#step4 : Choose the variable
#step5 : Predict a new value


ozone <- read.table("ozone.txt",header = T)

dim(ozone)
ozone.m<- ozone[,1:11]
names(ozone.m)
summary(ozone.m)


pairs(ozone.m)

reg.mul<- lm(maxO3~.,data=ozone.m)
summary(reg.mul)
# leaps package processes the choice of var
# the regsubset func gives various criteria

library("leaps")
choice <- regsubsets(maxO3~.,data=ozone.m,nbest = 1,nvmax=11)
plot(choice,scale="bic")

# Best model according to bic criteria

summary(choice)$which[which.min(summary(choice)$bic),]
     

final.reg <- lm(maxO3~T12+Ne9+Wx9+maxO3v,data=ozone.m)
summary(final.reg)


# conducting residual analysis

res.m<- rstudent(final.reg)
plot(res.m,pch=16,cex=.5,ylab="Residuals")
abline(h=c(-2,0,2),lty=c(2,1,2))

# predict new values

xnew <- matrix(c(19,8,2.05,70),nrow=1)
colnames(xnew)<- c("T12","Ne9","Wx9","maxO3v")
xnew<- as.data.frame(xnew)

predict(final.reg,xnew,interval="pred")

# the predicted value is 72.5 and Ci at 95% for the prediction
# is [43.8,101.2]
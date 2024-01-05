######### 9 Classification ###########

#Linear Discriminant Analysis

# Step1 | Read the csv

dati <- read.csv("insect.csv")

head(dati)

summary(dati)

dim(dati)


# Step2 | Constructing the model

library(MASS)

model <- lda(species~.,data=dati)

# Suppose if we know that joint 1 info only

model1 <- lda(species~joint1+aedeagus,data=dati)
model1


# Suppose the prior probabilities are 0.6 and o.4
model2 <- lda(species~.,data=dati, pior=c(0.6,0.4))
model2



# Step3 | Estimating the classification error rate

pred <- lda(species~. , data=dati, CV=TRUE)$class
table(pred,dati$species)


#Step4  | Making a prediction

mat <- matrix(c(157,127,56,171,125,49),nrow=2,ncol=3,byrow =T)

new_d <- as.data.frame(mat)
names(new_d) <- names(dati)[-1]

predict(model,newdata = new_d)


#######Logistic regression ##############

# step1 : Read the data
# Step2 : Construct the model
# Step3 : Choose the model
# Step4 : Make the predictions


datt <- read.csv("titanic.csv")

head(datt)

summary(datt)

dim(datt)


# Now encoding embarked variable
# Convert 'Embarked' and "Sex" to a factor variable
datt$Embarked <- factor(datt$Embarked)
datt$Sex       <- factor(datt$Sex)

# Check the unique values in 'Embarked' column
unique(datt$Embarked)
unique(datt$Sex)

# Map 'Embarked' categories to numeric values (0, 1, 2)

datt$Embarked <- as.numeric(datt$Embarked) - 1
datt$Sex <- as.numeric(datt$Sex) - 1


# Show the updated dataset
head(datt)

sum(is.null(datt))   # shows no null value


# Step 2 | Constructing the model

model_log <- glm(Survived~Pclass+Sex+Age+SibSp+Fare+Embarked,
                 family=binomial,data=datt)

summary(model_log)



# 'Fare' and 'Embarked' have higher p-values (above 0.05), 
#indicating they might not be statistically significant predictors of 
#survival in this model.

model_final <- glm(Survived~Pclass+Sex+Age+SibSp,
                   family=binomial,data=datt)
summary(model_final)


# Suppose if we consider only Age

model_Age <- glm(Survived~Age,family=binomial,data=datt)

summary(model_Age)

# As intercept is not significant so we remove it

model_Age_wi <- glm(Survived~-1+Age,family=binomial,data=datt) 

summary(model_Age_wi)


# Now lets plot the model_Age_wi(without intercept)

beta<- coef(model_Age_wi)
x <- seq(0,80,by=1)
y <- exp(beta*x)/(1+exp(beta*x))

plot(x,y,type="l", xlab ="Age",ylab="p(x)")

paste("If age is less than 40 then survival chance is more than",
      round(exp(beta*40)/(1+exp(beta*40)),2))

# we can also take the other logistic models

## Step3 | Chosing the model

#anova(model_Age,model_Age_wi)
# these can be also used
# anova(model_Age,model_final)

model_step <- step(model_final,direction="backward")

model_step 


# Step 4 | Making Prediction

new_datt <- as.data.frame(matrix(c(2,1,25,3,1,0,25,2,3,0,26,1),byrow = T,
                                 ncol=4,nrow=3))

names(new_datt) <- names(datt)[c(3,4,5,6)]


predict(model_step,newdata = new_datt, type="response")


# We can calculate missclassification probabilities
# USing predict function

prediction_prob <- predict(model_step,new_data=datt)
prediction_label <- as.numeric(prediction_prob>0.5)

table(datt$Survived,predictionlabel)


# Model's miss classification rate

error <- sum(prediction_label!=datt$Survived)/nrow(datt)
error


# as the rate are more optimistic as we are using 

library(boot)

cost <- function(Y_obs,prediction_prob){
  return(mean(abs(Y_obs-prediction_prob)>0.5))
}

cv.glm(datt,model_step,cost)$delta[1]


###### Decision Tree ##########

## Step 1 | Reading the data

datt <- read.csv("titanic.csv")

head(datt)

summary(datt)

dim(datt)


# Now encoding embarked variable
# Convert 'Embarked' and "Sex" to a factor variable
datt$Embarked <- factor(datt$Embarked)
datt$Sex       <- factor(datt$Sex)

# Check the unique values in 'Embarked' column
unique(datt$Embarked)
unique(datt$Sex)

# Map 'Embarked' categories to numeric values (0, 1, 2)

datt$Embarked <- as.numeric(datt$Embarked) - 1
datt$Sex <- as.numeric(datt$Sex) - 1



datT <- datt[,c(2,3,4,5,6)]

head(datT)

summary(datT)

# Step 2 | Constructing and Analysing the Tree

library(rpart)

DT <- rpart(Survived~.,data=datT)
DT



DT2 <-  rpart(Survived~Sex+Age,data=datT)
DT2

# Assuming you have a decision tree object named DT2

# Plot the decision tree
plot(DT2, 
     branch = 0.2,        # Length of branches
     compress = TRUE,     # Compress the plot
     margin = 0.1,        # Margin size
     main = "Decision Tree 2"
)

# Adjust text size in the plot
text(DT2, 
     cex = 0.6,           # Reduce text size for nodes
     col = "black"        # Set color for text
)


summary(DT2)

# Step 3 | Choosing the size of the tree

DT3 <- rpart(Survived~.,data=datT,minsplit=5)
DT3

plot(DT3, 
     branch = 0.2,        # Length of branches
     compress = TRUE,     # Compress the plot
     margin = 0.1,        # Margin size
     main = "Decision Tree 3"
)

# Adjust text size in the plot
text(DT3, 
     cex = 0.6,           # Reduce text size for nodes
     col = "black"        # Set color for text
)


# tree is prune using snip.rpart

DT3 <- rpart(Survived~.,data=datT,minsplit=5,xval=53)
plotcp(DT3)


# lets construct the final tree

DTF <- rpart(Survived~.,data=datT,minsplit=5,cp=0.017)


plot(DTF, 
     branch = 0.2,        # Length of branches
     compress = TRUE,     # Compress the plot
     margin = 0.1,        # Margin size
     main = "Decision Tree Final"
)

# Adjust text size in the plot
text(DTF, 
     cex = 0.6,           # Reduce text size for nodes
     col = "black"        # Set color for text
)




pred <- predict(DTF, newdata = newdata_for_prediction)



tab <- table(datT$Survived,round(pred))
tab

error <- sum(round(pred) != datT$Survived)/nrow(datT)
error


# Step 4 | Predicting survival for new values

new.d <- data.frame(matrix(c(3,1,28,2,1,0,37,0,2,1,27,3),nrow=3,
                           ncol = 4,byrow = T))

names(new.d) <- names(datT)[-1]


pred <- predict(DTF,newdata=new.d)
pred
  

###################################################
######### Exploratory Multivariate Analysis #######
###################################################


##### Principal Component Analysis ######


## step1 | Reading The data

datd <- read.csv("decathalon.csv")

dim(datd)

summary(datd)


## Step 2 | Choosing the active individual or variable

## Step 3 | Choosing if the variable needto be standardise

install.packages("FactoMineR")
library(FactoMineR)


# Remove non-quantitative variable (assuming it's the first column)
datd_numeric <- datd[, -1]  # This removes the first column assuming it's 'Athlets'

# Now perform PCA on the modified dataset
res.pca <- PCA(datd_numeric, quanti.sup = 11:12, quali.sup = 13)

names(res.pca)


## Step 4 | Choosing The Number of Dimension

barplot(res.pca$eig[,2],names=paste("Dim",1:nrow(res.pca$eig)))


round(res.pca$eig[1:4,],2)


## Step5 | Analysing the Result


round(cbind(res.pca$ind$coord[,1:3],res.pca$ind$cos2[,1:3],
            res.pca$ind$contrib[,1:3]),digits=2)


loadings <- sweep(res.pca$var$coord,2,sqrt(res.pca$eig[1:5,1]),
                  FUN="/")
plot(res.pca, choix = "ind",habillage=13,axes=3:4,cex=0.7)

plot(res.pca, choix = "var",habillage=13,axes=3:4)


# to save the graph in the pdf format

# pdf("mypath/mygraph.pdf")
# plot(res.pca, choix = "var",habillage=13,axes=3:4,newplot=F)
# dev.off()


# Step 6 | Automatically Describing the Dimensions of variability

dimdesc(res.pca)


dimdesc(res.pca, proba = 0.2)


# Step 7 | Going back to Raw Data

# Scale and round the first 12 columns of datd
scaled_rounded_data <- round(scale(datd[, 2:13]),2)

head(scaled_rounded_data)


round(cor(datd[,2:13]),2)

###########################
#### Correspondence Analysis#####



# Load the necessary library (if not already installed)
#install.packages("ca")
library(ca)

# Step 1 | Reading the dataset 

# Load the HairEyeColor dataset
data(HairEyeColor, package = "datasets")

datc <- HairEyeColor

# View the structure of the dataset
str(datc)

summary(datc)

head(datc)


# Step 2 | Choosing the active rows and columns

# Step 3 | Conduct the CA

library(FactoMineR)


datc <- xtabs(Freq ~ Hair + Eye, data = HairEyeColor)

# View the created contingency table
datc

res.ca <- CA(datc)

barplot(res.ca$eig[,2],names=paste("Dim",1:nrow(res.ca$eig)))

round(res.ca$eig,3)


# Step 5 | Analysing the result 

plot(res.ca, invisible = c("col","col.sup"))

round(cbind(res.ca$row$coord[,1:3],res.ca$row$cos2[,1:3]),2)


round(cbind(res.ca$col$coord[,1:3],res.ca$col$cos2[,1:3]),2)

plot(res.ca, axes =2:3)




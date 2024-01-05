########### Lab 7| Linear Regression and ANOVA #############

library(openxlsx)

#Problem1 

wdat<- read.xlsx("webster_data.xlsx")

head(wdat)

# part a

# Assuming your dataset is stored in 'wdat'
# Calculate mean and variance for each column
means <- colMeans(wdat)
variances <- apply(wdat, 2, var)

# Calculate (n - 1) for each variable (n is the number of observations)
n_minus_1 <- nrow(wdat) - 1

# Normalize the data
normalized_wdat <- as.data.frame(lapply(wdat, function(x) (x - means) / sqrt(n_minus_1 * variances)))

# Print the first few rows of the normalized dataset
head(normalized_wdat)


# part b | Calcuate VIFs

model <- lm(y~x1+x2+x3+x4+x5+x6,data=normalized_wdat)

# Calculate VIFs
vif_values <- car::vif(model)
# Print VIFs
print(vif_values)

# Problem 1

Webster.Data<-wdat

x0<-c(rep(1,12))
X<-cbind(rep(1,12),Webster.Data$x1,Webster.Data$x2,Webster.Data$x3,Webster.Data$x4,Webster.Data$x5,Webster.Data$x6)

A<-t(X)%*%X
vif_d<-diag(solve(A))
model<-lm(y~x1+x2+x3+x4+x5+x6, data=Webster.Data)
# package "car" is required
vif(model)

X1<-(Webster.Data$x1-mean(Webster.Data$x1))/(sqrt(11*var(Webster.Data$x1)))
X2<-(Webster.Data$x2-mean(Webster.Data$x2))/(sqrt(11*var(Webster.Data$x2)))
X3<-(Webster.Data$x3-mean(Webster.Data$x3))/(sqrt(11*var(Webster.Data$x3)))
X4<-(Webster.Data$x4-mean(Webster.Data$x4))/(sqrt(11*var(Webster.Data$x4)))
X5<-(Webster.Data$x5-mean(Webster.Data$x5))/(sqrt(11*var(Webster.Data$x5)))
X6<-(Webster.Data$x6-mean(Webster.Data$x6))/(sqrt(11*var(Webster.Data$x6)))

TX<-cbind(rep(1,12),X1,X2,X3,X4,X5,X6)
TX1<-cbind(X1,X2,X3,X4,X5,X6)
cor(TX1)

daig_TD<-diag(solve(t(TX)%*%TX))

EV<-eigen(t(TX)%*%TX)$values

Evec<-eigen(t(TX)%*%TX)$vectors


# Deleting 1st rwo and 1st coulmn
MEvec1<-Evec[-1,]
MEvec2<-MEvec1[,-1]

#Excluding the eqigen value associated with the intercept

MEV<-EV[2:7]

EVec_ratio_eg <- matrix(0,nrow=6,ncol=6)

for (i in 1:6){
  for (j in 1:6){
    EVec_ratio_eg[i,j]<-MEvec2[i,j]^2/MEV[j]
  }
}

VIFj<-daig_TD

VIFj1<-0
for (j in 1:6){
  VIFj1[j]<-  sum(EVec_ratio_eg[j,])
}

EVec_ratio_eg1<-matrix(0,nrow=6,ncol=6)

for (j in 1:6){
  EVec_ratio_eg1[j,]<-EVec_ratio_eg[j,]/VIFj1[j]
}

t(EVec_ratio_eg1)



###### Problem 2######
Acetylene_Data<- read.xlsx("Acetylene.xlsx",header=T)

y<-Acetylene_Data$Y
x1<-(Acetylene_Data$X1-mean(Acetylene_Data$X1))/(sqrt(15*var(Acetylene_Data$X1)))
x2<-(Acetylene_Data$X2-mean(Acetylene_Data$X2))/(sqrt(15*var(Acetylene_Data$X2)))
x3<-(Acetylene_Data$X3-mean(Acetylene_Data$X3))/(sqrt(15*var(Acetylene_Data$X3)))
x12<-x1*x2
x13<-x1*x3
x23<-x2*x3
x11<-x1*x1
x22<-x2*x2
x33<-x3*x3

TY<-(y-mean(y))/(sqrt(15*var(y)))
TX1<-x1
TX2<-x2
TX3<-x3
TX12<-(x12-mean(x12))/(sqrt(15*var(x12)))
TX13<-(x13-mean(x13))/(sqrt(15*var(x13)))
TX23<-(x23-mean(x23))/(sqrt(15*var(x23)))
TX11<-(x11-mean(x11))/(sqrt(15*var(x11)))
TX22<-(x22-mean(x22))/(sqrt(15*var(x22)))
TX33<-(x33-mean(x33))/(sqrt(15*var(x33)))

Mod_Acetylene_Data2<-cbind(TY,TX1,TX2,TX3,TX12,TX13,TX23,TX11,TX22,TX33)

mod1 <- lmridge(TY~., as.data.frame(Mod_Acetylene_Data2), K = seq(0, 0.1, 0.01))
plot(mod1, type = "ridge")

summary(mod1)

Webster.Data<-read.xlsx("/Users/satya/Desktop/MTH441/R/Webster_Data.xlsx", sheetName = 1, 
                        header = TRUE)

WY<-(Webster.Data$y-mean(Webster.Data$y))/(sqrt(11*var(Webster.Data$y)))
WX1<-(Webster.Data$x1-mean(Webster.Data$x1))/(sqrt(11*var(Webster.Data$x1)))
WX2<-(Webster.Data$x2-mean(Webster.Data$x2))/(sqrt(11*var(Webster.Data$x2)))
WX3<-(Webster.Data$x3-mean(Webster.Data$x3))/(sqrt(11*var(Webster.Data$x3)))
WX4<-(Webster.Data$x4-mean(Webster.Data$x4))/(sqrt(11*var(Webster.Data$x4)))
WX5<-(Webster.Data$x5-mean(Webster.Data$x5))/(sqrt(11*var(Webster.Data$x5)))
WX6<-(Webster.Data$x6-mean(Webster.Data$x6))/(sqrt(11*var(Webster.Data$x6)))

WTX<-cbind(WX1,WX2,WX3,WX4,WX5,WX6)


mod2 <- lmridge(WY~., as.data.frame(WTX), K = seq(0, 0.2, 0.02))
plot(mod2, type = "ridge")

summary(mod2)

# PCA
PCA_AD<-cbind(c(rep(1,16)),TX1,TX2,TX3,TX12,TX13,TX23,TX11,TX22,TX33)

AD_EV<-0
AD_EV<-eigen(t(PCA_AD)%*%PCA_AD)$values

AD_Evec<-0
AD_Evec<-eigen(t(PCA_AD)%*%PCA_AD)$vectors

TZ1<-0
#for(i in 1:16){
TZ1<-AD_Evec[2,2]*TX1 + AD_Evec[3,2]*TX2+AD_Evec[4,2]*TX3+AD_Evec[5,2]*TX12+AD_Evec[6,2]*TX13+AD_Evec[7,2]*TX23+AD_Evec[8,2]*TX11+AD_Evec[9,2]*TX22+AD_Evec[10,2]*TX33
#}

model_PC_AD<-lm(TY~TZ1 -1)
summary(model_PC_AD)
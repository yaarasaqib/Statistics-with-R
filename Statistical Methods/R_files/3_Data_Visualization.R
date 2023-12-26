## Unit 3 Graphics ##

ozone <- read.table("ozone.txt",header=T)

summary(ozone)
# maxO3 = max daily ozone
# T12 temp at midday
# Wx12 projection of wind speed vector 
#on east west axis at midday 

ozone$wind <- as.factor(ozone$wind)
ozone$rain<- as.factor(ozone$rain)

# scatter plot of maxO3 and T12
plot(maxO3~T12,data=ozone)

# plot of quantative variable wrt qual Var
plot(maxO3~wind,data=ozone, xlab= "Wind Direction",ylab="Maximum ozone concentration")

boxplot(maxO3~wind,data=ozone) # same graph

# plot of qual var wrt qual var
plot(rain~wind,data=ozone)

# qual var accord to quantiative
plot(wind~T12,data=ozone)
spineplot(wind~T12,data=ozone) # same plot


plot(ozone$T12,ozone$wind,yaxt="n",xlab="T12",
     ylab="wind",pch="+")
axis(side=2,at=1:4,labels=levels(ozone$wind))

#yaxt="n" remove scale from y axis


plot(ozone[,"maxO3"],xlab="num.",ylab="maxO3",cex=0.5,pch=16)

# line type
plot(ozone[,"maxO3"],xlab="num.",ylab="maxO3",type="l")

# representing histogram

hist(ozone$maxO3,main="Historam",prob=TRUE,xlab="Ozone")


# Kernel density estimate uisng density

plot(density(ozone$maxO3),main="Kernel Density Estimate",xlab="Ozone")


# barplot using table function

barplot(table(ozone$wind))

plot(ozone$wind) # same as Prev

# Adding to graphs

# plot and add text
plot(maxO3~T12,data=ozone,type="n")
text(ozone$T12,ozone$maxO3,substr(rownames(ozone),5,8),cex=0.75)

# to draw crosses on the coordinate
plot(maxO3~T12,data=ozone,type="p",pch=3,cex=0.75)
text(ozone$T12,ozone$maxO3,substr(rownames(ozone),5,8),cex=0.75,pos=3,offset=0.3)

# to add colored symbol addition to rainy days

selection <- ozone[,"rain"]=="Rainy"
points(ozone[selection,"T12"],ozone[selection,"maxO3"],pch=21,
       bg="grey70",cex=0.75)
abline(v=27,lty=2) # no rain if temp exceed 27

## to add line to a graph

plot(ozone[1:7,"maxO3"],type="l")
lines(ozone[8:14,"maxO3"],col="grey50")


#  taking the equivalent range
rangey <- range(ozone[1:7,"maxO3"],ozone[8:14,"maxO3"])
plot(ozone[1:7,"maxO3"],type="l",ylim=rangey,lty=1)
lines(ozone[8:14,"maxO3"],col="grey50")

# Graph with the multiple dimension

# 3D  graph with R

f <- function(x,y){
  z<- 10*sin(sqrt(x^2+y^2))/sqrt(x^2+y^2)
  return(z)
}

x<- seq(-10,10,length=30)
y<- x

z<- outer(x,y,f) # outer help this type of evaluation
persp(x,y,z,theta=30,phi=30,expand=0.5)

library(lattice)
cloud(maxO3~T12+Wx12,type=c("p","h"),data=ozone)


# Exporting Graph

# saving the graph as graphik.pdf 

pdf("graphik.pdf")
rangey <- range(ozone[1:7,"maxO3"],ozone[8:14,"maxO3"])
plot(ozone[1:7,"maxO3"],type="l",ylim=rangey,lty=1)
lines(ozone[8:14,"maxO3"],col="grey50",lty=1)
dev.off()

# pdf("mypath/graphik.pdf") to save for other destination

# Multiple graphs

# traditionlly we use par function

#par(mfrow=c(n,p)) oraganises n*p  graph as n rows and p col

par(mfrow=c(1,2))
plot(1:10,10:1,pch=0)
plot(rep(1,4),type="l")  # 1row and 2 column

# to come back 1 graph per window 
par(mfrow=c(1,1))

#  to get 3 graphs , in 2 rows first create a matrix

mat <- matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE)
layout(mat)  #  then layout func
plot(1:10,10:1,pch=0)
plot(rep(1,4),type="l")
plot(c(2,3,-1,0),type="b")


# Multiple Windows

plot(1:10,10:1,pch=0)
X11() #quartz in Mac OS
plot(rep(1,4),type='l')

# Improving and Personalising Graphs

#1######### To use color

plot(1:4,4:1,type="p",col="red")
# genrally 1 for "black", 2="red" , 3="green"

colors()     # gives vectors colors

# par funct manages all graphical parameter
par(fg="blue",bg="#f2a1c2")

#2 To modify palette (, ensuring correspondence b/w the num and color

palette(gray(seq(0,0.9,len=10))) #pallete of grey
plot(1:10,rep(1,10),type="p",pch=25,bg=1:10)
palette("default")

###3 To add Title

##4 to control the appearance of the axes or to del
## them (and  the corresponding legend)

plot(c(10,11),c(12,11),type="p",axes="FALSE",xlab="",ylab="")

# to fin redeffine them
axis(1,at=c(10,11),label=c("Dim1","Dim2"))


##5 To construct orthonormal axes

plot(1:4,4:1,type="p",asp=1)

##6 To add a legend

rangey <- range(ozone[1:7,"maxO3"])
plot(ozone[1:7,"maxO3"],type="l")
lines(ozone[8:14,"maxO3"],ylim=rangey,col="grey50")
legend("topleft",legend=c("week1","week2"),
       col=c("black","grey50"),lty=1)

##7 To insert symbols or mathematical formulae
help("plotmath")

plot(1,1,xlab=expression(bar(x)==sum(frac(x[i],n),i==1,n)))

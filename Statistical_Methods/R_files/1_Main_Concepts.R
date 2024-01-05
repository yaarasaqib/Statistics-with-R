### Help

help(mean) #or ?mean

help.start()  # to see in browser

### R objects

data(iris)

ozone <- read.table("ozone.txt",header="TRUE") # assigning table

x<- 5 # assign value
print(x) # print x
rm(x)  # rm x
x       # not found

rm(x,y,z)  # to remove multiple objects

rm(list=ls(pattern=".*a.*"))  #To remove a list of objects which 
                              # a part of name is common

### Type of object

mode(2)   # tells the mode of the object

x<-c(2,2)
# to check type of x

is.null(x)
is.logical(x)
is.numeric(x)
is.complex(x)
is.character(x)

# to convert x to diff type
as.null(x)
as.numeric(x)
as.character(x)


# attribute(object) to get dim names, class ,name

log(-2)  #NaN not a number
exp(1e10) # Inf infinity


#1.4.4 vectors

## Numeric Vectors

x <- c(5.6,-2,78,48.3)

x<- c(x,1)

seq(1,6,by=0.5) # sequence function

seq(1,6,length=3) # predefined length

rep(1,4) # construcion by rep function

rep(c(1,2),each=3)

marks<- scan(n=3)  #R ask to enter number one at a time


## character Vector

x <- rep('A',5)  # repeat A 5 times

paste("X",1:5,sep="-")   

paste(c("X","Y"),1:5,"txt",sep=".")

# argument collapse help to make it 1 vector
paste(c("X","Y"),1:5,"txt",sep=".",collapse ="+")


substr("freerider", start = 5,stop=9) # extract a part of string

#Logical Vectors

x<- c(-1,0,3)
x>2  # logical vctr

(1+x^2)*(x>0)  # prod of numeric and logical


any(x>1)  # to check any
all(x>1)  # to check all condition

2 %in% 1:3  # to check 2 lies in 1:3

# selecting a part of vector

# [] selection operator

v<- 1:100

v[6:8]
v[c(6,6,1:2)]
v[10:1]
v[(v<5)]
v<- 1:15
v[(v<5)&(v>12)]   # & means and
v[(v<5)|(v>12)]   #| means or

# selection in practice
x<- c(3,2,0,-1,-5,NA,5)
x[is.na(x)]<- 0  # NA replaced by 0

x[x<0]<- -x[x<0] # replace negative value by crspd +ve

y<- abs(x)   # absolute func

which.max(x)  # tells the index with max value
which.min(x)   # tells the index with min value

x==min(x)   #  test equality & yields boolean 

(1:length(x))[x==min(x)]  # position of min

########################################

## Matrices

matrix(1:8,ncol=2) # by default its arrange column wise

matrix(1:8,nrow=2)

matrix(1:8,ncol=2,byrow=TRUE) # arrange row wise

x<- 1:4
as.matrix(x)

# selecting a part of mat

mat <- matrix(1:9,nrow=3,ncol=3, byrow=TRUE)
mat[,3]    # third column
mat[-1,]   # without first row
mat[c(1,2),c(1,2)]  # submatrix with c(1,2) row and column

m<- matrix(1:8,ncol=4,byrow = T)
m[,m[1,]>2]
m[m>2]                 # yields a vector

### Calculation with matrices

A<- matrix(1:4,ncol=2,byrow=T)
B<- matrix(c(3,2,4,1),nrow= 2,byrow=T)
A
B
A*B  # multiply corresponding elts

sin(A)  # sinus elt by elts
exp(A)   # exponential elt by elts
A^4     # power 4 elts by elts

A%*%B    # matrix product
t(A)    # transpose of A

diag(3)         # diag matrix of order 3*3
diag(c(3,4,2))   # diag(vec)

det(A)           # det(A)  product of eigen values

eigen(A)$values  # eigen func gives eigen values and vec

eigen(A, only.values = T)  #use $ to extract the req


solve(A)  # matrix inversion

A%*%solve(A)  # gives identity

# solve(A,b) is used to get the solns of lin Sys
solve(matrix(c(2,2,1,3),ncol=2,byrow =T),c(18,17))


### Row and column operation

dim(A)     #dim of A
nrow(A)     # number of rows of A
ncol(A)      # number of rows of A

x<- 1:5
dim(x)   # if x is vec then dim is null

cbind(c(1,3),c(2,5))  # column bind

rbind(c(1,2),c(2,6))   # row bind


apply(A, MARGIN = 1,sum) # margin 1 for row
apply(A, 2, mean)        # margin 2 for colmns


### factor ####

# It is used to work with qualitative data

sex<- c("M","F","F","F","M","M")
sex<- factor(sex)

# factor using ordered function

ability<- ordered(c("Beginner","Champion", "Intermediate",
                   "Beginner","Champion"),levels=c("Beginner",
                                                   "Intermediate","Champion"))
f1<-as.factor(c(1:5,5:1))

levels(f1)  # number of levels
table(f1)   # use to make  table of factor

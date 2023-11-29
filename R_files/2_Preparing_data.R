## Preparing data

surname <- c("Tony","James","John")
height <- c(184,175,158)
weight <- c(80,78,72)
size <- c(9.5,8.5,8)
sex <- c("M","M","M")

my.data<- data.frame(surname,height,weight,size,sex)

write.csv(my.data,file="my.data.csv")

tab<- read.table("my.data.csv",sep=",",header=TRUE,
                 dec=".",row.names = 1)

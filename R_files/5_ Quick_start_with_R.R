### Quick start with R

# Sys.time() tells the time and date

now <-Sys.time()  # store the value
class(now)         # class of now [1] "POSIXct" "POSIXt" 
typeof(now)      # double

unclass(now)      # unclass the value


# NA values

mean(c(NA,1:50)) # gives mean NA
mean(c(NA,1:50),na.rm=T)  # gives mean except NA
mean(1:50)   # same

#NA==NA  # it also give NA
# c(1, 2, 3, NA) == NA  # Test wont work

is.na(NA)  # checkk this way
is.na(c(1, 2, 3, NA))  # gives a logical vector

### conditional statement

# if else
trunc(2.56)  # ignores decimal part

a <- 2.5
b <- 5.0

if (a==trunc(a)) {
  print("A wins!")
} else if (b== trunc(b)) {
  print("B wins!")
} else {
  print("Tie.")
}

## "




### Programming  Monte Carlo Simulation


#Objective to find the alpha significance Level

tstatistics = function(x,y){
  m= length(x)
  n=length(y)
  
  sp= sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
  t.stat = (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
  return(t.stat)
}



alpha = 0.1 ; m=10 ; n=10

N= 1e5
n.reject = 0
for( i in  1:N){
  x= rnorm(m, mean=0, sd=1)
  y= rnorm(n, mean=0, sd=1)
  t.stat = tstatistics(x,y)
  if(abs(t.stat)>qt(1-alpha/2,n+m-2))
    n.reject = n.reject+1
}

true.sig.level = n.reject /N

print(true.sig.level)
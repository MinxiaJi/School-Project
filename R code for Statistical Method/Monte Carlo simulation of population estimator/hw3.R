#HW3:Codes
#SDGB 7844: Statistical Methods and Computation I
#Minxia Ji

####################
#    Question 1    #
####################

#ramdomly capture 100 in the pool to get n1 and n2
n1 <- sample(1:5000,size=100,replace = FALSE)
n2 <- sample(1:5000,size=100,replace = FALSE)
#find out these in n2 also in n1
m2 <- n2[is.element(n2,n1)]
#calculate the length of m2
length(m2)
#calculate N.lp
N.lp <- (100*100)/2

####################
#    Question 2    #
####################
# set default value for N, n1, n2 and number of simulation runs
capture <- function(n.sides=5000,n1=100,n2=100,n.sim=1000){
# set m2
    m2 <- rep(0,n.sim)
  for(i in 1:n.sim){
   x <- sample(1:n.sides,size=n1,replace = FALSE)
   y <- sample(1:n.sides,size=n2,replace = FALSE)
   m2[i] <- length(y[is.element(y,x)])
 }
  result <- data.frame("m2"=m2,
                       "N.lp"=(n1*n2)/m2)
  
  return(list("result"=result,"N"=n.sides))
}
a <- capture(5000,100,100,1000)
hist(a$result$N.lp,las=TRUE,
     xlab = "Estimated N",main= "N.lp for 1000 simulations,size N=5000,n1=n2=100",
     border = "white",col = "pink")
abline(v=5000, col="firebrick", lty=2, lwd=2)
axis(side=1, at=5000, label="N=5000", col="firebrick", las=TRUE)
#dev.off

####################
#    Question 3    #
####################
#percent of the estimated population values in question 2 were in???nite
length(a$result$N.lp[a$result$N.lp=="Inf"])/1000

####################
#    Question 4    #
####################
#save reslults to M2
M2 <- a$result$m2
N.c <- ((100+1)^2)/(M2+1) - 1
hist(N.c,las=TRUE,
     main = "N.c for 1000 simulations,size N=5000,n1=n2=100",
     xlab = "Estimated N",border = "white", col = "deeppink2")
#indicate N
abline(v=5000, col="firebrick", lty=2, lwd=2)
axis(side=1, at=5000, label="N=5000", col="firebrick", las=TRUE)

####################
#    Question 5    #
####################

# initialize number of sumilation runs and population size
n.sim <- 1000
N <- 5000
# calculate the bias of the Lincoln-Peterson and Chapman estimators
# by mean
bias.Nc <- sum(N.c)/n.sim - N
bias.Nlp <- sum(N.lp)/n.sim -N

####################
#    Question 6    #
####################
#see solutions

####################
#    Question 7    #
####################
#constructs a function
largest.est <- function(N,n.sim,n){
  #initialize m2,N.c,bias and variance
  m2 <- rep(0,n.sim*length(n))
  N.c <- rep(0,n.sim*length(n))
  bias <- rep(0,length(n))
  variance <- rep(0,length(n))
  count <- 0
  #first for loop to loop the different different sample size
  for (i in 1:length(n)) {
    #second for loop to simulate n.sim times inside the loop for each sample size
    for (j in 1:n.sim) {
      count <- count+1
      x <- sample(1:N, size = n[i], replace = FALSE)
      y <- sample(1:N, size = n[i], replace = FALSE)
      m2[count] <- length(y[is.element(y,x)])
      N.c[count] <- (n[i]+1)*(n[i]+1)/(m2[count]+1)-1
    }
    bias[i] <- sum(N.c[(1+(i-1)*n.sim):(i*n.sim)])/n.sim-N
    variance[i] <- var(N.c[(1+(i-1)*n.sim):(i*n.sim)])
  }#end for
  #save data to the data.result
  data.result <- data.frame("n"=n, 
                            "bias of the Chapman estimator"=bias, 
                            "variance of the Chapman estimator"=variance)
  return(list("result"=data.result,"N"=N))
}
#set default to bias
bias <-largest.est(N=100000,n.sim = 1000,n=seq(from=100,to=5000,by=50))

#plot of bias versus n
plot(x = bias$result$n,
     y = bias$result$bias.of.the.Chapman.estimator,
     las = TRUE, main = "bias versus n", xlab = "n", ylab = "bias",
     pch = 20, type = "b",cex.main=2.2, cex.lab=1.8)
abline(h=0, col="firebrick", lty=1, lwd=1)
axis(side=2, at=0, label="bias=0", col="firebrick", las=TRUE)
#plost of variance versus n
plot(x = bias$result$n,
     y = bias$result$variance.of.the.Chapman.estimator,
     las = TRUE, main = "variance versus n", xlab = "n", ylab = "variance",
     pch = 20, type = "b",cex.main=2.2, cex.lab=1.8)
abline(h=0, col="firebrick", lty=2, lwd=2)
axis(side=2, at=0, col="firebrick", las=TRUE)
text(21.73636,297041071, labels="var=0",col = "firebrick")

save.image("HW3.RData")

####################
#    Question 8    #
####################
#increase sample size.Uses the function in Q7
result <- largest.est(N=100000,n.sim = 1,n=seq(from=100,to=10000,by=50))
#plot the bias and n 
plot(x = result$result$n, 
     y = result$result$bias.of.the.Chapman.estimator,
     las = TRUE, main = "bias and n", xlab = "n", ylab = "bias",
     pch = 20, type = "p")
abline(h=0, col="firebrick", lty=2, lwd=2)
axis(side=2, at=0, label="bias=0", col="red", las=TRUE)

####################
#    Question 9    #
####################
#see solutions
#SDGB7847
#Minxia Ji
#Machine Learning Homework1
library(MASS)
library(onion)
###############
# Question 1  #
###############
data.q1 <- read.table('data1.txt',header = TRUE,sep = '\t')
plot(data.q1$X,data.q1$Y)
#split the data into training and testing data
temp <- sample(1:nrow(data.q1),ceiling(nrow(data.q1))/2)
training <- data.q1[temp,]
testing <- data.q1[-temp,]
#save training and testing as matrics
x.training <- as.matrix(training[,1])
y.training <- as.matrix(training[,2])

x.testing <- as.matrix(testing[,1])
y.testing <- as.matrix(testing[,2])
#write the function
f <- function(x.train,y.train,x.test,y.test,d){
  a<-matrix(rep(1,nrow(x.train)),nrow(x.train),1)
  b<-matrix(rep(1,nrow(x.test)),nrow(x.test),1)
  SSE.train<-rep(NA,d)
  MSE.train<-rep(NA,d)
  SSE.test<-rep(NA,d)
  MSE.test<-rep(NA,d)
  B.container<-list(NULL)
  for (i in 1:d) {
    a<-cbind(a,x.train^i)
    b<-cbind(b,x.test^i)
    B<-matrix(nrow = ncol(a),ncol = 1)
    B<-ginv(t(a)%*%a)%*%(t(a)%*%y.train)
    #MSE of training data
    SSE.train[i]<-t(y.train-a%*%B)%*%(y.train-a%*%B)
    MSE.train[i]<-SSE.train[i]/(nrow(a)-ncol(a))
    #MSE of testing data
    SSE.test[i]<-t(y.test-b%*%B)%*%(y.test-b%*%B)
    MSE.test[i]<-SSE.test[i]/(nrow(b)-ncol(b))
    #list of beta
    B.container[[i]] <-B
  }
  return(list("MSE.training"=MSE.train,"MSE.testing"=MSE.test,
              "Beta"=B.container))
}
#save result to save.1
save.1 <- f(x.training,y.training,x.testing,y.testing,1000)

which.min(save.1$MSE.training)
which.min(save.1$MSE.testing)

# build X varibales matrix
X <- matrix(rep(1,nrow(data.1)),nrow(data.1),1)
for (i in 1:7) {
  X <- cbind(X,as.matrix((data.1[,"X"])^i))
}
beta <- result.1$Beta[[7]]
Y.predict <- as.vector( X %*% beta )
r.predict <- data.frame(data.1$X,Y.predict)
#plot MSE
plot(y=result.1$MSE.training,x=1:10,las = TRUE,type = "l",
     main = "MSE versus d", xlab = "polynomial order (d)",ylab = "MSE",
     col="black",lwd =2,lty=2, cex.main = 2)
lines(result.1$MSE.testing,col = "royalblue",lwd =2)
legend("topright",legend = c("TRAINING MSE","TESTING MSE"),
       fill=c("black", "royalblue"), cex=1.4, bty="n")
#scatterplot
plot(y = data.q1$Y, x = data.q1$X, las = TRUE, cex.main = 2,
     main = "Scatterplot for data",xlab = "X",ylab = "Y")
lines(x=r.predict$data.q1.X,y=r.predict$Y.predict, col="royalblue",lwd=3)
legend("topright",legend = "Regression Line",
       fill="royalblue", cex=1.4, bty="n")
###############
# Question 2  #
###############
data.q2 <- read.csv("q2.txt",header = TRUE,sep = "\t")
temp2 <- sample(1:nrow(data.q2),nrow(data.q2)/2)
training.2 <- data.q2[temp2,]
testing.2 <- data.q2[-temp2,]

x.training.2 <- as.matrix(training.2[,-1])
y.training.2 <- as.matrix(training.2[,1])

x.testing.2 <- as.matrix(testing.2[,-1])
y.testing.2 <- as.matrix(testing.2[,1])

f.2 <- function(x.train,y.train,x.test,y.test,lambda){
  a <- matrix(rep(1,nrow(x.train)),nrow(x.train),1)
  a <- cbind(a,x.train) 
  b <- matrix(rep(1,nrow(x.test)),nrow(x.test),1)
  b <- cbind(b,x.test)
  I <- diag(rep(1,ncol(a)))
  B <- matrix(rep(NA,ncol(a)),ncol(a),1)
  SSE.train <- rep(NA,lambda)
  SSE.test <- rep(NA,lambda)
  MSE.train <- rep(NA,lambda)
  MSE.test <- rep(NA,lambda)
  for (i in 0:lambda) {
    B <- ginv((t(a)%*%a+i*I))%*%(t(a)%*%y.train)
    SSE.train[i]<-t(y.train-a%*%B)%*%(y.train-a%*%B)
    SSE.test[i]<-t(y.test-b%*%B)%*%(y.test-b%*%B)
    MSE.train[i]<-t(y.train-a%*%B)%*%(y.train-a%*%B)/(nrow(b)-ncol(b))
    MSE.test[i]<-t(y.test-b%*%B)%*%(y.test-b%*%B)/(nrow(b)-ncol(b))
  }
  return(list("errorfortraining"=MSE.train,"errorfortesting"=MSE.test))
}
#error plots
save.2 <- f.2(x.training.2,y.training.2,x.testing.2,y.testing.2,1000)
plot(save.2$errorfortraining,las = TRUE,cex.lab = 1.6 ,cex.main =2.2,type = "l", col = "royalblue",
     xlab = "lambda",ylab = "MSE",main = "Error for training")
plot(save.2$errorfortesting, type = "l",xlab = "Lambda",ylab = "MSE",main = "Testing MSE versus lambda",
     cex.lab = 1.2, col = "royalblue") 
abline(v=which.min(result.ridge$Testing.SSE),col = "black",lty = 2)
arrows(x0=788.1744, y0=306.201, x1=590.6369, y1=251.0786, length=0.1, lwd=1.8)
text(700, 330.201,cex = 1.1, labels="When lambda = 595, minimize MSE")

###############
# Question 4  #
###############
data.q4 <- read.csv("q4.txt",header = TRUE,sep = "\t")
#spliting data
temp4 <- sample(1:nrow(data.q4),nrow(data.q4)/2)
training.4 <- data.q4[temp4,]
testing.4 <- data.q4[-temp4,]

x.training.4 <- as.matrix(training.4[,-4])
y.training.4 <- as.matrix(training.4[,4])

x.testing.4 <- as.matrix(testing.4[,-4])
y.testing.4 <- as.matrix(testing.4[,4])

f.4<-function(x.train,y.train,x.test,y.test,tolerance,stepsize){
  #initialize theta
  theta <- matrix(rep(1,ncol(x.training.4)),nrow=ncol(x.training.4),1)
  #initialize error and index
  error <- 10
  index <- 0
  while (error>tolerance) {
    temporary <- theta - stepsize*(t(x.train)%*%(1/(1+exp(-x.train%*%theta))-y.train))
    error <- as.numeric(sqrt(t(theta-temporary)%*%(theta-temporary)))
    theta <- temporary
    index <- index+1
  }
  return(list("iterationtimes"=index,"theta"=theta))
}

plot(x=save.4,y=tolerance,main = "when steosize = 0.5")
plot(x=data.q4$X1,y=data.q4$X2,xlab = "x1",ylab = "x2")
lines(x=data.q4$X1,y=(37.13-98.33*data.q4$X1)/51.14,col="royalblue")
legend("bottomleft",legend = "decision boundary",fill = "royalblue",bty = "n")
#
#for loop
tolerance.4<-seq(0.1,0.01,by=-0.0001)
result.4<-rep(NA,length(tolerance.4))
theta.container<-list(NULL)
index.4<-1
for (i in tolerance.4) {
  result.4[index.4]<-save.4<-f.4(x.training.4,y.training.4,x.testing.4,
                                   y.testing.4,stepsize = 0.1,tolerance = 0.01)$iterationtimes
  
  theta.container[index.4]<-f.4(x.training.4,y.training.4,x.testing.4,
                                 y.testing.4,stepsize = 0.1,tolerance = 0.01)$theta
  
  index.4 <- index.4 +1
  
}

plot(x=result.4,y=tolerance.4,las = TRUE,cex.lab = 1.6 ,cex.main =2.2,
     type = "l", col = "royalblue",xlab = "iterations", ylab = "tolerance",
     main = "Rate of Convergence")
###############
# Question 5  #
###############
data.q4 <- read.csv("q4.txt",header = TRUE,sep = "\t")

f.5<-function(x,y,stepsize,tolerance){
  theta <- matrix(rep(1,ncol(x)),nrow=ncol(x),1)
  g <- t(x)%*%(1/(1+exp(-x%*%theta))-y)
  theta2 <- -stepsize*g
  index <- 0
  while(Mod.onion(theta2)>tolerance){
    g <- t(x)%*%(1/(1+exp(-x%*%theta))-y)
    h <- ginv(t(x)%*%diag(as.vector(1/(1+exp(-x%*%theta))*(1-1/(1+exp(-x%*%theta)))))%*%x)
    temp <- theta - stepsize*h%*%g
    theta2 <- -stepsize*g
    theta <- temp
    index <- index +1
    
  }
return(list("iterationtimes"=index,"theta"=theta))
}
#for loop
tolerance.nt<-seq(0.1,0.01,by=-0.0001)
result.nt<-rep(NA,length(tolerance.nt))
theta.container<-list(NULL)
index.nt<-1
for (i in tolerance.nt) {
  result.nt[index.nt]<-f.5(x.training.4,y.training.4,0.5,i)$interationtimes
  
  theta.container[index.nt]<-f.5(x.training.4,y.training.4,0.5,i)$theta
  
  index.nt <- index.nt +1
  
}

plot(x=result.nt,y=tolerance.nt,las = TRUE,cex.lab = 1.6 ,cex.main =2.2,
     type = "l", col = "royalblue",xlab = "iterations", ylab = "tolerance",
     main = "Rate of Convergence")
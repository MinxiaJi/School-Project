###############
# Question 2  #
###############
#class 1 
class1 <- iris[which(iris[,"Species"] == "setosa"),]
class1$Species <- 1

#class 2
class2 <- iris[which(iris[,"Species"] == "versicolor"),]
class2$Species <- -1

#bind 2 classes
flower <- rbind(class1,class2)

#spliting into training and testing data
temp <- sample(1:nrow(flower),ceiling(nrow(flower))/2)
train <- flower[temp,]
test <- flower[-temp,]


#x and y
X <- train[,-5]
Y <- train[,5]


#D d A b0
SVM.solution <- function(x,y){
  
  require(quadprog)
  #size and features
  m <- nrow(x)
  n <- ncol(x)
  
  Dmat <- diag(c(0.00001,rep(1,n)))
  dvec <- c(0,rep(0,n))
  A <- as.matrix(cbind(as.matrix(y),x*as.matrix(y)[,rep(1,n)]))
  bvec <- c(rep(1,m))
  Amat <- t(A)
  
  sol <- solve.QP(Dmat,dvec,Amat,bvec,meq=0 )
}

result <- SVM.solution(X,Y)
#compute theta
theta <- result$solution
theta.0 <- theta[1]
theta.1234 <- theta[-1]

#X for test data
X.test <- test[,-5]
Y.test <- test[,5]
#add a prediction coloumn to test data
prediction <- as.matrix(X.test)%*%theta.1234 + theta.0
test[,"prediction"] <- prediction

#add result 1 or -1 to prediction coloumn
for (i in 1:nrow(test)){
  
  if (prediction[i] >= 0){
    test[i,"prediction"] <- 1
  }else{
    test[i,"prediction"] <- -1
  }
  
}

#check difference between real and predictions
setdiff(test$Species,test$prediction)  

###############
# Question 3  #
###############
#use function in question 2 to compute theta
plot1 <- SVM.solution(X[,c(1,2,3)],Y)
plot2 <- SVM.solution(X[,c(1,3,4)],Y)
plot3 <- SVM.solution(X[,c(2,3,4)],Y)
plot4 <- SVM.solution(X[,c(1,2,4)],Y)
#3d 4 plots
require(rgl)
#colors setting
color.setting <- rep(NA,nrow(test))
color.setting [which(test[,"Species"] == 1)] <- "hotpink"
color.setting [which(test[,"Species"] == -1)] <- "mistyrose"

#plot1
plot3d(X.test$Sepal.Length, X.test$Sepal.Width, X.test$Petal.Length, 
       col = color.setting,type ='s',alpha = .5,
       xlab = "sepal length",ylab = "sepal width",zlab = "petal length")

planes3d(plot1$solution[2], plot1$solution[3], 
         plot1$solution[4], plot1$solution[1], alpha = 0.5)

#plot2
plot3d(X.test$Sepal.Length, X.test$Petal.Length, X.test$Petal.Width, 
       col = color.setting,type ='s',alpha = .5,
       xlab = "sepal length",ylab = "petal length",zlab = "petal width")

planes3d(plot2$solution[2], plot2$solution[3], 
         plot2$solution[4], plot2$solution[1], alpha = 0.5)

#plot3
plot3d(X.test$Sepal.Width, X.test$Petal.Length, X.test$Petal.Width, 
       col = color.setting,type ='s',alpha = .5,
       xlab = "sepal width",ylab = "petal length",zlab = "petal width")

planes3d(plot3$solution[2], plot3$solution[3], 
         plot3$solution[4], plot3$solution[1], alpha = 0.5)

#plot4
plot3d(X.test$Sepal.Length, X.test$Sepal.Width, X.test$Petal.Width, 
       col = color.setting,type ='s',alpha = .5,
       xlab = "sepal length",ylab = "sepal width",zlab = "petal width")

planes3d(plot4$solution[2], plot4$solution[3], 
         plot4$solution[4], plot4$solution[1], alpha = 0.5)


###################
# Question Bouns  #
###################

gaussian.kernel <- function(x,sig=1){
  
  k <- matrix(nrow=nrow(x),ncol=nrow(x))
  
  for(i in 1:nrow(x)){
    
    for(j in 1:nrow(x)){
      k[i,j] <- exp(-(dist(rbind(x[i,],x[j,])))^2/(sig^2))
    }
  }
  return(k)
}
#function for polynomial kernel
polynomial.kernel <- function(x,degree=3,gamma=1){
  
  k <- matrix(nrow=nrow(x),ncol=nrow(x))
  
  for(i in 1:nrow(x)){
    
    for(j in 1:nrow(x)){
      k[i,j] <- (gamma*as.matrix(X[i,])%*%t(as.matrix(X[j,])))^degree
    }
  }
  return(k)
}
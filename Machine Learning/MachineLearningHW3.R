###############
# Question 1  #
###############
#load data
coins <- read.csv("data.csv",header = FALSE, stringsAsFactors = FALSE)
split.temp <- sample(1:nrow(coins),ceiling(nrow(coins))/2)
training <- coins[-split.temp,]
testing <- coins[split.temp,]
#Y
Ytraining <- as.numeric(apply(training,1,sum))
Ytesting <- as.numeric(apply(testing,1,sum))
#get n
ntraining <- rep(50,nrow(training))
ntesting <- rep(50,nrow(testing))

# E-step

# probability components for indicator variables
probs <- function(i,m,A,Q,n,Y){choose(n[i],Y[i]) * Q[m]^Y[i] * (1 - Q[m])^(n[i]-Y[i]) * A[m]}

# chaining across columns
doCol <- function(m,A,Q,n,Y){sapply(1:length(Y),function(i){probs(i,m,A,Q,n,Y)})}

# generate the expected value of the 'hidden' coin used in each experiment
mu.update <- function(A,Q,n,Y){
  unnorm <- sapply(1:length(A),function(i){doCol(i,A,Q,n,Y)})
  norms <- apply(unnorm,1,sum)
  mu <- unnorm / norms        # Expected value of the indicator variables
  mu 
}

# M-step

# update mixture fractions
A.update <- function(mu,N){apply(mu,2,sum)/N}

# update mixture distribution parameters
Q.update <- function(mu,n,Y){apply(mu * Y,2,sum)/apply(mu * n,2,sum)}

# log-likelihood
L <- function(mu,A,Q,Y){
  ll <- 0
  for(i in 1:nrow(mu)){
    for(j in 1:ncol(mu)){
      ll <- ll + mu[i,j]*(log(A[j]) + Y[i]*log(Q[j]) + (50-Y[i])*log(1-Q[j]))
    }
  }
  return(ll)
}

# EM - A and Q are the initial mixture fractions and mixture parameters respectively
doEM <- function(epsilon,A,Q,n,Y){
  error <- 10
  mu <- mu.update(A,Q,n,Y)
  A <- A.update(mu,length(Y))
  Q <- Q.update(mu,n,Y)
  while(error > epsilon){
    ll.old <- L(mu,A,Q,Y)
    mu <- mu.update(A,Q,n,Y)
    A <- A.update(mu,length(Y))
    Q <- Q.update(mu,n,Y)
    ll.new <- L(mu,A,Q,Y)
    error <- abs(ll.new - ll.old)
  }
  list("mu"=mu, "A"=A, "Q"=Q)
}

L.training <- matrix(nrow = 6,ncol = 5)
L.testing <- matrix(nrow = 6,ncol = 5)

A <- list()
Q <- list()
count <- 1
for(i in 1:6){
  for(k in 1:5){
    J <- k                                          # Number of coins _suspected_ to be in bag
    Q0 <- runif(J,0,1)                          # Randomised corresponding starting probabilities for each coin
    A0 <- rep(1/J,J)                                # Initial expectation of coin proportions
    
    # Run EM
    EM.result <- doEM(0.01,A0,Q0,ntraining,Ytraining)
    A[[count]] <- EM.result$A
    Q[[count]] <- EM.result$Q
    L.training[i,k] <- L(EM.result$mu, EM.result$A, EM.result$Q, Ytraining)
    L.testing[i,k] <- L(EM.result$mu, EM.result$A, EM.result$Q, Ytesting)
    count <- count+1
  }
}

mean.training <- apply(L.training,2,mean)
sd.training <- apply(L.training,2,sd)
mean.testing <- apply(L.testing,2,mean)
sd.testing <- apply(L.testing,2,sd)

par(mfrow=c(1,2))
plot(x=1:5,y=mean.training,type = "b",xlab = "k",ylab = "loglikihood",main = "Training",cex = 1.2)
plot(x=1:5,y=mean.testing,type = "b",xlab = "k",ylab = "loglikihood",main = "Testing",cex = 1.2)

L.training <- rbind(L.training,mean.training,sd.training)
L.training <- as.data.frame(L.training)
rownames(L.training) <- c("1st iteration","2nd iteration","3rd iteration",
                        "4th iteration","5th iteration","6th iteration","mean","standard deviation")
colnames(L.training) <- c("K=1","K=2","K=3","K=4","K=5")

L.testing <- rbind(L.testing,mean.testing,sd.testing)
L.testing <- as.data.frame(L.testing)
rownames(L.testing) <- c("1st iteration","2nd iteration","3rd iteration",
                       "4th iteration","5th iteration","6th iteration","mean","standard deviation")
colnames(L.testing) <- c("K=1","K=2","K=3","K=4","K=5")

J <- 1                                # Number of coins in bag
Q0 <- runif(J,0,1)                    # Randomised corresponding starting probabilities for each coin
A0 <- rep(1/J,J)                      # Initial expectation of coin proportions

# Run EM
EM.result <- doEM(0.01,A0,Q0,ntraining,Ytraining)
EM.result$Q
    
###############
# Question 3  #
###############
library (jpeg) 
require(ggplot2)
#plot original picture of cat
ggplot(data = cat.RGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(cat.RGB[c("R", "G", "B")])) +
  labs(title = "Original picture of the cat") +
  xlab("x") +
  ylab("y")
#Get dimensions
img <-readJPEG ("cat.jpg")
catdm <- dim(img)

#Get RGB
cat.RGB <- data.frame(
  x = rep(1:catdm[2],  each =catdm[1]),
  y = rep(catdm[1]:1, catdm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

# k-means model function
# k: number of clusterings
# X: Input the RGB values of a image
# max.time: maximum times of iterations
# or we can set epsilon between seed and means for each cluster
# but set max iteration times to interupt the function would be more convenient
kMM <- function(k,X,max.time) {
  X <- as.matrix(X)
  n <- nrow(X)
  #randomly initialize K centers
  means <- matrix(runif(k*3),nrow = k, ncol = 3)
  #seed:store temp values
  seed <- matrix(runif(k*3),nrow = k, ncol = 3)
  #creat a temp to store the current min distance of each points to the centers
  temp <- rep(NA,n)
  count <- 0
  while(!all((seed-means)==0) & count < max.time){
    seed <- means
    #find the closest centroid for each pixels
    for(i in 1:n){
      temp[i] <- which.min(diag((X[i,]-seed)%*%t(X[i,]-seed)))
    }
    #compute new centroid means
    for(j in 1:k){
      means[j,]<-colMeans(X[which(temp==j),])
    }
    count <- count + 1
    if(count%%10 ==0 | count == max.time){
      print(paste("iteration", count,"th"))
    }
  }
  X.new <- matrix(nrow = n,ncol = 3)
  #assign new centroid means to each pixels
  for(i in 1:n){
    X.new[i,] <- means[temp[i],]
  }
  return(X.new)
}

#plot new pictures of cat

# run function when k = 3
result.k3 <- kMM(3,cat.RGB[,3:5],150)
result.data <- as.data.frame(result.k3)
colnames(result.data) <- c("R","G","B")
imgRGB <- data.frame(cat.RGB[,1:2],result.data)

# plot picture when k = 3
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "K = 3: cat") +
  xlab("x") +
  ylab("y")


# run function when k = 5
result.k5 <- kMM(5,cat.RGB[,3:5],150)
result.data <- as.data.frame(result.k5)
colnames(result.data) <- c("R","G","B")
imgRGB <- data.frame(cat.RGB[,1:2],result.data)

# plot picture when k = 5
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "K = 5: cat") +
  xlab("x") +
  ylab("y")
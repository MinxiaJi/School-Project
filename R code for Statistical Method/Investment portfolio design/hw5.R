#HW5:Codes
#SDGB 7844: Statistical Methods and Computation I
#Minxia Ji

###############
# Question 1  #
###############
#load the data
data.x <- read.table("asset_data.txt",header = TRUE,sep = ",",stringsAsFactors = FALSE)
# turn the ?rst column from a character string to a date
data.x$date <- as.Date(data.x$date, format="%Y-%m-%d")
#remove NA
data.x<-subset(data.x,!is.na(data.x$fed.rate))
#first and last data of this reduced data
fist <- data.x$date[1]
last <- data.x$date[nrow(data.x)]
#graph by time series
plot(data.x$date,data.x$fed.rate,
     main = "the federal funds interest rate as a time series",
     xlab = "date", ylab = "federal funds interest rate",type ="l")

###############
# Question 2  #
###############
data.training <- subset(data.x,data.x$date<"2014-1-1")
data.testing <- subset(data.x,data.x$date>"2014-1-1")
#how many observations in two sets
nrow(data.training)
nrow(data.testing)

###############
# Question 3  #
###############
#compute 2 returns use the formula
data.training$fed.rate <- (data.training$fed.rate)/100
#initialize rt.sp and rt.tlt
rt.sp <- rep(NA,length(data.training$close.spy))
rt.tlt <- rep(NA,length(data.training$close.tlt))
for (i in 2:length(data.training$close.spy)) {
  rt.sp[i] <- (data.training$close.spy[i]-data.training$close.spy[i-1])/data.training$close.spy[i-1]
  rt.tlt[i] <- (data.training$close.tlt[i]-data.training$close.tlt[i-1])/data.training$close.tlt[i-1]
}#end for
#add 2 returns to the traning data
data.training <- data.frame(data.training,rt.sp,rt.tlt)
#plots
#rt.sp without ylim
par(mfrow=c(2,1))
plot(data.training$date,data.training$rt.sp,
     main = "Return rates of S&P as a time series",
     xlab = "date",ylab = "return rates",
     ylim = c(min(data.training$rt.sp,na.rm = TRUE),max(data.training$rt.sp,na.rm = TRUE)),
     type="l",pch=20)
abline(h=0,col="firebrick",lwd=2,lty=2)
#rt.tlt
plot(data.training$date,data.training$rt.tlt,
     main = "Return rate of tlt as a time series",
     xlab = "date",ylab = "asset price",
     ylim = c(min(data.training$rt.sp,na.rm = TRUE),max(data.training$rt.sp,na.rm = TRUE)),
     type="l",pch=20)
abline(h=0,col="firebrick",lwd=2,lty=2)

###############
# Question 4  #
###############
#qq plot of return
qqnorm(data.training$rt.sp,main = "Normal Q-Q plot of s&p return",
       xlab = "theoretical quantiles",ylab = "sample quantiles",
       plot.it = TRUE)
qqline(data.training$rt.sp,distribution = qnorm,qtype = 7)
#qq plot of rt.tlt
qqnorm(data.training$rt.tlt,main = "Normal Q-Q plot of tlt return",
       xlab = "theoretical quantiles",ylab = "sample quantiles",
       plot.it = TRUE )
qqline(data.training$rt.tlt,distribution = qnorm,qtype = 7)

###############
# Question 5  #
###############
#compute cor without na
cor(data.training$rt.sp[-1],data.training$rt.tlt[-1])
#rolling
#compute how many times of rolling
rolling.times<-length(data.training$rt.sp)-24
cor.rolling <- rep(NA,rolling.times)
for(i in 1:rolling.times){
  cor.rolling[i] <- cor(data.training$rt.sp[(i+1):(i+24)],
                        data.training$rt.tlt[(i+1):(i+24)])
}#end for 
plot(data.training$date[25:570],cor.rolling,
     main = "rolling window correlations as a time serious",
     xlab = "date(last day)",ylab = "rolling correlations",pch=20)
abline(h=0,col="gray",lty=2,lwd=2)

###############
# Question 6  #
###############
#step 0
yt <- data.training$fed.rate
#step1
et.sp <- rep(NA,nrow(data.training))
et.tlt <- rep(NA,nrow(data.training))
for (i in 2:length(et.sp)) {
  et.sp[i] <- rt.sp[i]-(yt[i-1]/52)
  et.tlt[i] <- rt.tlt[i]-(yt[i-1]/52)
}#end for

#step2
gt.sp<-rep(NA,nrow(data.training))
gt.tlt<-rep(NA,nrow(data.training))
for (i in 2:length(gt.sp)){
  gt.sp[1]<-100
  gt.tlt[1]<-100
  gt.sp[i] <- (gt.sp[i-1])*(1+et.sp[i])
  gt.tlt[i] <- (gt.tlt[i-1])*(1+et.tlt[i])
}#end for
#step 3
n <- (nrow(data.training)-1)/52
#step 4
CAGR.sp <- (gt.sp[nrow(data.training)]/gt.sp[1])^(1/n)-1
CAGR.tlt <- (gt.tlt[nrow(data.training)]/gt.tlt[1])^(1/n)-1
#step 5
v.sp <- ((52)^(1/2))*sd(et.sp,na.rm = TRUE)
v.tlt <- ((52)^(1/2))*sd(et.tlt,na.rm = TRUE)
#step 6
SR.sp <- CAGR.sp/v.sp
SR.tlt <- CAGR.tlt/v.tlt

###############
# Question 7  #
###############
portfolio <- function(x,rt.sp=data.training$rt.sp,rt.tlt=data.training$rt.tlt,
         fr=data.training$fed.rate){
  for (j in 1:length(x)) {
    rt.portfolio <-x[j]*rt.sp+(1-x[j])*rt.tlt
  #step 0
  yt <- data.training$fed.rate
  #step1
  et.portfolio <- rep(NA,length(rt.sp))
  for (i in 2:length(rt.sp)) {
    et.portfolio[i] <- rt.portfolio[i]-(yt[i-1]/52)
  }#end for
  
  #step2
  gt.portfolio<-rep(NA,length(rt.sp))
  for (i in 2:length(rt.sp)){
    gt.portfolio[1]<-100
    gt.portfolio[i] <- (gt.portfolio[i-1])*(1+et.portfolio[i])
  }#end for
  #step 3
  n <- (length(rt.sp)-1)/52
  #step 4
  CAGR[j] <- (gt.portfolio[length(rt.sp)]/gt.portfolio[1])^(1/n)-1
  #step 5
  v[j] <- ((52)^(1/2))*sd(et.portfolio,na.rm = TRUE)
  #step 6
  SR[j]<- CAGR[j]/v[j]

  }#end for
  
  return(SR)
}#end function

curve(portfolio,from = 0,to = 1,main = "sharpe rate vs. weights",
      xlab = "weight", ylab = "sharpe ratio",type = "l")

###############
# Question 8  #
###############

optimize(portfolio,c(0,1),maximum = TRUE)

###############
# Question 9  #
###############
data.testing$fed.rate <- (data.testing$fed.rate)/100
r.sp <- rep(NA,nrow(data.testing))
r.tlt <- rep(NA,nrow(data.testing))
for (i in 2:length(r.sp)) {
  r.sp[i] <- (data.testing$close.spy[i]-data.testing$close.spy[i-1])/data.testing$close.spy[i-1]
  r.tlt[i] <- (data.testing$close.tlt[i]-data.testing$close.tlt[i-1])/data.testing$close.tlt[i-1]
}#end for
data.testing <- data.frame(data.testing,r.sp,r.tlt)

#compute index
#step 0
y.f <- data.testing$fed.rate
g.sp<-rep(NA,nrow(data.testing))
g.tlt<-rep(NA,nrow(data.testing))
opt<-optimize(portfolio,c(0,1),maximum = TRUE)
r.both <-opt$maximum*r.sp+(1-opt$maximum)*r.tlt
#step1
e.sp <- rep(NA,nrow(data.testing))
e.tlt <- rep(NA,nrow(data.testing))
e.both <- rep(NA,nrow(data.testing))
for (i in 2:nrow(data.testing)) {
  e.sp[i] <- r.sp[i]-(y.f[i-1]/52)
  e.tlt[i] <- r.tlt[i]-(y.f[i-1]/52)
  e.both[i] <- r.both[i]-(y.f[i-1]/52)
}#end  for

#step2
g.sp<-rep(NA,nrow(data.testing))
g.tlt<-rep(NA,nrow(data.testing))
g.both <-rep(NA,nrow(data.testing))
for (i in 2:nrow(data.testing)){
  g.sp[1]<-100
  g.tlt[1]<-100
  g.both[1]<-100
  g.sp[i] <- (g.sp[i-1])*(1+e.sp[i])
  g.tlt[i] <- (g.tlt[i-1])*(1+e.tlt[i])
  g.both[i] <- (g.both[i-1])*(1+e.both[i])
 }#end  for
plot(data.testing$date,g.sp,
     main = "the return rate of S&P as a time series",
     xlab = "date", ylab = "S&P return rate",type ="l",
     ylim = c(min(g.sp,g.tlt,g.both),max(g.sp,g.tlt,g.both)))
lines(data.testing$date,g.tlt, col = "firebrick")
lines(data.testing$date,g.both,col = "blue")
abline(h=100,col="gray",lty=2,lwd=2)
legend("topleft",legend = c("long term treasure bonds","combined portfolio","S&P500"),
       fill = c("firebrick","blue","black"),bty = "n")
################
# Question 10  #
################
#compute excess returns index for three investments in test data
g.sp[nrow(data.testing)]
g.tlt[nrow(data.testing)]
g.both[nrow(data.testing)]

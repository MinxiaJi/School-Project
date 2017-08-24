# SDGB 7840
# Minxia Ji

#load data

sport <- read.csv("Sporting.csv",header = TRUE)

###############
# Question 2  #
###############
#draw histograms to compare skeness and outliers
par(mfrow=c(2,3))
hist(sport$Sales,main = "last one month sale total",
     xlab = "sales($)",ylab = "number of stores")
hist(sport$Age,main = "median age of customer base",
     xlab = "age(years)",ylab = "number of stores")
hist(sport$Growth,main = "annual population growth rate",
     xlab = "growth rate(%)",ylab = "number of stores")
hist(sport$Income,main = "median family income of cutomer base",
     xlab = "income($)",ylab = "number of stores")
hist(sport$HS,main = "percentage of customer base with a high school diploma",
     xlab = "percentage",ylab = "number of stores")
hist(sport$College,main = "percentage of customer base with a college diploma",
     xlab = "percentage",ylab = "number of stores")
#calculate summary data way 1
summary(data.x$Sales)
summary(data.x$Age)
summary(data.x$Growth)
summary(data.x$Income)
summary(data.x$HS)
summary(data.x$College)
#calculate summary data way 2
apply(sport,2,mean)
apply(sport,2,median)
apply(sport,2,sd)
apply(sport,2,min)
apply(sport,2,max)

###############
# Question 3  #
###############
#compute correlations
cor(sport$Age,sport$Sales)
cor(sport$Growth,sport$Sales)
cor(sport$Income,sport$Sales)
cor(sport$HS,sport$Sales)
cor(sport$College,sport$Sales)
#scatter plot
par(mfrow=c(2,3))
plot(sport$Age,sport$Sales,main = "sales vs age",xlab = "age(years)",ylab = "sales($)")
plot(sport$Growth,sport$Sales,main = "sales vs growth",xlab = "growth(%)",ylab = "sales($)")
plot(sport$Income,sport$Sales,main = "sales vs income",xlab = "income($)",ylab = "sales($)")
plot(sport$HS,sport$Sales,main = "sales vs HS",xlab = "percentage",ylab = "sales($)")
plot(sport$College,sport$Sales,main = "sales vs college",xlab = "percentage",ylab = "sales($)")

###############
# Question 5  #
###############
#fit a linear model 
lm.sport<-lm(Sales~HS,data = sport)
summary(lm.sport)


###############
# Question 6  #
###############
#write a estimate regression line
plot(sport$HS,sport$Sales,las=TRUE,pch=19,
     main = "Sales vs HS",
     xlab = " Percentage of customer base with a high school diploma % ",
     ylab = "latest one-month sales total in $",
     cex.axis=0.5,cex.lab=1.5)
abline(coef(lm.sport), col="forestgreen", lwd=3)

###############
# Question 7  #
###############
#compute the interpretation meaningful x range
range(sport$HS)

###############
# Question 10 #
###############
#esidual plot
plot(y = lm.sport$residuals, x = sport$HS,  
     main="Residuals Plot", xlab = "Percentage of customer base with a high school diploma (%)", 
     ylab = "Residuals")
abline(h=0, lty=2, lwd=2)
#normal quantile plot
qqnorm(lm.sport$residuals, las=TRUE, cex.lab=1, cex.axis=0.8, cex.main=1.8, pch=19, col="#FF824770", 
       main="Normal Q-Q Plot of Residuals")
qqline(lm.sport$residuals) 

###############
# Question 12 #
###############
summary(lm.sport) 
# test statistic for null hypothesis: H_0: beta_1 = 0
# p-value computed using alternative hypothesis: H_1: beta_1 != 0

nrow(sport)

# manually computing p-value:
2*pt(lm.sport$coef[2]/summary(lm.sport)$coefficients[2,2], df=nrow(sport)-2, lower.tail=FALSE)


#######

# we want to test the following hypotheses:
# H_0: beta_1 = 1
# H_1: beta_1 < 1

# test statistic = (hat(beta)_1 - 1)/se for hat(beta)_1
test.stat <- (lm.sport$coef[2] - 0)/summary(lm.sport)$coefficients[2,2]

# p-value: one-sided test, < alternative
# t distribution with n-2 degrees of freedom
pt(test.stat, df=nrow(sport)-2, lower.tail=TRUE)

###############
# Question 13 #
###############
#predict county A,B,C

data.new <- data.frame("HS"=c(50,75,70))
predict(lm.sport,newdata = data.new)

###############
# Question 14 #
###############

pred.interval <- predict(lm.sport, newdata=data.new, interval="prediction", level=0.95)
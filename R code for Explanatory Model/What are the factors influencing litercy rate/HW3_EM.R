###############
# Question 1  #
###############
dating <- read.csv("SpeedDating.csv",header = TRUE,stringsAsFactors = FALSE)
#data to fill the table
both.want <- length(which(dating$DecisionM == dating$DecisionF & dating$DecisionM == 1))
both.not <- length(which(dating$DecisionM == dating$DecisionF & dating$DecisionM == 0))
temp <- dating[-which(dating$DecisionM == dating$DecisionF),]
F1M0 <- length(which(temp$DecisionF == 1))
F0M1 <- length(which(temp$DecisionM == 1))
#calculate the percentage
both.want.percent <- nrow(dating[which(dating$DecisionM == dating$DecisionF & dating$DecisionM == 1),])/nrow(dating)

###############
# Question 2  #
###############
#Add a new column to the dataset
sec.date <- rep(0,nrow(dating))
dating <- data.frame(dating, sec.date)
dating$sec.date[which(dating$DecisionM == dating$DecisionF & dating$DecisionM == 1)] <-1
#pchs settings
pchs <- rep(NA,nrow(dating))
pchs[which(dating[,"sec.date"] == 1)] <- 19
pchs[which(dating[,"sec.date"] == 0)] <- 4
#colors setting
color.setting <- rep(NA,nrow(dating))
color.setting [which(dating[,"sec.date"] == 1)] <- "hotpink"
color.setting [which(dating[,"sec.date"] == 0)] <- "royalblue"
#scatterplots
par(mfrow = c(3,3))
plot(dating$LikeM,dating$LikeF,col = color.setting, pch = pchs,main = "Like")
plot(dating$PartnerYesM,dating$PartnerYesF,col = color.setting, pch = pchs,main = "PartnerYes")
plot(dating$AgeM,dating$AgeF,col = color.setting, pch = pchs,main = "Age")
plot(dating$AttractiveM,dating$AttractiveF,col = color.setting, pch = pchs,main = "Attractive")
plot(dating$IntelligentM,dating$IntelligentF,col = color.setting, pch = pchs,main = "Intelligent")
plot(dating$FunM,dating$FunF,col = color.setting, pch = pchs,main = "Fun")
plot(dating$AmbitiousM,dating$AmbitiousF, col = color.setting, pch = pchs,main = "Ambitious")
plot(dating$SincereM,dating$SincereF,col = color.setting, pch = pchs,main = "Sincere")
plot(dating$SharedInterestsM,dating$SharedInterestsF,col = color.setting, pch = pchs,main = "SharedInterest")

###############
# Question 3  #
###############
summary(dating)
#change the rating scores which are out of rating range
dating$PartnerYesM[which(dating$PartnerYesM==0)]<-1
dating$FunM[which(dating$FunM==0)]<-1
dating$SharedInterestsF[which(dating$SharedInterestsF==0)]<-1
dating$SharedInterestsM[which(dating$SharedInterestsM==0)]<-1

###############
# Question 4  #
###############
#check missing data
length(which(dating$RaceF == ""))
length(which(dating$RaceM == ""))

#replace missing data with NA
dating$RaceF[which(dating$RaceF == "")] <- NA
dating$RaceM[which(dating$RaceM == "")] <- NA

answer <- rep("will be a second date", times=nrow(dating))
answer[dating$sec.date == 0] <- "will not be a second date"

#Mosaic Plot with Female and Male Race
temp <- data.frame(dating$RaceM,dating$RaceF)
mosaicplot(temp, 
           main="Mosaic Plot with Female and Male Race", 
           xlab="RaceM", ylab="RaceF", 
           las=TRUE, cex.axis=1.2,color = c("royalblue","deeppink"))

###############
# Question 5  #
###############
#remove NA
dating <- dating[complete.cases(dating[,-c(1,2,7,8,9,10)]),]
#glm model
logit.1 <- glm(formula=sec.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
    +SincereF+SincereM+IntelligentF+IntelligentM+FunF+FunM+SharedInterestsF+SharedInterestsM
    +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.1)

logit.2 <- glm(formula=sec.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +SincereF+IntelligentF+IntelligentM+FunF+FunM+SharedInterestsF+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.2)

logit.3 <- glm(formula=sec.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +SincereF+IntelligentF+IntelligentM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.3)

logit.4 <- glm(formula=sec.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +IntelligentF+IntelligentM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.4)

logit.5 <- glm(formula=sec.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +IntelligentF+IntelligentM+FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.5)

logit.6 <- glm(formula=sec.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +IntelligentM+FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.6)

logit.7 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +IntelligentM+FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.7)

logit.8 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.8)

logit.9 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
               +FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.9)

logit.10 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
               +FunF+FunM
               +AmbitiousF,family = "binomial",data = dating)
summary(logit.10)

logit.11 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
                +FunF
                +AmbitiousF,family = "binomial",data = dating)
summary(logit.11)

logit.12 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF
                +FunF
                +AmbitiousF,family = "binomial",data = dating)
summary(logit.12)

logit.13 <- glm(formula=sec.date~LikeM+PartnerYesM+PartnerYesF
                +FunF
               ,family = "binomial",data = dating)
summary(logit.13)
#final model
second.date <- logit.13
summary(second.date)

#check multicollinearity
dating.vif <- dating[,c("LikeM","PartnerYesM","PartnerYesF","FunF")]
library(usdm)
vif(dating.vif[complete.cases(dating.vif),])

#compute P-value
pchisq(summary(second.date)$null.deviance-summary(second.date)$deviance,
       df=summary(second.date)$df.null - summary(second.date)$df.residual, lower.tail=FALSE)

###############
# Question 6  #
###############
#create a new data only include variables in the model
dating.q6 <- data.frame(dating.vif,dating$DecisionM,dating$DecisionF,dating$sec.date)

both.want.1 <- length(which(dating.q6$dating.DecisionM == dating.q6$dating.DecisionF 
                            & dating.q6$dating.DecisionM == 1))
both.not.1 <- length(which(dating.q6$dating.DecisionM == dating.q6$dating.DecisionF 
                           & dating.q6$dating.DecisionM == 0))
temp1 <- dating.model[-which(dating.q6$dating.DecisionM == dating.q6$dating.DecisionF),]
F1M0.1 <- length(which(temp1$dating.DecisionF == 1))
F0M1.1 <- length(which(temp1$dating.DecisionM == 1))
#calculate the percentage
both.want.percent.1 <- nrow(dating[which(dating.model$DecisionM == dating.model$DecisionF & dating.model$DecisionM == 1),])/nrow(dating.model)

#check sample size
table(dating.model$dating.sec.date)

###############
# Question 7  #
###############
summary(second.date)

###############
# Question 8  #
###############
#ROC curve
require(pROC)
roc(response=dating.model$dating.sec.date[complete.cases(dating.model)],
    predictor=second.date$fitted.values,
    plot=TRUE, las=TRUE, lwd=3,	legacy.axes=TRUE, 
    main="ROC for Second Date Analysis", cex.main=1.6, cex.axis=1.2, cex.lab=1.3)
auc(response=dating.model$dating.sec.date[complete.cases(dating.model)],
    predictor=second.date$fitted.values)
#find best threshold
roc.info <- roc(response=dating.model$dating.sec.date[complete.cases(dating.model)],
                predictor=second.date$fitted.values)
#adding best sum to the plot
# adding best sum to plot
temp <- as.data.frame(t(coords(roc.info, x="best", ret=c("threshold", "specificity", "sensitivity"))))
points(temp$threshold, temp$specificity + temp$sensitivity, pch=19, col="firebrick")
temp1 <- as.data.frame(t(coords(roc.info, x=0.5, input="threshold", ret=c("threshold", "specificity", "sensitivity"))))
points(temp1$threshold, temp1$specificity + temp1$sensitivity, pch=19, col="blue")
legend("topleft", legend=c(paste("best threshold =", round(temp$threshold, digits=3)),
                           "threshold = 0.5"), pch=19, col=c("firebrick", "blue"), bty="n", cex=1.1)



#temptemptemp
# sensitivity and specificity for a specific threshold of 0.5
coords(roc.info, x=0.5, input="threshold", ret=c("threshold", "sensitivity", "specificity"))

# sensitivity and specificity for the threshold with highest sensitivity + specificity
coords(roc.info, x="best", ret=c("threshold", "specificity", "sensitivity"))

# sensitivity and specificity for a wide range of thresholds
# use t() to transpose output from coords() for easier use
pi.range <- t(coords(roc.info, x="all", ret=c("threshold", "specificity", "sensitivity")))
head(pi.range)


# compute many sensitivity and specificity for a wide range of thresholds:
# t() takes the transpose ---> rows become columns, columns become rows
pi.range <- t(coords(roc.info, x="all", ret=c("threshold", "specificity", "sensitivity")))
head(pi.range)
tail(pi.range)
dim(pi.range)
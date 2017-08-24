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
second.date <- rep(0,nrow(dating))
dating <- data.frame(dating, second.date)
dating$second.date[which(dating$DecisionM == dating$DecisionF & dating$DecisionM == 1)] <-1
#pchs settings
pchs <- rep(NA,nrow(dating))
pchs[which(dating[,"second.date"] == 1)] <- 19
pchs[which(dating[,"second.date"] == 0)] <- 4
#colors setting
color.setting <- rep(NA,nrow(dating))
color.setting [which(dating[,"second.date"] == 1)] <- "hotpink"
color.setting [which(dating[,"second.date"] == 0)] <- "royalblue"
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
dating.q4 <- dating[complete.cases(dating[,c("RaceF","RaceM")]),]
#Mosaic Plot with Female and Male Race
temp <- data.frame(dating.q4$RaceM,dating.q4$RaceF)
mosaicplot(table(temp), 
           main="Mosaic Plot with Female and Male Race", 
           xlab="RaceM", ylab="RaceF", 
           las=TRUE, cex.axis=1.2,color = c("royalblue","pink"))

###############
# Question 5  #
###############
#glm model
logit.1 <- glm(formula=second.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +SincereF+SincereM+IntelligentF+IntelligentM+FunF+FunM+SharedInterestsF+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.1)

logit.2 <- glm(formula=second.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +SincereF+SincereM+IntelligentF+IntelligentM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.2)

logit.3 <- glm(formula=second.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +SincereF+SincereM+IntelligentF+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.3)

logit.4 <- glm(formula=second.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveM+AttractiveF
               +SincereF+SincereM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.4)

logit.5 <- glm(formula=second.date~LikeM+LikeF+PartnerYesM+PartnerYesF+AttractiveF
               +SincereF+SincereM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.5)

logit.6 <- glm(formula=second.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
               +SincereF+SincereM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.6)

logit.7 <- glm(formula=second.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
               +SincereM+FunF+FunM+SharedInterestsM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.7)

logit.8 <- glm(formula=second.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
               +SincereM+FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.8)

logit.9 <- glm(formula=second.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
               +FunF+FunM
               +AmbitiousF+AmbitiousM,family = "binomial",data = dating)
summary(logit.9)

logit.10 <- glm(formula=second.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
                +FunF+FunM
                +AmbitiousF,family = "binomial",data = dating)
summary(logit.10)

logit.11 <- glm(formula=second.date~LikeM+PartnerYesM+PartnerYesF+AttractiveF
                +FunF
                +AmbitiousF,family = "binomial",data = dating)
summary(logit.11)
#final model
final.model <- logit.11
summary(final.model)

##outlier check
dating.q5 <- dating[,c("LikeM","PartnerYesM","PartnerYesF","AttractiveF",
                       "FunF","AmbitiousF")]
plot(dating.q5,pch=20,col = "deeppink")
#check leverage
which(hatvalues(final.model)>2*mean(hatvalues(final.model)))
#compute critical value for influential
cv <- qf(0.5, df1 = 5, df2 = 201)
#check cook distance
which(cooks.distance(final.model)>cv)

##check multicollinearity
library(usdm)
vif(dating.q5[complete.cases(dating.q5),])

#compute P-value
pchisq(summary(final.model)$null.deviance-summary(final.model)$deviance,
       df=summary(final.model)$df.null - summary(final.model)$df.residual, lower.tail=FALSE)

###############
# Question 6  #
###############
#create a new data only include variables in the model
dating.q6 <- dating[complete.cases(dating[,c("LikeM","PartnerYesM","PartnerYesF","AttractiveF",
                                             "FunF","AmbitiousF")]),]
temp.q6 <- data.frame(dating.q6$DecisionM,dating.q6$DecisionF)
table(temp.q6)

#check sample size
table(dating.q6$second.date)

###############
# Question 7  #
###############
summary(final.model)

###############
# Question 8  #
###############
#ROC curve
require(pROC)
roc(response=dating.q6$second.date,
    predictor=final.model$fitted.values,
    plot=TRUE, las=TRUE, lwd=3,	legacy.axes=TRUE, 
    main="ROC for Second Date Analysis", cex.main=1.6, cex.axis=1.2, cex.lab=1.3)
auc(response=dating.q6$dating.second.date[complete.cases(dating.q6)],
    predictor=sfinal.model$fitted.values)

roc.info <- roc(response=dating.q6$second.date,predictor=final.model$fitted.values)
# sensitivity and specificity for a specific threshold of 0.5
coords(roc.info, x=0.5, input="threshold", ret=c("threshold", "sensitivity", "specificity"))

# sensitivity and specificity for the threshold with highest sensitivity + specificity
coords(roc.info, x="best", ret=c("threshold", "specificity", "sensitivity"))

#compute accuracy based on tables
temp1 <- dating.q6
rownames(temp1) <- 1:nrow(temp1)
temp1 <- data.frame(temp1, "fitted.values"=round(final.model$fitted.values, digits=3))


actual.sec <- rep("second.date", times=nrow(temp1))
actual.sec[temp1$second.date == 0] <- "no second.date"

classify.50 <- rep("second.date", times=nrow(temp1))
classify.50[temp1$fitted.values < 0.5] <- "no second.date"

classify.best <- rep("second.date", times=nrow(temp1))
classify.best[temp1$fitted.values < coords(roc.info, x="best", ret="threshold")] <- "no second.date"

table(classify.50, actual.sec)
table(classify.best, actual.sec)

dating.miss <- dating[!complete.cases(dating[,c("LikeM","PartnerYesM","PartnerYesF","AttractiveF",
                                             "FunF","AmbitiousF")]),]
table(dating.miss$second.date)
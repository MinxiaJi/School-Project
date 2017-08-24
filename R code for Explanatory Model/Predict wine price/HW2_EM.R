wine <- read.csv("wine.csv",header = TRUE,stringsAsFactors = FALSE)
prestige <- read.csv("prestige.csv",header = TRUE)

###############
# Question 1c #
###############
wine$VINT[wine$LPRICE2 == "."]

###############
# Question 1d #
###############
wine$LPRICE2 <- as.numeric(wine$LPRICE2)
wine$DEGREES <- as.numeric(wine$DEGREES)
pairs(wine)

###############
# Question 1e #
###############
wine$LPRICE2[wine$LPRICE2 == "."] <- NA
wine$DEGREES[wine$DEGREES == "."] <- NA

rm1 <- lm(as.numeric(LPRICE2)~TIME_SV, data = wine)
rm2 <- lm(as.numeric(LPRICE2)~TIME_SV+as.numeric(DEGREES)+HRAIN+WRAIN,data = wine)

anova(rm1,rm2)

###############
# Question 1f #
###############
summary(rm2)
min(wine$DEGREES)
min(wine$WRAIN)
min(wine$HRAIN)

###############
# Question 1h #
###############

require(qpcR)

# MSE and RMSE
summary(rm1)
summary(rm2)

anova(rm1)
anova(rm2)

#PRESS
PRESS(rm1)
PRESS(rm2)


###############
# Question 2b #
###############
pairs(prestige[,c("education","income","women","prestige")])
prestige$type[prestige$type == ""] <- NA

#pchs settings
pchs <- rep(NA,nrow(prestige))
pchs[which(prestige[,"type"] == "bc")] <- 6
pchs[is.na(prestige$type)] <- 3
pchs[which(prestige[,"type"] == "prof")] <- 8
pchs[which(prestige[,"type"] == "wc")] <- 0
#colors setting
color.setting <- rep(NA,nrow(prestige))
color.setting [which(prestige[,"type"] == "bc")] <- "thistle4"
color.setting [is.na(prestige$type)] <- "hotpink"
color.setting [which(prestige[,"type"] == "prof")] <- "thistle2"
color.setting [which(prestige[,"type"] == "wc")] <- "powderblue"
pairs(prestige[,c("education","income","women","prestige")],pch = pchs,col = color.setting)

###############
# Question 2c #
###############
prestige$occupation.group[is.na(prestige$type)]
prestige <- prestige[!is.na(prestige$type),]


###############
# Question 2e #
###############
lm.2e <- lm(prestige~education+income+type+education*type+income*type, data = prestige)
summary(lm.2e)

###############
# Question 2f #
###############
par(mfrow=c(1,2))
hist(prestige$income, main = "histogram of income", xlab = "income")
log.income <- log(prestige$income)
hist(log.income, main = "histogram of log income", xlab = "log income")

###############
# Question 2g #
###############
prestige.2g <- prestige[,-3]
data.2g <- data.frame(prestige.2g,log.income)
lm.2g <- lm(prestige~education+log.income+type+education*type+log.income*type, 
            data = data.2g)
summary(lm.2g)

###############
# Question 2h #
###############
require(qpcR)

summary(lm.2e)
summary(lm.2g)

anova(lm.2e)
anova(lm.2g)

#PRESS
PRESS(lm.2e)
PRESS(lm.2g)


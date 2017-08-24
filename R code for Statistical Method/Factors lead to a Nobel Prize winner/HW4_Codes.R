#HW4:Codes
#SDGB 7844: Statistical Methods and Computation I
#Minxia Ji

###############
# Question 4  #
###############
#import data
NC<-read.table("nobel_chocolate.txt",header = TRUE,sep = ",")
#scartter plot
plot(x = NC$chocolate, y = NC$nobel_rate,type = "p",
     las = TRUE, main = " Nobel laureates per 100M vs. Chocolate consumption",
     xlab = "Chocolate Consumption (kg/yr/capita)",
     ylab = "Nobel Laureates per 10 Million Population")
# point out the Sweden and label it on the plot
points(x=NC$chocolate[NC$country=="Sweden"],
       y=NC$nobel_rate[NC$country=="Sweden"],cex=1.2, pch=20)
text(6.4,31,cex = 1.2, labels="Sweden")
# compute the correlation between these two variables and add it to the scatterplot
cor(NC[,c("chocolate","nobel_rate")])
text(2.5,20,cex = 1.1, labels = "Correlation coefficient:\n0.8010949")

lm.result <- lm(nobel_rate ~ chocolate, data = NC)
abline(coef(lm.result), col="firebrick", lwd=2)
#dev.off()

###############
# Question 7  #
###############

lm.result <- lm(nobel_rate ~ chocolate, data = NC)

#pdf("graph.pdf", height=5, width=10)
# conduct a residual analysis
plot(x = NC$chocolate, y = lm.result$residuals, type = "p", pch = 19,
     las = TRUE, main = "Residuals vs. Chocolate consumption",
     xlab = "Chocolate Consumption (kg/yr/capita)",ylab = "Residuals")
abline(h=0, col="firebrick", lty=2, lwd=2)
qqnorm(lm.result$residuals, las=TRUE, main="Normal Q-Q Plot for Residuals", pch=19)
qqline(lm.result$residuals)
#dev.off()

# conduct a hypothesis test
summary(lm.result)


###############
# Question 8  #
###############
# the number of Nobel laureates expected to be for Sweden
predict(lm.result)[NC$country == "Sweden"]
# residual for Sweden
residuals(lm.result)[NC$country == "Sweden"]
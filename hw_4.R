data <- read.csv(choose.files())

attach(data)

#Q1 Scatterplot 

plot(education, prestige, pch = 19, xlab = "Education (years)", ylab = "Prestige score", main = "Scatterplot of Canadian Census Data")
cor(education, prestige)

#Question 2 

m1 <- lm(prestige ~ education)
summary(m1)
plot(education, resid(m1)); abline(h = 0, lty = 2)

boxplot.stats(income)

m1 <- lm(prestige ~ education)
data$ID <- 1:nrow(data)

data[abs(rstandard(m1)) > 2, c("ID", "occupation")]
##Checking for occupations with min and max residuals
data[which.min(resid(m1)), c("ID", "occupation")]  
data[which.max(resid(m1)), c("ID", "occupation")]   

plot(data$education, resid(m1), pch = 19, xlab = "Education (years)", ylab = "Residual")
abline(h = 0, lty = 2)
text(data$education[c(53, 67)], resid(m1)[c(53, 67)], labels = c("53 Newsboys", "67 Farmers"), pos = 4)

data2 <- subset(data, !(ID %in% c(53, 67)))
nrow(data2)                                         
m1b <- lm(prestige ~ education, data = data2)
summary(m1b)

qt(0.975, 98)

##Question 4

summary(m)$coefficients
qt(0.975, df = 98)
confint(m, c("education", "income"), level = 0.95)
confint(m, "income") * 1000

m2b <- lm(prestige ~ education + income + women, data)
summary(m2b)

plot(fitted(m), resid(m), pch = 19, main = "Residuals vs Fitted", xlab = "Fitted", ylab = "Residual")
abline(h = 0, lty = 2)
hist(resid(m), main = "Histogram of Residuals")

summary(update(m, data = subset(data, ID != 2)))
summary(update(m, data = subset(data, ID != 24)))
summary(update(m, data = subset(data, !(ID %in% c(2, 24)))))
summary(update(m, data = subset(data, !(ID %in% c(53, 67)))))



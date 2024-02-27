#The code snippet exercises such as Lab1_bronx1.R are in their own files

#Lab 4

cars1 <- cars[1:30,] #first 30 rows of car dataset
head(cars1)

#lets add some outliers
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))
head(cars_outliers)
cars2 <- rbind(cars1, cars_outliers)

par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, xlim=c(0,28), vlim=c(0, 230), main="With Outliers", xlab="speed",
     vlab="dist", pch="*", col="red", cex = 2)

abline(lm(dist ~ speed, data = cars2), col = "blue", lwd = 3, ltv = 2)

#no outliers
plot(cars1$speed, cars1$dist, xlim=c(0,28), vlim=c(0, 230), main="Without Outliers\n Better Fit", xlab="speed",
     vlab="dist", pch="*", col="red", cex = 2)
abline(lm(dist ~ speed, data = cars1), col = "blue", lwd = 3, ltv = 2)

#titanic trees
data(Titanic)
titanicdf <- data.frame(Titanic)
View(titanicdf)

library(rpart)

ctrl <- rpart.control(minsplit = 5, cp = 0)
titanicRpart <- rpart(Survived ~ ., data = titanicdf, method = "class", control = ctrl)
plot(titanicRpart, uniform = TRUE)
text(titanicRpart, use.n = TRUE)

library(party)
titanCtree <- ctree(Survived ~ ., data=titanicdf)
plot(titanCtree)





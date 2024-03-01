mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~ cyl + wt , data = mtcars)
model1
plot(model1, pch = 18, col = "red", which = c(4))

cooks.distance(model1)
CooksDistance <- cooks.distance(model1)

#round off values to 5 decimals
round(CooksDistance, 5)

#ascending
sort(round(CooksDistance, 5))

#outlier detection
library(ISLR)
head(Hitters)
dim(Hitters)
is.na(Hitters)

library(dplyr)
HittersData <- na.omit(Hitters) #remove na values
glimpse(HittersData)
head(HittersData)

#multivariate regression model
#predict salary of baseball player
SalaryPredictModel1 <- lm(Salary ~.,data = HittersData)
summary(SalaryPredictModel1)

#Cook's Distance

cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]

#18 playes have a cook's distance > 3x the mean
#let's exclude those 18 players

namesOfInfluential <- names(influential)
namesOfInfluential
outliers <- HittersData[namesOfInfluential,]
HittersWithoutOutliers <- HittersData %>% anti_join(outliers)

#model 2 without the outliers
SalaryPredictModel2 <- lm(Salary~., data = HittersWithoutOutliers)
summary(SalaryPredictModel2)

#normality tests
set.seed(10)
data1 <- rnorm(50)

set.seed(30)
data2 <- rnorm(50)
#shapiro-wilk normality test

shapiro.test(data1)
hist(data1, col='green')

shapiro.test(data2)

hist(data2, col='steelblue')

set.seed(0)
data <- rnorm(100)

shapiro.test(data)

set.seed(0)

#poisson distribution
data <- rpois(n=100, lambda = 3)

shapiro.test(data)
hist(data, col = 'yellow')

#anderson-darling test for normality

install.packages('nortest')
library(nortest)

set.seed(1)

x <- rnorm(100, 0, 1)

ad.test(x)

set.seed(1)

#uniform distribution
x <- runif(100, 0, 1)

ad.test(x)



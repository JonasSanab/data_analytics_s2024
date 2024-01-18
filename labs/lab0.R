#install.packages(MASS) install MASS package (already did)
library(MASS)
attach(Boston) #attach dataset
head(Boston) #show head of dataset
dim(Boston) #show dimensions of dataset - 506, 14
names(Boston) #show col names
str(Boston) #show structure of dataset
nrow(Boston) #num of rows - 506
ncol(Boston) #num of cols - 14
summary(Boston) #shows statistical summary
summary(Boston$crim) #summary of crime col


library(ISLR)
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg) #min, 1st quartile, median, 3rd qu, max
boxplot(Auto$mpg) #creates boxplot of mpg data
hist(Auto$mpg) #creates histogram of mpg
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))


#reading EPI data from 2010 csv file, skip first line because it's a note
EPI_data <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/2010EPI_data.csv", skip = 1, header = TRUE) #reads in csv file
View(EPI_data)
summary(EPI_data$EPI) #summary of EPI col
boxplot(EPI_data$EPI) #boxplot of EPI col
fivenum(EPI_data$EPI, na.rm=TRUE) #remove NAs
hist(EPI_data$EPI) #histogram of EPI col



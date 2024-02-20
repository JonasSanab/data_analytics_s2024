#Exercise 1

#set seed
set.seed(12345)

par(mar = rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

#run hierarchal cluster analysis on set
#use heatmap
help("heatmap")

par(mar = rep(0.2,4))
heatmap(dataMatrix)

# When we run the heatmap() here, we get the dendrograms printed on the
#both columns and the rows and still there is no real immerging pattern that is
#interesting to us,
#it is because there is no real interesting pattern underlying in the data we
#generated.


set.seed(678910)
for(i in 1:40){
  #by chance
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  
  # if the coin is "Heads", add a common pattern to that row,
  if(coinFlip){
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each =5)
  }
}

#now we will plot data and see that there's more yellow on the right
#means they have a higher vaue, rarer
par(mar=rep(0.2,4))
heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1 , xlab = "The Row Mean", ylab="Row", pch=19)
plot(rowMeans(dataMatrixOrdered), xlab = "Column", ylab="Column Mean", pch=19)

#left plot has the original data reordered
#according the the hierarchical cluster analysis
#of the rows.
#Middle plot has the mean of the each
#rows.(there are 40 rows and therefore 40 dots
#representing the mean)
#right hand side plot has the means of the
#each columns (there are 10 columns and
#therefore 10 dots representing the mean)

#Exercise 2
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep=",")
#add column names
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

#EDA
View(abalone)
summary(abalone)
str(abalone)
summary(abalone$rings)

#we want to predict rings
#abalone is young if less than 8 rings, adult between 8-11, old if > 11

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1, 8, 11, 35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)

#sex variable is not numeric, KNN needs numeric values, so let's get rid of it
aba <- abalone
aba$sex <- NULL

#normalize dataset
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_weight)

#after normalization, each variable has a min of 0 and a max of 1

#split data into training and testing sets

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)

# make k equal to the square root of 2918, the number of observations in the training set.
# sqrt(2918) ~= 54.01852 round it to 55 and use k = 55 # We usually take an Odd number for k value,
# knn model
# knn() is in the "class" library. Make sure to install it first on your RStudio
library(class)
KNNpred <- knn(train =KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

#Exercise 3
library(ggplot2)
head(iris)
str(iris) #150 observations amongst 3 species (equally split)

summary(iris)

#sepal.length vs. sepal.width plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()

# plot Petal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()


# kmeans clustering
set.seed(300)
k.max <- 12

# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss <- sapply(1:k.max, function(k){kmeans(iris[,3:4], k, nstart = 20,iter.max = 1000)$tot.withinss})
wss # within sum of squares.
plot(1:k.max, wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4], 3, nstart = 20, iter.max = 1000)
table(icluster$cluster, iris$Species)

# Classification ctrees
# Install the following libararies/packages
library(rpart)
library(rpart.plot)

#iris dataset
iris
dim(iris) # check the dimensions of the iris dataset

#create a sample
s_iris <- sample(150,100)
s_iris

#create testing and training sets
iris_train <-iris[s_iris,]
iris_test <-iris[-s_iris,]
dim(iris_test)
dim(iris_train)

#generate the decision tree model
dectionTreeModel <- rpart(Species~., iris_train, method = "class")
dectionTreeModel

#plotting the decision tree model using rpart.plot() function
rpart.plot(dectionTreeModel)



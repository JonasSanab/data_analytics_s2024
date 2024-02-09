library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

dim(iris)
siris <- sample(150,100)

iris_train <-iris[siris,]
iris_test <-iris[-siris,]
dim(iris_test)
dim(iris_train)

dectionTreeModel <- rpart(Species~., iris_train,
                          method = "class")
dectionTreeModel

rpart.plot(dectionTreeModel)



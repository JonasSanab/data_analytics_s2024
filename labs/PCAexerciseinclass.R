#PCA with iris dataset

data("iris")
head(iris)


#creating another dataset from iris dataset that contains the columns from 1 to 4

irisdata1 <- iris[, 1:4]

irisdata1

head(irisdata1)

principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)

#cor = a logical value indicating whether the calculation should
#use the correlation matrix or the covariance matrix.
#The correlation matrix can only be used if there are no constant variables.
#score = a logical value indicating whether the score on
#each principal component should be calculated

summary(principal_components)

#Four principal components because the input data has four differnet features
#1 and 2 represent 95%

plot(principal_components)

plot(principal_components, type = "l")

help("biplot")
biplot(principal_components)



#Boston dataset
install.packages("MASS")
data(Boston, package = "MASS")

#prcomp() computes the principal components

pca_out <- prcomp(Boston, scale = T)

pca_out

plot(pca_out)
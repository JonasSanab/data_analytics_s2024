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

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc

head(boston_pc)
summary(boston_pc)


#USArrests
#50 states are the rows
data(USArrests)
View(USArrests)
states = row.names(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# We now perform principal components analysis using the prcomp() function, which is one of several functions in R that perform PCA.
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE,
# we scale the variables to have standard deviation one.
# The output from prcomp() contains a number of useful quantities.
pr.out=prcomp(USArrests, scale=TRUE)

names(pr.out)

# The center and scale components correspond to the means and standard deviations of the
# variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale


#The rotation matrix provides the principal component loadings; each column of pr.out$rotation contains
#the corresponding principal component loading vector
#We see that there are four distinct principal components.
pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale = 0)
pr.out$sdev


#The variance explained by each principal component is obtained by squaring these:
pr.var = pr.out$sdev^2
pr.var


#To compute the proportion of variance explained by each principal component, we simply
#divide the variance explained by each principal component by the total variance explained
#by all four principal components.
pve = pr.var/sum(pr.var)
pve

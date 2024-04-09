#13 variables in the dataset such as alcohol, malic acid, ash, etc.
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)
#need to add variable names
nrow(wine_data)

colnames(wine_data) <- c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols",
                         "Flavanoids", "NonFlavanoid_Phenols", "Proanthocyanins", "Color_Intensity",
                         "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
head(wine_data)

#in the heatmap, dark colors represent the correlated
#light colors represent the not or less correlated

heatmap(cor(wine_data), Rowv = NA, Colv = NA )

cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes


#nromalize the wine data to a common scale so that PCA will not overweight
#variables that happen to have larger values

wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
#PC1 gives 36.2% of cumulative contribution, so PC1 represents 36.2% variance of the data

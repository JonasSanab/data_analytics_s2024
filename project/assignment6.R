load("C:/Users/JNSSN/Desktop/Data Analytics/Latinobarometro_2023_Eng_Rdata_v1_0.rdata")
latinobarometrodf <- data.frame(Latinobarometro_2023_Eng_v1_0)
latinobarometrodf
View(latinobarometrodf)
str(latinobarometrodf)

write.csv(latinobarometrodf, "C:/Users/JNSSN/Desktop/Data Analytics/latino2023.csv", row.names = FALSE)


#exploratory data analysis

#idenpa == country in this dataset
length(unique(latinobarometrodf$idenpa))

library(ggplot2)
library(reshape2)
library(dplyr)

#plot of number of respondents by country
ggplot(latinobarometrodf, aes(x = as.factor(idenpa)))
  + geom_bar(fill = "purple") 
  + labs(x = "Country ID", y = "Number of Respondents", title = "Number of Respondents by Country")
  + theme_minimal()

sum(latinobarometrodf$idenpa == 76)

#histogram of age count
hist(latinobarometrodf$edad, 
     breaks = 20,   
     main = "Distribution of Respondent Ages", 
     xlab = "Age",  
     ylab = "Frequency",  
     col = "skyblue",   
     border = "black")


#plot social media usage counts
socialMedia <- latinobarometrodf[, c('S14M.A', 'S14M.B', 'S14M.C', 'S14M.D', 'S14M.E', 'S14M.F', 'S14M.G', 'S14M.H', 'S14M.I', 'S14M.J')]
colnames(socialMedia) <- c('Facebook', 'Snapchat', 'YouTube', 'X (Twitter)', 'WhatsApp', 'Instagram', 'TikTok', 'LinkedIn', 'Other', 'None')


socialMediaMelted <- melt(socialMedia, variable.name = "Platform", value.name = "usage")

usageCounts <- as.data.frame(table(socialMediaMelted$Platform, socialMediaMelted$usage))
colnames(usageCounts) <- c('Platform', 'usage', 'count')
usageCounts <- subset(usageCounts, usage == 1)

#Make it pretty and give each platform the color they are associated with
#Except I am associating Twitter with a light blue because X is a really silly branding decision by Elon lol

platformColors <- c('Facebook' = 'navyblue', 'Snapchat' = 'yellow', 'YouTube' = 'red', 'X (Twitter)' = 'skyblue',
                    'WhatsApp' = 'green', 'Instagram' = 'magenta', 'TikTok' = 'black', 'LinkedIn' = 'royalblue', 'Other' = 'gray', 'None' = 'orange')

ggplot(subset(socialMediaMelted, usage == 1), aes(x = Platform, fill = Platform)) +
  geom_bar(stat = "count") +
  scale_fill_manual(values = platformColors) +
  theme_minimal() +
  labs(title = "Number of Respondents Using Each Social Media Platform",
       x = "Social Media Platform",
       y = "Number of Users") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sum(latinobarometrodf$S14M.E == 1)

#plot political activities count

sum(latinobarometrodf$P45ST.C == 3)
str(latinobarometrodf$P45ST.A)


actions <- c("P45ST.A" = "Signing a petition",
             "P45S.B" = "Authorized demonstration",
             "P45ST.C" = "Non-authorized demonstration",
             "P45STN.D" = "Social media protest")

bdf <- latinobarometrodf %>%
  select(P45ST.A, P45S.B, P45ST.C, P45STN.D) %>%
  pivot_longer(cols = everything(), names_to = "action", values_to = "response") %>%
  mutate(fullAction = factor(actions[action], levels = actions))

ggplot(bdf, aes(x = fullAction, fill = as.factor(response))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Response Counts for Participating in Different Actions",
       x = "Action",
       y = "Count",
       fill = "Response Category") +
  scale_fill_manual(values = c("1" = "green", "2" = "yellow", "3" = "red", "-5" = "grey"),
                    labels = c("1" = "Performed", "2" = "Haven't, but would",
                               "3" = "Would never", "-5" = "DNA")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#let's prepare for our model
ldf <- data.frame(latinobarometrodf)

#Let us define "political participation" as the willingness or history of performing any of the four listed political activities
ldf$pActive <- as.integer((ldf$P45ST.A %in% c(1, 2) |
                                              ldf$P45S.B %in% c(1, 2) |
                                              ldf$P45ST.C %in% c(1, 2) |
                                              ldf$P45STN.D %in% c(1, 2)))
table(ldf$pActive)

#logistic regression model
lrmodel <- glm(pActive ~ S14M.A + S14M.B + S14M.C + S14M.D + S14M.E + S14M.F + S14M.G + S14M.H + S14M.I + S14M.J,
               family = binomial(), data = ldf)

summary(lrmodel)

library(pROC)

probabilities <- predict(lrmodel, type = "response")
rocCurve <- roc(ldf$pActive, probabilities)
aucVal <- auc(rocCurve)
aucVal

#validation

library(caret)
set.seed(125)
lrdataSplit <- createDataPartition(ldf$pActive, p = 0.80, list = FALSE)
training1 <- ldf[lrdataSplit, ]
testing1 <- ldf[-lrdataSplit, ]

lrmodel <- glm(pActive ~ S14M.A + S14M.B + S14M.C + S14M.D + S14M.E + S14M.F + S14M.G + S14M.H + S14M.I + S14M.J,
               family = binomial(), data = training1)

probabilities <- predict(lrmodel, newdata = testing1, type = "response")
predicted1 <- ifelse(probabilities > 0.5, 1, 0)

rocCurve <- roc(testing1$pActive, probabilities)
aucVal <- auc(rocCurve)
confMatrix <- table(Predicted = predicted1, Actual = testing1$pActive)
accuracy <- sum(diag(confMatrix)) / sum(confMatrix)

rocCurve
aucVal
confMatrix
accuracy
summary(lrmodel)

library(reshape2)  


#let's make a prettier visual representation of our confusion matrix
matrixdf <- as.data.frame(as.table(confMatrix))
colnames(matrixdf) <- c("Predicted", "Actual", "Count")


ggplot(data = matrixdf, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() + 
  scale_fill_gradient(low = "gray", high = "steelblue") +  
  geom_text(aes(label = Count), vjust = 1.5, color = "black") + 
  labs(title = "Confusion Matrix Heatmap", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()




coefdf <- as.data.frame(summary(lrmodel)$coefficients)


names(coefdf) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

ggplot(coefdf, aes(x = rownames(coefdf), y = Estimate, ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`)) +
  geom_pointrange() +
  coord_flip() +
  xlab("Predictors") +
  ylab("Effect Size") +
  ggtitle("Coefficient Plot with 95% CI")











#let's try tuning the weights to see if we can get better results



classCounts <- table(ldf$pActive)
classWeights <- ifelse(ldf$pActive == 1, 1 / classCounts[2], 1 / classCounts[1])
weightsData <- data.frame(pActive = ldf$pActive, weights = classWeights)

set.seed(125)
dataSplit <- createDataPartition(ldf$pActive, p = 0.80, list = FALSE)
training2 <- ldf[dataSplit, ]
testing2 <- ldf[-dataSplit, ]

trainingWeights <- weightsData[dataSplit, "weights"]

lrModel <- glm(pActive ~ S14M.A + S14M.B + S14M.C + S14M.D + S14M.E + S14M.F + S14M.G + S14M.H + S14M.I + S14M.J,
               family = binomial(), data = training2, weights = trainingWeights)

probabilities <- predict(lrModel, newdata = testing2, type = "response")
predicted2 <- ifelse(probabilities > 0.5, 1, 0)

rocCurve <- roc(testing2$pActive, probabilities)
aucVal <- auc(rocCurve)



confMatrix <- table(Predicted = predicted2, Actual = testing2$pActive)
accuracy <- sum(diag(confMatrix)) / sum(confMatrix)

confMatrix
accuracy
aucVal
rocCurve

#these results were not as good


#lets make our second model
#k-means clustering
ldf

socialMedia2 <- c('S14M.A', 'S14M.B', 'S14M.C', 'S14M.D', 'S14M.E', 'S14M.F', 'S14M.G', 'S14M.H', 'S14M.I', 'S14M.J', 'pActive')

clusterData <- ldf[socialMedia2]



clusterDataScaled <- scale(clusterData)

set.seed(125)

sse <- numeric(10)
for (k in 1:10) {
  kmresult <- kmeans(clusterDataScaled, centers=k, nstart=25)
  sse[k] <- kmresult$tot.withinss
}

plot(1:10, sse, type='b', pch=19, frame = FALSE, xlab="Number of Clusters", ylab="Total Within Sum of Squares")
title("Elbow Method Plot")

numClusters <- 3
fkmeans <- kmeans(clusterDataScaled, centers=numClusters, nstart=25)

theclusters <- fkmeans$cluster
table(theclusters)

ldf$cluster <- theclusters
clusterSumm <- ldf %>%
  group_by(cluster) %>%
  summarise(across(all_of(socialMedia2), mean, na.rm = TRUE))

clusterSumm



ldf$cluster <- as.factor(theclusters)

pcaResult <- prcomp(clusterDataScaled, center = TRUE, scale. = TRUE)
pcaData <- as.data.frame(pcaResult$x)

pcaData$cluster <- ldf$cluster

ggplot(pcaData, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "PCA of K-Means Clustering",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster")

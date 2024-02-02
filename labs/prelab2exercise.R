#reading csv file and attaching dataframe
multivarData <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/multivariate.csv", skip = 0, header = TRUE) #reads in csv file
attach(multivarData)
names(multivarData)
multivarData

#creating scatterplots
plot(Income, Immigrant, main = "Scatterplot")
plot(Immigrant, Homeowners)

#Fitting linear models using lm function
help(lm)
mm <- lm(Homeowners ~ Immigrant)
mm
plot(Immigrant, Homeowners)
abline(mm)
abline(mm, col=2, lwd=3)

summary(mm)
attributes(mm)
mm$coefficients

plot(mm)
#abline(mm)

hp <- Homeowners/Population
PD <- Population/area
mm <- lm(Immigrant~Income+Population+PD)
summary(mm)
lm(formula = Immigrant ~ Income + Population + hp + PD)

cm <- coef(mm)
cm

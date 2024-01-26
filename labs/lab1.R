help(read.csv)
theData <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/2010EPI_data.csv", skip = 1, header = TRUE) #reads in csv file
theData
View(theData)
df1 <- data.frame(theData) #adds dataframe
fix(df1) #editor
firstEPI <- df1$EPI


#theOtherData <- read.csv(file.choose(), header=TRUE)

tf <- is.na(firstEPI) # records True values if the value is NA
E <- firstEPI[!tf] # filters out NA values, new array
E

#Exercise 1
summary(E)
fivenum(E,na.rm=TRUE)
stem(E) #stem and leaf plot of EPI col
hist(E)
hist(E, seq(30., 95., 1.0), prob=TRUE)
lines(density(E,na.rm=TRUE,bw=1.))
rug(E)

plot(ecdf(E), do.points=FALSE, verticals=TRUE)#cumulative density function


par(pty="s") #quantile-quantile plot
qqnorm(E); qqline(E)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t
dsn")
qqline(x)

#second variable
tf2 <- is.na(df1$DALY) # records True values if the value is NA
D <- df1$DALY[!tf2]
D
fivenum(D,na.rm=TRUE)
stem(D) #stem and leaf plot of EPI col
hist(D)

#compare plots
boxplot(E,D)
qqplot(E,D)
qqline(D, col = "blue") #i like blue


#exercise 2
EPILand<-df1$EPI[!df1$Landlock] #filters out values where Landlock is true/1
ELand <- EPILand[!is.na(EPILand)] #filters out NA values
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE) #histogram from 30 to 95 with intervals of 1


#comparison between 2010 and 2016 EPI files
newData <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/EPI_data.csv", skip = 0, header = TRUE) #reads in csv file
View(newData)
df2 <- data.frame(newData)
print(df2)
df2$EPI
tf2 <- is.na(df2$EPI) # records True values if the value is NA
E2 <- df2$EPI[!tf2] # filters out NA values, new array
E2
E
fivenum(E2)
fivenum(E)

#Filtering out Western Europe from newData EPI
onlyWestEurope <- E2[df2$GEO_subregion == "Western Europe"]
onlyWE <- onlyWestEurope[!is.na(onlyWestEurope)]
hist(onlyWE)
hist(E2)
#compare no filter and only western europe
boxplot(E2, onlyWE)
fivenum(E2)
fivenum(onlyWE)
print(onlyWE)

#GPW3_grump
GPW3Data <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/GPW3_GRUMP_SummaryInformation_2010.csv", skip = 0, header = TRUE) #reads in csv file
View(GPW3Data)
gdf <- data.frame(GPW3Data)
lvUsed <- gdf$LevelUsed
print(lvUsed)

#get rid of NAs
levels <- lvUsed[!is.na(lvUsed)]
summary(levels)

#values can only be integers for levelUsed
#qqplot looks a little weird
hist(levels)
lines(density(levels,na.rm=TRUE,bw=1.))

qqnorm(levels)
qqline(levels)

#filter out values with an area less than a certain threshold
onlyBig <- levels[gdf$Area > 3000]
summary(onlyBig)
boxplot(levels,onlyBig)

#watertreatment.csv
waterData <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/water-treatment.csv", skip = 0, header = TRUE) #reads in csv file
View(waterData)
wdf <- data.frame(waterData)
php <- wdf$PH.P
print(php)
summary(php)
hist(php)

#lets filter for a specific range
rangedPHP <- php[wdf$PH.P > 7.5 & wdf$PH.P < 8.1]
summary(rangedPHP)

boxplot(php,rangedPHP)
qqnorm(rangedPHP)
qqline(rangedPHP)

#lab 1 part 2
plot(ecdf(df1$EPI),do.points=FALSE,verticals = TRUE)
plot(ecdf(df1$EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(df1$EPI)
qqline(df1$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

#data exploration for 1 other variable (i already did DALY previously)
View(theData)
summary(df1$WATER_H)
plot(ecdf(df1$WATER_H),do.points=FALSE,verticals = TRUE)
plot(ecdf(df1$WATER_H),do.points=TRUE,verticals = TRUE)
qqnorm(df1$WATER_H)
qqline(df1$WATER_H)

#compare the multiple variables using a boxplot
boxplot(df1$WATER_H, df1$DALY, df1$EPI)
boxplot(df1$BIODIVERSITY, df1$ENVHEALTH, df1$ECOSYSTEM, df1$AIR_H, df1$AIR_E, df1$WATER_E)

#book chapter exercises

plot(cars$dist~cars$speed)
plot(cars$dist~cars$speed,
main="Relationship between car distance & speed",
xlab="Speed (miles per hour)",
ylab="Distance travelled (miles)",
xlim=c(0,30),
ylim=c(0,140),
xaxs="i",
yaxs="i",
col="red",
pch=19)

plot(mtcars$wt,mtcars$mpg)
#after installing proper packages
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure, type = "l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")

qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure, data = pressure, geom="line")
ggplot(pressure, aes(x = temperature,y=pressure)) +geom_line() + geom_point()
 


barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()

mtc <- mtcars$mpg
hist(mtc)
hist(mtc, breaks = 10)
hist(mtc, breaks= 5)
hist(mtc, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 5)

plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp,y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()

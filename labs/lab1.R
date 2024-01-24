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

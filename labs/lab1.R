help(read.csv)
theData <- read.csv("C:/Users/sanabj2/Dropbox/My PC (LAPTOP-8KJIP18D)/Desktop/Data Analytics/data_analytics_s2024/labs/2010EPI_data.csv", skip = 1, header = TRUE) #reads in csv file
theData
View(theData)
attach(theData) #adds dataframe
fix(theData) #editor
EPI #Because we added a dataframe, this should print out theData$EPI


#theOtherData <- read.csv(file.choose(), header=TRUE)

tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array
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
tf2 <- is.na(DALY) # records True values if the value is NA
D <- DALY[!tf2]
D
fivenum(D,na.rm=TRUE)
stem(D) #stem and leaf plot of EPI col
hist(D)

#compare plots
boxplot(E,D)
qqplot(E,D)


#exercise 2
EPILand<-EPI[!Landlock] #filters out values where Landlock is true/1
ELand <- EPILand[!is.na(EPILand)] #filters out NA values
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE) #histogram from 30 to 95 with intervals of 1


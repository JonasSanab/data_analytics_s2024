library(gdata) 
#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1Filtered$GROSS.SQUARE.FEET!="0" & bronx1Filtered$LAND.SQUARE.FEET!="0" & bronx1Filtered$SALE.PRICE!="$0"),]

#alternate
library("xlsx", lib.loc="C:/Users/JNSSN/AppData/Local/R/win-library/4.3")
bronx1<-read.xlsx("C:/Users/JNSSN/Desktop/Data Analytics/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)
#
#attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
bronx1$SALE.PRICE<-sub("\\$","",bronx1$SALE.PRICE) 
bronx1$SALE.PRICE<-as.numeric(gsub(",","", bronx1$SALE.PRICE)) 
#bronx1$SALE.PRICE <- bronx1$SALE.PRICE > 0
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$GROSS.SQUARE.FEET)) 
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$LAND.SQUARE.FEET)) 
summary(bronx1$SALE.PRICE <= 0)
bronx1Filtered <- subset(bronx1, SALE.PRICE > 0 & GROSS.SQUARE.FEET > 0 & LAND.SQUARE.FEET > 0 & !is.na(SALE.PRICE) & !is.na(GROSS.SQUARE.FEET) & !is.na(LAND.SQUARE.FEET))
plot(log(bronx1Filtered$GROSS.SQUARE.FEET), log(bronx1Filtered$SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET), data = bronx1Filtered)
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2
bronx1Filtered$SALE.PRICE <- as.numeric(gsub("[\\$,]","",bronx1Filtered$SALE.PRICE ))
bronx1Filtered$GROSS.SQUARE.FEET <- as.numeric(bronx1Filtered$GROSS.SQUARE.FEET)
bronx1Filtered$LAND.SQUARE.FEET <- as.numeric(gsub("[\\,]","",bronx1Filtered$LAND.SQUARE.FEET))

m2<-lm(log(bronx1Filtered$SALE.PRICE)~log(bronx1Filtered$GROSS.SQUARE.FEET)+log(bronx1Filtered$LAND.SQUARE.FEET)+factor(bronx1Filtered$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1Filtered$SALE.PRICE)~0+log(bronx1Filtered$GROSS.SQUARE.FEET)+log(bronx1Filtered$LAND.SQUARE.FEET)+factor(bronx1Filtered$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1Filtered$SALE.PRICE)~0+log(bronx1Filtered$GROSS.SQUARE.FEET)+log(bronx1Filtered$LAND.SQUARE.FEET)+factor(bronx1Filtered$NEIGHBORHOOD)+factor(bronx1Filtered$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1Filtered$SALE.PRICE)~0+log(bronx1Filtered$GROSS.SQUARE.FEET)+log(bronx1Filtered$LAND.SQUARE.FEET)+factor(bronx1Filtered$NEIGHBORHOOD)*factor(bronx1Filtered$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#

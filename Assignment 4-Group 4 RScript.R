#Group 4 Assignment 4 (Barrington and Garcia )

setwd("~/IE 4331 - Exploratory Data Analysis SPRING 2022 TTU/Homework Assignment 3 & 4")
Master<-read.csv("SKU Master.csv")

#Remove rows including "NA"
dat<-na.omit(Master)
#Filter rows where UOMCube is greater than 2 and any less than 0
dat<-dat[dat$UomCube>0&dat$UomCube<2,]
#Filter out rows where UOMWeight is greater than 50 and any less than 0
dat<-dat[dat$UomWeight>0&dat$UomWeight<50,]

#view datatypes in columns
str(dat)
#assign data type to UOM
dat$Uom<-as.factor(dat$Uom)
#view 23 levels of UOM
levels(dat$Uom)
#filter keeping only rows in which UOM includes values in vector c("CA","LB","PL","EA")
dat<-dat[dat$Uom%in%c("CA","EA","LB","PL"),]
#drop unused levels
dat<-droplevels(dat)
#view remaining levels
levels(dat$Uom)

#create a boxplot of UomWeight
boxplot(dat$UomWeight, main="Boxplot on Weight",
        ylab="Weight", col="cyan3", 
        pch=24,bg="cyan3",border="cyan4",cex=.75)

#view statistics on UomWeight
boxplot.stats(dat$UomWeight)#Upper whisker=46.50
#find how many observations lie above upper whisker
upperdat<-dat[dat$UomWeight>46.50,]
nrow(upperdat) #number of values above upper whisker =26
stats<-c(0.01,2.11,9.90,19.00,44.15)

boxplot(dat$UomWeight,
        main="WEIGHT",
        sub="Boxplot on Weight",
        xlab="Weight",
        font.main=2,
        col="red",
        border="black",
        horizontal=TRUE,
        ylim=c(0,50),
        staplewex=1,
        pch=23,
        bg="red",
        notch=TRUE)
        text(x=fivenum(dat$UomWeight),labels=fivenum(dat$UomWeight),y=1.25,cex=.45)
        

#scatterplots of UnitsPerCase and UomWeight
plot(dat$UomWeight~dat$UnitsPerCase,
     pch=19,
     cex=0.75,
     xlab="Units Per Case",
     ylab="Weight",
     main="Weight v. Units Per Case",
     col="gray50")
   
plot(dat$UomWeight)

#barchart to show frequuency of Uom levels
plot(dat$Uom,
     main="Units of Measure",
     col=c("lightgray","gray","darkgray","black"),
     border="red",
     ylab="Count",
     xlab="UOM Type",
     font.lab=4)

na.omit(dat$Commodity) 
droplevels(dat$Commodity)
dat$Commodity=as.factor(dat$Commodity)
plot(dat$Commodity,
     main="Frequency of Commodities",
     sub="Frequency per Type of Commodity",
     cex.main=1.5,
     cex.sub=1.25,
     font.sub=4,
     font.main=2,
     cex.lab=1.15,
     col=rainbow(nlevels(dat$Commodity)),
     xlab="Type of Commodity",
     ylab="Frequency/Count")


#assign Flow as a factor
dat$Flow<-as.factor(dat$Flow)
str(dat$Flow)
#side by side plot of UomCube, Uom, and Flow
boxplot(dat$UomCube~dat$Flow,
        col="blue",
        pch=21,
        bg="red",
        main="Cubic Feet Per Flow Type",
        xlab="Type of Flow",
        ylab="Cubic Feet",
        ylim=c(0,2000))


#Filter to only "direct to store items"
datDD<-dat[dat$Flow=="DD",]

#boxplot of UomWeight
boxplot(datDD$UomWeight,
        main="Weight",
        col="red",
        border="black",
        pch=21,
        cex=1.2,
        bg="red",
        ylab="Weight",
        sub="Direct to Store Items Only")
boxplot.stats(datDD$UomWeight)#outliers appear to be those greater than 14.5
boxplot.stats(datDD$UomWeight)$out #outliers appear to be 20.0,29.0, and 17.8

#event of Weight reaching 30 or plastic/disposable items having UnitsPerCase over 1000
#would remove lines of outliers associated with plastic/disposable items with UnitsPerCase at 1000
datDD<-datDD[!(datDD$UomWeight==29.0)&!(datDD$UomWeight==17.8),]

str(datDD)

#histogram of UomWeight
hist(datDD$UomWeight,
     breaks=5,
     main="Weight Frequencies",
     xlab="Weight",
     col=rainbow(4),
     ylim=c(0,20))

#create dotchart of weight and sku number
dotchart(datDD$UomWeight,datDD$ï..SkuNbr,
         pch=19,
         main="Weight",
         cex.main=2,
         col="black",
         ylab="SkuNumber",
         xlab="Weight",
         cex=.5)
#identify the SkuNbr of the item with the maximum weight
maxweight<-datDD$ï..SkuNbr[datDD$UomWeight==max(datDD$UomWeight)]
maxweight #Sku Number of item with maxweight is 06992111

#filter Uom to EA and CA
datstripchart<-dat[dat$Uom%in%c("CA","EA"),]
datstripchart<-droplevels(datstripchart)
droplevels(datstripchart$Uom)
is.factor(dat$Uom)
#Create stripchart
stripchart(datstripchart$UomWeight~datstripchart$Uom,
           pch=5,
           cex=.5,
           xlab="Weight", 
           main="Stripchart for CA and EA UOMs",
           ylab="UOM Type")


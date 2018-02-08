# Disclaimer - there is probably a lot of unnecessary code in here...

# look at the few weeks befoer the tweets of the WHO statistics

WHOinfluenzastats <- read.csv("filepath/FluNetInteractiveReport_UK.csv", header = T)

# Change the start date to date format:
class(WHOinfluenzastats$SDATE)
# factor
WHOinfluenzastats$SDATE2 <- as.Date(WHOinfluenzastats$SDATE)
WHOinfluenzastats$SDATE2 <- as.character(WHOinfluenzastats$SDATE)
WHOinfluenzastats$SDATE2 <- as.Date(WHOinfluenzastats$SDATE, format("%d/%m/%Y"))

# plot the number of influenza cases against week start date
plot(WHOinfluenzastats$SDATE2, WHOinfluenzastats$ALL_INF)

# Break down by year so we can plot which week the peak is in:
WHOstats2012 <- WHOinfluenzastats[1:52,]
WHOstats2013 <- WHOinfluenzastats[53:104,]
WHOstats2014 <- WHOinfluenzastats[105:156,]
WHOstats2015 <- WHOinfluenzastats[157:209,]
WHOstats2016 <- WHOinfluenzastats[210:261,]
WHOstats2017 <- WHOinfluenzastats[262:313,]
WHOstats2018 <- WHOinfluenzastats[314:316,]

plot(WHOstats2012$Week, WHOstats2012$ALL_INF, ylim=c(0,4500))
plot(WHOstats2013$Week, WHOstats2013$ALL_INF, ylim=c(0,4500))
plot(WHOstats2014$Week, WHOstats2014$ALL_INF, ylim=c(0,4500))
plot(WHOstats2015$Week, WHOstats2015$ALL_INF, ylim=c(0,4500))
plot(WHOstats2016$Week, WHOstats2016$ALL_INF, ylim=c(0,4500))
plot(WHOstats2017$Week, WHOstats2017$ALL_INF, ylim=c(0,4500))
plot(WHOstats2018$Week, WHOstats2018$ALL_INF, ylim=c(0,4500))

# Which weeks are the peak number of flu cases?

WHOstats2012[which.max(WHOstats2012$ALL_INF),]
# Week 51
WHOstats2013[which.max(WHOstats2013$ALL_INF),]
# Week 10
WHOstats2014[which.max(WHOstats2014$ALL_INF),]
# Week 9
WHOstats2015[which.max(WHOstats2015$ALL_INF),]
# Week 8
WHOstats2016[which.max(WHOstats2016$ALL_INF),]
# Week 11
WHOstats2017[which.max(WHOstats2017$ALL_INF),]
# Week 52
WHOstats2018[which.max(WHOstats2018$ALL_INF),]
# Week 2

# which week is the peak?
PeakFluWeek <- as.data.frame(c(51, 10, 9, 8, 11, 52, 2))
names(PeakFluWeek) <- c("PeakFluWeek")
PeakFluWeek$Year <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)

# NHS sick data:

NHSsickdata <- read.csv("filepath/Percentage sick in NHS.csv")

# Look at the trends across the year with all years data:
plot(NHSsickdata$month, NHSsickdata$percentagesick)
plot(WHOinfluenzastats$Week, WHOinfluenzastats$ALL_INF)


WHOinfluenzastats$month <- substr(WHOinfluenzastats$SDATE, 4,5)
WHOinfluenzastats$yearmonth <- paste(WHOinfluenzastats$month,WHOinfluenzastats$Year, sep = "-")
NHSsickdata$month <- as.character(NHSsickdata$month)
NHSsickdata$month[NHSsickdata$month=="1"] <- "01"
NHSsickdata$month[NHSsickdata$month=="2"] <- "02"
NHSsickdata$month[NHSsickdata$month=="3"] <- "03"
NHSsickdata$month[NHSsickdata$month=="4"] <- "04"
NHSsickdata$month[NHSsickdata$month=="5"] <- "05"
NHSsickdata$month[NHSsickdata$month=="6"] <- "06"
NHSsickdata$month[NHSsickdata$month=="7"] <- "07"
NHSsickdata$month[NHSsickdata$month=="8"] <- "08"
NHSsickdata$month[NHSsickdata$month=="9"] <- "09"

NHSsickdata$date <- paste(NHSsickdata$month,NHSsickdata$year, sep = "/")
NHSsickdata$monthyear <- as.Date(NHSsickdata$date, format("%m/%Y"))
NHSsickdata$monthyear <- as.Date(NHSsickdata$year2, format("%Y.%m"))

class(WHOinfluenzastats$yearmonth)
# character
WHOinfluenzastats$year <- substr(WHOinfluenzastats$SDATE2, 1,4)
class(WHOinfluenzastats$year)
WHOinfluenzastats$year <- as.numeric(as.character(WHOinfluenzastats$year))
WHOinfluenzastats$month <- as.numeric(WHOinfluenzastats$month)
WHOinfluenzastats$monthyear <- (WHOinfluenzastats$year + (WHOinfluenzastats$month-1)/12)

# so use WHOinfluenzastats$monthyear and NHSsickdata$year2 for the year in the graphs.

# first aggregate the flu cases per month:

num <- aggregate(WHOinfluenzastats$ALL_INF~WHOinfluenzastats$monthyear,WHOinfluenzastats,length)
names(num)[2] <- 'num'
WHOtotalflupermonth <- aggregate(WHOinfluenzastats$ALL_INF~WHOinfluenzastats$monthyear,WHOinfluenzastats,sum)
names(WHOtotalflupermonth)[2] <- 'totalflupermonth'
merge(num,WHOtotalflupermonth)
colnames(WHOtotalflupermonth)[1] <- "monthyear"
NHSsickLeavePerMonth <- cbind.data.frame(NHSsickdata$year2, NHSsickdata$percentagesick) 
colnames(NHSsickLeavePerMonth)[1] <- "year2"
colnames(NHSsickLeavePerMonth)[2] <- "percentagesick"
plot(NHSsickLeavePerMonth$year2, NHSsickLeavePerMonth$percentagesick, xlim=c(2012, 2018), ylim=c(0, 10))

# Put data into Excel to match them up (because I can't do this in R...)
write.csv(NHSsickLeavePerMonth, file="filepath/NHSsickLeavePerMonth.csv")
write.csv(WHOtotalflupermonth, file="filepath/WHOtotalflupermonth.csv")

# load this back in...
FluAndSickLeavePerMonth <- read.csv(file="filepath/FluAndSickLeavePerMonth.csv", header = T)

# Time series plot
## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)
## Plot first set of data and draw its axis
plot(FluAndSickLeavePerMonth$year, FluAndSickLeavePerMonth$percentagesick, pch=16, cex=0.5, axes=FALSE, xlab="", ylab="", 
     type="o",col="black", main="Cases of flu in UK population, and % of \nNHS staff on sick leave, per month")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("% NHS staff off sick",side=2,line=2.5)
box()
## Allow a second plot on the same graph
par(new=TRUE)
## Plot the second plot and put axis scale on right
plot(FluAndSickLeavePerMonth$year, FluAndSickLeavePerMonth$totalflupermonth, pch=15,  cex=0.5, xlab="", ylab="", 
     axes=FALSE, type="o", col="red")
## a little farther out (line=4) to make room for labels
mtext("Total flu per month",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
## Draw the FluAndSickLeavePerMonth$year axis
axis(1,pretty(range(FluAndSickLeavePerMonth$year),10))
mtext("Time (Months)",side=1,col="black",line=2.5)  
## Add Legend
legend("topleft",legend=c("NHS sick leave","Flu cases"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

# Cross correlogram plot
ccf(FluAndSickLeavePerMonth$percentagesick, FluAndSickLeavePerMonth$totalflupermonth, lag.max = 13, na.action = na.pass, 
    demean=T, main="Cross correlogram of flu cases and \n% NHS sick leave per month", xlab="Lag in months", ylab="CCF")



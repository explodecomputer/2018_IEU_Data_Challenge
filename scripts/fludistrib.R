
library(plyr)
#install.packages("dplyr")

gp_satisfied<-read.csv("C:\\Users\\vh17342\\Desktop\\2018_IEU_Data_Challenge\\GP_PatientExperience\\NHSOF_4a.i_I00739_D.csv")
gp_overload<-read.csv("C:\\Users\\vh17342\\Desktop\\2018_IEU_Data_Challenge\\Unnecessary_Emergency_Admissions\\CCG_3.1_I00759_D.csv")

#hist(gp_satisfied$Indicator.value)
#hist(gp_overload$Indicator.value)

gp_overload_person<-gp_overload[gp_overload$Gender=="Person",-3]
colnames(gp_overload_person)[1]<-"Year"

dfs<-list(gp_overload_person, gp_satisfied)

joined<-join_all(dfs, type="inner", by="Year")
joined_age<-joined[joined$Breakdown=="Age",]

colnames(joined_age)[7]<-"Indicator.value.satisfied"
compare<-compare[compare$Level %in% c("25 to 34","45 to 54"),]

compare<-joined_age[,c(1,7,14,16)]
#plot(compare$Year,compare$Indicator.value.satisfied, cex.axis=0.5, col="red", type="l")
#par(new=T)
#plot(compare$Year,compare$Indicator.value, col="green")

library(ggplot2)
#head(compare)

compare$Indicator.value<-as.numeric(compare$Indicator.value)

ggplot(compare, aes(x=Year, y=Indicator.value)) +
  geom_boxplot(aes(fill=Level))

ggplot(subset(compare), aes(x=Year, y=Indicator.value.satisfied)) +
  geom_violin(aes(fill=Level))

ggplot(compare, aes(x=Year, y=Indicator.value)) +
  geom_boxplot(aes(fill=Level))

summary(compare$Indicator.value)
compare[is.na(compare$Indicator.value),]

dat <- rbind(
  data.frame(Year=compare$Year, Level=compare$Level, value=compare$Indicator.value, what="Unnecessary admissions"),
  data.frame(Year=compare$Year, Level=compare$Level, value=compare$Indicator.value.satisfied, what="Patient satisfaction")
)

dats <- ddply(dat, .(Year, Level, what), summarise, val=mean(value))

ggplot(subset(dats, what=="Patient satisfaction"), aes(x=Year, y=val)) +
geom_bar(stat="identity", aes(fill=Level), position="dodge")+ggtitle("Patient Experience")+
  ylab("Indicator value")

ggplot(subset(dats, what=="Unnecessary admissions"), aes(x=Year, y=val)) +
  geom_bar(stat="identity", aes(fill=Level), position="dodge")+ggtitle("Unnecessary Emergency Admissions")+
  ylab("Indicator value, %")

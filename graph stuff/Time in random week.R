myTable=read.table(file = "train1.txt", header=T, sep=";", colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), ,na.strings="?")
myTable$Date=as.Date(myTable$Date, format="%d/%m/%Y")
summary(myTable$Date)
myTable$time_temp <- paste(myTable$Date, myTable$Time)
myTable$Time2 <- strptime(myTable$time_temp, format = "%Y-%m-%d %H:%M:%S")
myTable2=myTable[myTable$Date>="2007-07-23" & myTable$Date<="2007-07-29",]

library(scales)
ggplot(data=myTable2, aes(x=myTable2$Time2, y=myTable2$Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Figure 1: Global active power in a week")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))

myTable3=myTable[myTable$Date>="2007-12-13" & myTable$Date<="2007-12-17",]

ggplot(data=myTable3, aes(myTable3$Time2))+
  geom_line(aes(y = myTable3$Sub_metering_1, color="Sub metering 1")) + 
  geom_line(aes(y = myTable3$Sub_metering_2, color="Sub metering 2")) + 
  geom_line(aes(y = myTable3$Sub_metering_3, color="Sub metering 3")) + 
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Figure 4: Global Active Power in Sub Metering")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  scale_colour_manual(name='', values=c('Sub metering 1'=rgb(247, 189, 44, maxColorValue = 255), 'Sub metering 2'=rgb(0,0,0, maxColorValue = 255), 'Sub metering 3'=rgb(194, 66, 244, maxColorValue = 255)),guide='legend') 

rm(list=ls())

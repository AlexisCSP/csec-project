Rawdata=read.table(file = "train1.txt", header=T, sep=";", colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), ,na.strings="?")
dim(Rawdata)
annual_GloActPow <- aggregate(Rawdata$Global_active_power,
                              by = list(Rawdata$Date), 
                              FUN = sum, na.action = T)
names(annual_GloActPow) <- c("Date","GloActPow")
annual_GloReaPow <- aggregate(Rawdata$Global_reactive_power,
                              by = list(Rawdata$Date), 
                              FUN = sum, na.action = T)
names(annual_GloActPow) <- c("Date","GloReaPow")
annual_Power <- cbind(annual_GloActPow, annual_GloReaPow$x)
annual_Power <- setNames(cbind(annual_Power,rowSums(annual_Power[,c(2,3)])),
                         c("Date","GloActPo","GloReaPo","Sum_AcRePo"))
library(chron)
annual_Power$Date <- chron(dates=annual_Power$Date)
class(annual_Power$Date)

ggplot(annual_Power,aes(Date,Sum_AcRePo)) + 
  geom_smooth(model = lm) + 
  geom_point(aes(color = as.numeric(format(Date,'%Y')))) + 
  xlab("Date (year)") + ylab("Total Energy Comsuption (kilowatts)") +
  ggtitle ("Total Energy Comsuption by one household per day from 2007 to 2011") + 
  theme(legend.title=element_blank())

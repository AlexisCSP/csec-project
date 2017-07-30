library(readr)
library(zoo)
data <- read_csv("~/cybersecurity/train.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
#View(data)
data <- na.omit(data)
reduced_data <- data[,1:3]
reduced_data$Time <- NULL
agg_Day <- aggregate (Global_active_power~Date, reduced_data, FUN=mean)
View(agg_Day)
tsWeek <- ts(agg_Day$Global_active_power,frequency=12,start=c(2006,51))
View(tsWeek)
plot(agg_Day, type="o", col="black", ylab="Global Active Power(Kilowatts)",xlab="Years",main="Figure 4: Global active power in every year")
r <- decomposes(tsWeek)
plot(tsWeek, type="o", col="red", ylab="Global Active Power(Kilowatts)",xlab="Month")
rm(list=ls())

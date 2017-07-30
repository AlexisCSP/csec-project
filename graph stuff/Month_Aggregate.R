library(readr)
library(zoo)
data <- read_csv("~/cybersecurity/train.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
#View(data)
data <- na.omit(data)
reduced_data <- data[,1:4]
#View(reduced_data)
reduced_data$DateTime <-NULL
#reduced_data$Time <-NULL
agg_Month <-aggregate(reduced_data,by=list(as.yearmon(reduced_data$Date,'%d/%m/%Y')), 
                      FUN = mean, na.rm = TRUE )
#View(agg_Month)

agg_Month$Date <- NULL
agg_Month$Time <- NULL
agg_Month$Global_reactive_power <- NULL
plot(agg_Month, type="o", col="black", ylab="Global Active Power(Kilowatts)",xlab="Month")

xdata2 <- read_csv("~/cybersecurity/test2.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
#View(data)
data2 <- na.omit(data)
reduced_data2 <- data[,1:4]
#View(reduced_data)
reduced_data2$DateTime <-NULL
#reduced_data$Time <-NULL
agg_Month2 <-aggregate(reduced_data2,by=list(as.yearmon(reduced_data2$Date,'%d/%m/%Y')), 
                      FUN = mean, na.rm = TRUE )
#View(agg_Month)

agg_Month2$Date <- NULL
agg_Month2$Time <- NULL
agg_Month2$Global_reactive_power <- NULL
library(zoo)
agg_Month$Group.1 <-as.yearmon(agg_Month$Group.1)
agg_Month2$Group.1 <-as.yearmon(agg_Month2$Group.1)

library(ggplot2)
ggplot(agg_Month,aes(Group.1,Global_active_power))+geom_line(aes(color="First line"))+geom_line(data=agg_Month2,aes(color="Second line"))+labs(color="Legend text")

                       
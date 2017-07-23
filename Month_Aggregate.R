library(readr)
library(zoo)
data <- read_csv("~/cybersecurity/train.csv", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
View(data)
data <- na.omit(data)
reduced_data <- data[,1:4]
View(reduced_data)
reduced_data$DateTime <-NULL
#reduced_data$Time <-NULL
agg_Month <-aggregate(reduced_data,by=list(as.yearmon(reduced_data$Date,'%d/%m/%Y')), 
                      FUN = mean, na.rm = TRUE )
View(agg_Month)

agg_Month$Date <- NULL
agg_Month$Time <- NULL

plot(agg_Month, type="o", col="blue", ylab="Global Active Power(Kilowatts)",xlab="Month")


                       
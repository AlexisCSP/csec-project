library(readr)
data <- read_csv("~/cybersecurity/train.csv", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
View(data)
data <- na.omit(data)
reduced_data <- data[,1:4]
View(reduced_data)
reduced_data$DateTime <-NULL

require(data.table)
reduced_data <- data.table(reduced_data)
res<-reduced_data[,list(Mean=mean(Global_active_power)),by=list(Date,hour(as.POSIXct(reduced_data$Time, 
                                                                                    format = "%H:%M:%S")))]

res$Date <- NULL
r1 <- aggregate (Mean~hour, res, FUN=mean)
View(r1)

plot(r1, type="o", col="blue", ylab="Global Active Power(Kilowatts)",xlab="Time")

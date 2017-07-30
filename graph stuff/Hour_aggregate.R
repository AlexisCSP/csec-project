library(readr)
data <- read_csv("~/cybersecurity/train.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
#View(data)
data <- na.omit(data)
reduced_data <- data[,1:4]
#View(reduced_data)
reduced_data$DateTime <-NULL

require(data.table)
reduced_data <- data.table(reduced_data)
res<-reduced_data[,list(Mean=mean(Global_active_power)),by=list(Date,hour(as.POSIXct(reduced_data$Time, 
                                                                                    format = "%H:%M:%S")))]

res$Date <- NULL
r1 <- aggregate (Mean~hour, res, FUN=mean)
#View(r1)
par(bg='grey')
plot(r1, type="b", col="black", main="Figure 2: Global active power in 24 hours",ylab="Global Active Power(Kilowatts)",xlab="Time(24 hrs)")
 

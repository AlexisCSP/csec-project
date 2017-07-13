library(HMM)
library(mhsmm)

if(!exists("foo", mode="function")) source("formatMhsmm.R")

# load data
library(readr)
data <- read_csv("~/cybersecurity/train.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))
View(data)

data$Date <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
data <- na.omit(data)
data$Time <- NULL

reduced_data <- data[1:100000,]

plot.ts(reduced_data[2:8])

# Number of states
J <- 5

train_data <- formatMhsmm(reduced_data[,2:8])
test_data <- formatMhsmm(reduced_data[1:20000,2:8])

# Calculating the means by KNN
m <- kmeans(reduced_data[,2:8], J)

reduced_data$cluster <- m$cluster

k1 <- subset(reduced_data, reduced_data$cluster == 1)
k2 <- subset(reduced_data, reduced_data$cluster == 2)
k3 <- subset(reduced_data, reduced_data$cluster == 3)
k4 <- subset(reduced_data, reduced_data$cluster == 4)
k5 <- subset(reduced_data, reduced_data$cluster == 5)

# Calculating the std
k11_std <- sd(k1$Global_active_power)
k21_std <- sd(k2$Global_active_power)
k31_std <- sd(k3$Global_active_power)
k41_std <- sd(k4$Global_active_power)
k51_std <- sd(k5$Global_active_power)

k12_std <- sd(k1$Global_reactive_power)
k22_std <- sd(k2$Global_reactive_power)
k32_std <- sd(k3$Global_reactive_power)
k42_std <- sd(k4$Global_reactive_power)
k52_std <- sd(k5$Global_reactive_power)

k13_std <- sd(k1$Voltage)
k23_std <- sd(k2$Voltage)
k33_std <- sd(k3$Voltage)
k43_std <- sd(k4$Voltage)
k53_std <- sd(k5$Voltage)

k14_std <- sd(k1$Global_intensity)
k24_std <- sd(k2$Global_intensity)
k34_std <- sd(k3$Global_intensity)
k44_std <- sd(k4$Global_intensity)
k54_std <- sd(k5$Global_intensity)

k15_std <- sd(k1$Sub_metering_1)
k25_std <- sd(k2$Sub_metering_1)
k35_std <- sd(k3$Sub_metering_1)
k45_std <- sd(k4$Sub_metering_1)
k55_std <- sd(k5$Sub_metering_1)

k16_std <- sd(k1$Sub_metering_2)
k26_std <- sd(k2$Sub_metering_2)
k36_std <- sd(k3$Sub_metering_2)
k46_std <- sd(k4$Sub_metering_2)
k56_std <- sd(k5$Sub_metering_2)

k17_std <- sd(k1$Sub_metering_3)
k27_std <- sd(k2$Sub_metering_3)
k37_std <- sd(k3$Sub_metering_3)
k47_std <- sd(k4$Sub_metering_3)
k57_std <- sd(k5$Sub_metering_3)

k1_std = c(k11_std,k21_std,k31_std, k41_std, k51_std)
k2_std = c(k12_std,k22_std,k32_std, k42_std, k52_std)
k3_std = c(k13_std,k23_std,k33_std, k43_std, k53_std)
k4_std = c(k14_std,k24_std,k34_std, k44_std, k54_std)
k5_std = c(k15_std,k25_std,k35_std, k45_std, k55_std)
k6_std = c(k16_std,k26_std,k36_std, k46_std, k56_std)
k7_std = c(k17_std,k27_std,k37_std, k47_std, k57_std)

std_matrix = matrix(c(k1_std, k2_std, k3_std, k4_std, k5_std, k6_std, k7_std), nrow = J, ncol = 7)

# init probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

# emission matrix
b <- list(mu = m$centers, sigma = std_matrix)

# fitting the model
model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dnorm.hsmm)
model


h1 = hmmfit(train_data$x, model, mstep = mstep.norm)
h1$loglik

summary(h1)

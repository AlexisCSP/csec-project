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
J <- 3

train_data <- formatMhsmm(reduced_data[,2])
test_data <- formatMhsmm(reduced_data[1:20000,2])

# Calculating the means by KNN
t <- kmeans(reduced_data$Global_active_power, J)
reduced_data$cluster <- t$cluster

k1 <- subset(reduced_data, reduced_data$cluster == 1)
k2 <- subset(reduced_data, reduced_data$cluster == 2)
k3 <- subset(reduced_data, reduced_data$cluster == 3)

# Calculating the std
k1_std <- sd(k1$Global_active_power)
k2_std <- sd(k2$Global_active_power)
k3_std <- sd(k3$Global_active_power)

# init probabilities
init <- rep(1/J,J)

# transition probability matrix
P <- matrix(rep(1/J,J),nrow=J,ncol = J)

# emission matrix
b <- list(mu=t$centers,sigma=c(k1_std,k2_std,k3_std))

# fitting the model
model <- hmmspec(init=init, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
model


h1 = hmmfit(train_data$x,model,mstep=mstep.norm)
h1$loglik


summary(h1)

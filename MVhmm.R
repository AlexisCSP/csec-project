library(mhsmm)

if(!exists("foo", mode="function")) source("formatMhsmm.R")

# load data
library(readr)
data <- read_csv("train.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))

# remove NA rows
data <- na.omit(data)

# Leave columns of interest and reduce size of dataset
reduced_data <- data[1:100000,3:6]
plot.ts(reduced_data)

# scale the values
scaled_data <- data.frame(scale(reduced_data))

correlation <- cor(scaled_data)

# Defining MHMM parameters


# Number of states
J <- 3


# Calculating the means by KNN
m <- kmeans(scaled_data, J)

scaled_data$cluster <- m$cluster

k1 <- subset(scaled_data, scaled_data$cluster == 1)
k2 <- subset(scaled_data, scaled_data$cluster == 2)
k3 <- subset(scaled_data, scaled_data$cluster == 3)

scaled_data$cluster <- NULL

# Calculating the std

k11_std <- sd(k1$Global_active_power)
k21_std <- sd(k2$Global_active_power)
k31_std <- sd(k3$Global_active_power)

k12_std <- sd(k1$Global_reactive_power)
k22_std <- sd(k2$Global_reactive_power)
k32_std <- sd(k3$Global_reactive_power)

k13_std <- sd(k1$Voltage)
k23_std <- sd(k2$Voltage)
k33_std <- sd(k3$Voltage)

k14_std <- sd(k1$Global_intensity)
k24_std <- sd(k2$Global_intensity)
k34_std <- sd(k3$Global_intensity)

# Diagonal for the std matrix of each state

k1_diag = c(k11_std,k12_std,k13_std, k14_std)
k2_diag = c(k21_std,k22_std,k23_std, k24_std)
k3_diag = c(k31_std,k32_std,k33_std, k34_std)


# Standard deviation matrix, first get the covariance for each state

k1_std <- cov(k1[,1:4])
k2_std <- cov(k2[,1:4])
k3_std <- cov(k3[,1:4])

# fill in the diagonal

diag(k1_std) <- k1_diag
diag(k2_std) <- k2_diag
diag(k3_std) <- k3_diag


std_matrix <- list(k1_std, k2_std, k3_std)

# init probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

# emission matrix
means <- t(m$centers)
means <- list(c(means[,1]), c(means[,2]), c(means[,3]))

b <- list(mu = means, sigma = std_matrix)

# fitting the model
model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dmvnorm.hsmm)
model

# train model

train_data <- list(x = data.frame(scaled_data), N = nrow(scaled_data))
train_data$x <- data.matrix(train_data$x, rownames.force = NA)
class(train_data) <- "hsmm.data"

h1 = hmmfit(train_data$x, model, mstep = mstep.mvnorm, maxit= 300)
summary(h1)

# test data

test <- read_csv("test2.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))

# remove NA rows
test <- na.omit(test)

test <- test[1:100000,3:6]
scaled_test <- data.frame(scale(test))

test_data <- list(x = data.frame(scaled_test), N = nrow(scaled_test))
test_data$x <- data.matrix(test_data$x, rownames.force = NA)
class(test_data) <- "hsmm.data"


yhat1 <- predict.hmm(h1, train_data$x)
yhat2 <- predict.hmm(h1, test_data$x)

mean(yhat1$s != yhat2$s)

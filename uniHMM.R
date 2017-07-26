library(mhsmm)

if(!exists("foo", mode="function")) source("formatMhsmm.R")

# load data
library(readr)
data <- read_csv("train.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))


# test data
test <- read_csv("test2.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))

# remove NA rows
data <- na.omit(data)
test <- na.omit(test)

# Leave columns of interest and reduce size of dataset


w = 250000

whole <- TRUE
if (whole) {
  w = length(data$Global_active_power)
}

reduced_data <- data[1:w,3]
train_data <- formatMhsmm(reduced_data)

reduced_test <- test[1:w,3]
test_data <- formatMhsmm(reduced_test)


plot.ts(reduced_data)


# Defining MHMM parameters


# Number of states
J <- 3


# Calculating the means by KNN
m <- kmeans(reduced_data, J)

reduced_data$cluster <- m$cluster

k1 <- subset(reduced_data, reduced_data$cluster == 1)
k2 <- subset(reduced_data, reduced_data$cluster == 2)
k3 <- subset(reduced_data, reduced_data$cluster == 3)

reduced_data$cluster <- NULL

# Calculating the std

k1_std <- sd(k1$Global_active_power)
k2_std <- sd(k2$Global_active_power)
k3_std <- sd(k3$Global_active_power)

std_matrix <- c(k1_std, k2_std, k3_std)

# init probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

# emission matrix
means <- c(m$centers)

b <- list(mu = means, sigma = std_matrix)

# fitting the model
model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dnorm.hsmm)
model

# train model

h1 = hmmfit(train_data$x, model, mstep = mstep.norm)
summary(h1)


#yhat2 <- predict.hmm(h1, validation_data$x)
yhat2 <- predict.hmm(h1, test_data$x)

#plot(yhat2)

emission_params = h1$model$parms.emission

anomalies <- 0
anomaly_results = matrix(nrow = yhat2$N, ncol = 2)
for(i in 1:yhat2$N) {
  test_state = yhat2$s[i]
  test_obs = yhat2$x[i]
  state_mean = emission_params$mu[test_state]
  state_std = emission_params$sigma[test_state]
  anomaly_results[i,1] <- 0
  anomaly_results[i,2] <- 1 - pnorm(test_obs, mean=state_mean, sd=state_std) 
  if(abs(test_obs - state_mean) > state_std) {
    anomaly_results[i,1] <- 1
    anomalies <- anomalies + 1
  }
}

h1$loglik

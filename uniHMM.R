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



yhat1 <- predict.hmm(h1, train_data$x)
yhat2 <- predict.hmm(h1, test_data$x)

plot(yhat2)

anomalies = 0
for(i in 1:yhat1$N) {
  train_state = yhat1$s[i]
  test_state = yhat2$s[i]
  train_prob = yhat1$p[i,train_state]
  test_prob = yhat1$p[i,test_state]
  anomaly_prob = train_prob - test_prob
  if(anomaly_prob > 0.66) {
    print(anomaly_prob)
    anomalies <- anomalies + 1
  }
}

yhat1$p[yhat1$s != yhat2$s,yhat2$s[yhat1$s != yhat2$s]]

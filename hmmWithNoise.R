library(mhsmm)
library(mclust)

if(!exists("foo", mode="function")) source("formatMhsmm.R")

# load train data
library(readr)
train <- read_csv("train.txt", 
                  col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                   Time = col_time(format = "%H:%M:%S")))


# load test data
test <- read_csv("test2.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))

# remove NA rows
train <- na.omit(train)
test <- na.omit(test)

# Split into train and validation data

rows <- nrow(train)
split1 <- floor(.90*rows)
split2 <- split1 + 1

train_split <- train[1:split1,]
validation_split  <- train[split2:rows,]

# Leave column of interest (Global reactive power)

train_data <- train_split[,3]
validation_data <- validation_split[,3]
test_data <- test[,3]

# Defining MHMM parameters

# Number of states
J <- 4

# init probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

b <- list(mu = c(1,2), sigma = c(2,1))

# fitting the model
model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dnorm.hsmm)
model

# format the data for training, prediction

train_data <- formatMhsmm(data.frame(train_data))
validation_data <- formatMhsmm(data.frame(validation_data))
test_data <- formatMhsmm(data.frame(test_data))

corrupt <- rbinom(length(train_data$x), 1, 0.1)
corrupt <- as.logical(corrupt)
noise <- runif(sum(corrupt), 0.0, 7.0)
train_data$x[corrupt] <- noise

# train model

hmm = hmmfit(train_data$x, model, mstep = mstep.norm, maxit=100)
summary(hmm)

validation_pred <- predict.hmm(hmm, validation_data$x)
test_pred <- predict.hmm(hmm, train_data$x,method="smoothed")

emission_params = hmm$model$parms.emission

min <- min(train_data$x)
max <- max(train_data$x)

normalize <- function(value, min, max) {
  num <- value - min
  den <- max - min
  result <- num/den
  return(result)
}

anomalies <- 0
errors <- 0
threshold <- 1
anomaly_results = matrix(nrow = validation_pred$N, ncol = 7)
for(i in 1:validation_pred$N) {
  test_state = validation_pred$s[i]
  test_obs = validation_pred$x[i]
  state_mean = emission_params$mu[test_state]
  state_std = emission_params$sigma[test_state]
  
  normal <- normalize(abs(test_obs-state_mean), min, max)
  
  anomaly_results[i,1] <- 0
  anomaly_results[i,2] <- normal
  
  if(normal > 0.5) {
    anomalies <- anomalies + 1
  }
  
  if(abs(test_obs-state_mean) > threshold) {
    
    anomaly_results[i,1] <- 1
    #anomaly_results[i,2] <- 0.95
    anomaly_results[i,3] <- test_state
    anomaly_results[i,4] <- state_std
    anomaly_results[i,5] <- 3*state_std
    anomaly_results[i,6] <- test_obs
    anomaly_results[i,7] <- state_mean
    #anomalies <- anomalies + 1
  }
}

test_pred <- predict.hmm(hmm, test_data$x)

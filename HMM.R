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
J <- 7

# Calculating the means by KNN
kclust <- kmeans(train_data, J, nstart = 25)
gm_clusters <- Mclust(train_data)

train_data$cluster <- kclust$cluster

# create arrays to hold each cluster

clusters <- vector("list", J)

for(i in 1:J) {
  clusters[[i]] = numeric(m$size[i])
  clusters[[i]] = subset(train_data, train_data$cluster == i)
}

train_data$cluster <- NULL

# Calculating the std

clusters_std <- vector(mode = "numeric", length = J)

for(i in 1:J) {
  clusters_std[i] = sd(clusters[[i]][[1]])
}

std_matrix <- clusters_std

# init probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

# emission matrix
means <- c(kclust$centers)

b <- list(mu = means, sigma = std_matrix)

# fitting the model
model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dnorm.hsmm)
model

# format the data for training, prediction

train_data <- formatMhsmm(data.frame(train_data))
validation_data <- formatMhsmm(data.frame(validation_data))
test_data <- formatMhsmm(data.frame(test_data))

# train model

hmm = hmmfit(train_data$x, model, mstep = mstep.norm)
summary(hmm)

validation_pred <- predict.hmm(hmm, validation_data$x)

emission_params = hmm$model$parms.emission

anomalies <- 0
anomaly_results = matrix(nrow = validation_pred$N, ncol = 2)
for(i in 1:validation_pred$N) {
  test_state = validation_pred$s[i]
  test_obs = validation_pred$x[i]
  state_mean = emission_params$mu[test_state]
  state_std = emission_params$sigma[test_state]
  #test_obs_prob = pnorm(test_obs, mean=state_mean, sd=state_std)
  anomaly_results[i,1] <- 0
  anomaly_results[i,2] <- 0.05
  if(abs(test_obs - state_mean) > 2*state_std) {
    anomaly_results[i,1] <- 1
    anomaly_results[i,2] <- 0.95
    anomalies <- anomalies + 1
  }
}

pnorm(1.65, mean=0.44, sd=0.0335)

test_pred <- predict.hmm(hmm, test_data$x)

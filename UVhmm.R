library(mhsmm)
if(!exists("foo", mode="function")) source("formatHMM.R")
if(!exists("foo", mode="function")) source("detectPointAnomaliesUVhmm.R")
if(!exists("foo", mode="function")) source("detectCollectiveAnomaliesUVhmm.R")
if(!exists("foo", mode="function")) source("AddCollectiveNoiseUVhmm.R")


#--------------------------------------------- Load dataset ---------------------------------------------#

# load train data
library(readr)
train <- read_csv("train.txt", 
                  col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                   Time = col_time(format = "%H:%M:%S")))


# load test data
test <- read_csv("test1.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))

#--------------------------------------------- Clean dataset ---------------------------------------------#

# remove NA rows
train <- na.omit(train)
test <- na.omit(test)

#--------------------------------------------- Split dataset  ---------------------------------------------#

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

#--------------------------------------------- Define HMM parameters  ---------------------------------------------#

# Number of states
J <- 3

# Number of variables
v <- 1

# Calculating the clusters and the means of the states by KNN
kclust <- kmeans(train_data, J)

means <- c(kclust$centers)

train_data$cluster <- kclust$cluster

# create arrays to hold observations of each cluster/state
clusters <- vector("list", J)

for(i in 1:J) {
  clusters[[i]] = numeric(kclust$size[i])
  clusters[[i]] = subset(train_data, train_data$cluster == i)
}

train_data$cluster <- NULL

# Calculating the std for each cluster/state (column of the matrix)
clusters_std <- matrix(nrow = v, ncol = J)

for(i in 1:J) {
  for(k in 1:v) {
    clusters_std[k,i] = sd(clusters[[i]][[k]])
  }
}

std_matrix <- c(clusters_std)

#--------------------------------------------- Create Univariate HMM Specification ---------------------------------------------#

# initial state probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

# emission matrix
b <- list(mu = unlist(means), sigma = std_matrix)

# fitting the model

model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dnorm.hsmm)
model

#--------------------------------------------- Fit Univariate HMM ---------------------------------------------#

# format the training data
train_data <- formatHMM(train_data)

# train model
hmm = hmmfit(train_data, model, mstep = mstep.norm, maxit= 300)
summary(hmm)

#--------------------------------------------- Validate Univariate HMM ---------------------------------------------#

# format the validation data
validation_data <- formatHMM(data.frame(validation_data))

# add point anomalies to validation data
noisyP_validation_data <- validation_data

corrupt <- rbinom(length(noisyP_validation_data$x), 1, 0.1)
corrupt <- as.logical(corrupt)
noise <- runif(sum(corrupt), 0.0, 7.0)
noisyP_validation_data$x[corrupt] <- noise

# predict with noisy validation data
validation_pred <- predict.hmm(hmm, noisyP_validation_data)

# detect point anomalies
threshold <- 1
validation_point_anomalies <- detectPointAnomaliesUVhmm(validation_pred, hmm$model$parms.emission, threshold)

# add collective anomalies to validation data
window_size <- 5
noisyC_validation_data <- validation_data
noisyC_validation_data <- AddCollectiveNoiseUVhmm(noisyC_validation_data, window_size)
validation_pred <- predict.hmm(hmm, noisyC_validation_data)

# detect collective anomalies
threshold <- 1
validation_collective_anomalies <- detectCollectiveAnomaliesUVhmm(validation_pred, hmm$model$parms.emission, window_size, threshold)


#--------------------------------------------- Test Univariate HMM ---------------------------------------------#

# format the test data
test_data <- formatHMM(data.frame(test_data))

# predict with test data
test_pred <- predict.hmm(hmm, test_data)

# detect anomalies
threshold <- 2
test_point_anomalies <- detectPointAnomaliesUVhmm(test_pred, hmm$model$parms.emission, threshold)

# detect collective anomalies
threshold <- 1
window_size <- 5
test_collective_anomalies <- detectCollectiveAnomaliesUVhmm(test_pred, hmm$model$parms.emission, window_size, threshold)


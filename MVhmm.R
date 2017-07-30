library(mhsmm)
if(!exists("foo", mode="function")) source("formatHMM.R")
if(!exists("foo", mode="function")) source("detectPointAnomaliesMVhmm.R")
if(!exists("foo", mode="function")) source("detectCollectiveAnomaliesMVhmm.R")

#--------------------------------------------- Load dataset ---------------------------------------------#

# load train data
library(readr)
train <- read_csv("train.txt", 
                  col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                   Time = col_time(format = "%H:%M:%S")))


# load test data
test <- read_csv("test2.txt", 
                 col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                  Time = col_time(format = "%H:%M:%S")))

#--------------------------------------------- Clean dataset ---------------------------------------------#

# remove NA rows
train <- na.omit(train)
test <- na.omit(test)

# scale observations
correlation <- cor(train[,3:6])
scaled_train <- data.frame(scale(train[,3:6]))

#--------------------------------------------- Split dataset  ---------------------------------------------#

# Split into train and validation data

rows <- nrow(scaled_train)
split1 <- floor(.90*rows)
split2 <- split1 + 1

train_split <- scaled_train[1:split1,]
validation_split  <- scaled_train[split2:rows,]

train_data <- train_split
validation_data <- validation_split
test_data <- data.frame(scale(test[,3:6])) # Leave columns of interest in test

#--------------------------------------------- Define MVHMM parameters  ---------------------------------------------#

# Number of states
J <- 3

# Number of variables
v <- 4

# Calculating the clusters and the means of the states by KNN
kclust <- kmeans(train_data, J)

means <- vector("list", J)

for(i in 1:J) {
  means[[i]] <- kclust$centers[i,]
}

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

# Calculate covariance between variables within a cluster
clusters_cov <- vector("list", J)

i <- 1
for(c in clusters) {
  clusters_cov[[i]] = matrix(nrow = v, ncol = v)
  clusters_cov[[i]] = cov(c[1:4])
  i <- i + 1
}

# Define the diagonal of the covariance matrices for each state as the standard deviation of corresponding variable
for(i in 1:J) {
    diag(clusters_cov[[i]]) = clusters_std[,i]
}

std_matrix <- clusters_cov

#--------------------------------------------- Create MVHMM Specification ---------------------------------------------#

# initial state probabilities
init <- rep(1/J, J)

# transition probability matrix
P <- matrix(rep(1/J, J), nrow = J, ncol = J)

# emission matrix
b <- list(mu = means, sigma = std_matrix)

# fitting the model

model <- hmmspec(init = init, trans = P, parms.emission = b, dens.emission = dmvnorm.hsmm)
model

#--------------------------------------------- Fit MVHMM ---------------------------------------------#

# format the training data
train_data <- formatHMM(train_data)

# train model
hmm = hmmfit(train_data, model, mstep = mstep.mvnorm, maxit= 300)
summary(hmm)

#--------------------------------------------- Validate MVHMM ---------------------------------------------#

# format the validation data
validation_data <- formatHMM(data.frame(validation_data))

corrupt <- rbinom(length(validation_data$x[,1]), 1, 0.1)
corrupt <- as.logical(corrupt)
noise <- runif(sum(corrupt), 0.0, 7.0)

validation_data$x[corrupt,1] <- noise

# predict with validation data
validation_pred <- predict.hmm(hmm, validation_data)

# detect point anomalies
threshold <- 2
validation_point_anomalies <- detectPointAnomaliesMVhmm(validation_pred, hmm$model$parms.emission, threshold)

# detect collective anomalies
threshold <- 1
window_size <- 20
validation_collective_anomalies <- detectCollectiveAnomaliesMVhmm(validation_pred, hmm$model$parms.emission, window_size, threshold)


#--------------------------------------------- Test MVHMM ---------------------------------------------#

# format the test data
test_data <- formatHMM(data.frame(test_data))

# predict with test data
test_pred <- predict.hmm(hmm, test_data)

# detect point anomalies
threshold <- 2
test_point_anomalies <- detectPointAnomaliesMVhmm(test_pred, hmm$model$parms.emission, threshold)

# detect collective anomalies
threshold <- 1
window_size <- 20
test_collective_anomalies <- detectCollectiveAnomaliesMVhmm(test_pred, hmm$model$parms.emission, window_size, threshold)


detectPointAnomaliesUVhmm <- function(prediction, params, threshold) {
  min <- min(prediction$x[,1])
  max <- max(prediction$x[,1])
  N <- prediction$N
  anomaly_results <- matrix(ncol = 2, nrow = N)
  anomalies <- 0
  for(i in 1:N) {
    obs_state <- prediction$s[i]
    obs_value <- prediction$x[i,1]
    state_mean <- params$mu[obs_state]
    state_std <- params$sigma[obs_state]
    probability <- 0
    anomaly <- 0
    if(abs(obs_value - state_mean) > threshold) {
      anomaly <- 1
      probability <- 1
      anomalies <- anomalies + 1
    }
    anomaly_results[i,] <- c(anomaly, probability)
  }
  return(list(data = anomaly_results, anomalies = anomalies))
}

normalize <- function(value, min, max) {
  num <- value - min
  den <- max - min
  result <- num/den
  return(result)
}
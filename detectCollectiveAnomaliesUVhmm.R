detectCollectiveAnomaliesUVhmm <- function(prediction, params, window_size, threshold) {
  N <- prediction$N
  size <- ceiling(N/window_size)
  window_threshold <- floor(window_size/2)
  anomaly_results <- matrix(ncol = 2, nrow = size)
  total_anomalies <- 0
  
  for(i in 1:size) {
    window_anomalies <- 0
    k <- (i-1)*window_size + 1
    for(j in k:(k + window_size - 1)) {
      if(j > N) {
        break
      } else {
        obs_state <- prediction$s[j]
        obs_value <- prediction$x[j,1]
        state_mean <- params$mu[obs_state]
        state_std <- params$sigma[obs_state]
        if(abs(obs_value - state_mean) > threshold) {
          window_anomalies <- window_anomalies + 1
        }
      }
    }
    anomaly <- 0
    if(window_anomalies > window_threshold) {
      total_anomalies <- total_anomalies + 1
      anomaly <- 1
    }
    probability <- window_anomalies / window_size
    anomaly_results[i,] <- c(anomaly, probability)
  }
  
  return(list(data = anomaly_results, anomalies = total_anomalies))
}


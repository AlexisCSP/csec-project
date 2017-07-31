AddCollectiveNoise <- function(data, window_size) {
  N <- data$N
  size <- ceiling(N/window_size)

  anomalies_added <- 0
  for(i in 1:size) {
    k <- (i-1)*window_size + 1
    condition <- runif(1, 0.0, 10.0)
    if(floor(condition) > 5) {
      anomalies_added <- anomalies_added + 1
      for(j in k:(k + window_size - 1)) {
        if(j > N) {
          break
        } else {
          data$x[j] <- runif(1, 0.0, 7.0)
        }
      }
    }
  }
  print(anomalies_added)
  return(data)
}


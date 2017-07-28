detectAnomaliesMVHMM <- function(prediction, params) {
  anomalies <- 0
  anomaly_results = df <- data.frame(Global_active_power=double(),
                                     Ints=integer(),
                                     Factors=factor(),
                                     Logicals=logical(),
                                     Characters=character(),
                                     stringsAsFactors=FALSE)
  for(i in 1:prediction$N) {
    obs_state = prediction$s[i]
    obs_value = prediction$x[i]
    state_mean = params$mu[[obs_state]][test_state]
    state_std = params$sigma[test_state]
    #test_obs_prob = pnorm(test_obs, mean=state_mean, sd=state_std)
    anomaly_results[i,1] <- 0
    anomaly_results[i,2] <- 0.05
    if(abs(test_obs - state_mean) > 1) {
      anomaly_results[i,1] <- 1
      anomaly_results[i,2] <- 0.95
      anomalies <- anomalies + 1
    }
  }
}
formatMVhmm <- function(data) {
  mod_data <- list(x = data.frame(data), N = nrow(data))
  mod_data$x <- data.matrix(mod_data$x, rownames.force = NA)
  class(mod_data) <- "hsmm.data"
  return(mod_data)
}
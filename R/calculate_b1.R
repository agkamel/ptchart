calculate_b1 <- function(day, log10_freq, antilog = FALSE) {
  mean_day <- mean(day, na.rm = TRUE)
  mean_log10_freq <- mean(log10_freq, na.rm = TRUE)

  day_deviation_from_mean <- day - mean_day
  day_deviation_from_mean_squared <- day_deviation_from_mean^2

  log10_freq_deviation_from_mean <- log10_freq - mean_log10_freq

  if (antilog == FALSE) {
    sum(day_deviation_from_mean * log10_freq_deviation_from_mean) / sum(day_deviation_from_mean_squared)
  } else {
    antilog(sum(day_deviation_from_mean * log10_freq_deviation_from_mean) / sum(day_deviation_from_mean_squared))
  }



}

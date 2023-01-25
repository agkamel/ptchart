calculate_b0 <- function(day, log10_freq, antilog = FALSE) {
  stopifnot("`day` must be integer" = is.integer(day))
  stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))

  mean_day <- mean(day, na.rm = TRUE)
  mean_log10_freq <- mean(log10_freq, na.rm = TRUE)
  b1 <- calculate_b1(day, log10_freq)

  if (antilog == FALSE) {
    mean_log10_freq - (b1 * mean_day)
  } else {
    antilog(mean_log10_freq - (b1 * mean_day))
  }
}

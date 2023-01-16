calculate_predicted_values <- function(day, log10_freq, antilog = FALSE){
  b0 <- calculate_b0(day, log10_freq)
  b1 <- calculate_b1(day, log10_freq)

  if (antilog == FALSE) {
    b0 + b1 * day
  } else {
    antilog(b0 + b1 * day)
  }
}

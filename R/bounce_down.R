bounce_down <- function(day, log10_freq, raw = TRUE) {
  errors <- calculate_errors(day, log10_freq)
  min_error <- antilog(min(errors))

  if (raw == TRUE) {
    min_error
  } else {
    convert_value(min_error)
  }

}

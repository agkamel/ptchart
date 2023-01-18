bounce_up <- function(day, log10_freq, raw = TRUE) {
  errors <- calculate_errors(day, log10_freq)
  max_error <- antilog(max(errors))

  if (raw == TRUE) {
    max_error
  } else {
    convert_value(max_error)
  }

}


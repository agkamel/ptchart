calculate_accuracy_ratio <- function(freq, freq_err, raw = TRUE) {

  ar_raw <- freq / freq_err
  ar_val <- convert_value(ar_raw)

  if (raw == TRUE) {
    return(ar_raw)
  } else {
    return(ar_val)
  }
}

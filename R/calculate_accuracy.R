calculate_accuracy <- function(day, log10_acc_ratio_raw, raw = TRUE) {

  b1 <- calculate_b1(day, log10_acc_ratio_raw)
  a_raw <- antilog(b1)^7
  a_val <- convert_value(a_raw)

  if (raw == TRUE) {
    return(a_raw)
  } else {
    return(a_val)
  }
}

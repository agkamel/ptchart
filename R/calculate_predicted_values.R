calculate_predicted_values <- function(day, log10_freq, antilog = FALSE){
  stopifnot("`day` must be integer" = is.integer(day))
  stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))
  stopifnot("`antilog` must be a logical `TRUE` or `FALSE`" = is.logical(antilog) & antilog %in% c(TRUE, FALSE))

  b0 <- calculate_b0(day, log10_freq)
  b1 <- calculate_b1(day, log10_freq)

  if (antilog == FALSE) {
    b0 + b1 * day
  } else {
    antilog(b0 + b1 * day)
  }
}

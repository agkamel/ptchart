calculate_errors <- function(day, log10_freq, antilog = FALSE, squared = FALSE){
  # stopifnot("`day` must be integer" = is.integer(day))
  # stopifnot("`log_10_freq` must be numeric" = is.numeric(log10_freq))
  stopifnot("`antilog` must be a logical `TRUE` or `FALSE`" = is.logical(antilog) & antilog %in% c(TRUE, FALSE))

  predicted_values <- calculate_predicted_values(day, log10_freq)

  if (antilog == FALSE){
    errors <- log10_freq - predicted_values
  } else {
    errors <- antilog(log10_freq - predicted_values)
  }

  if (squared == FALSE){
    errors
  } else {
    errors^2
  }
}

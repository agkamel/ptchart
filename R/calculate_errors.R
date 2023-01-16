calculate_errors <- function(day, log10_freq, antilog = FALSE, squared = FALSE){
  predicted_values <- calculate_predicted_values(day, log10_freq)

  if (antilog == FALSE){
    errors <- log10_freq - predicted_values
  } else {
    errors <- antilog10(log10_freq - predicted_values)
  }

  if (squared == FALSE){
    errors
  } else {
    errors^2
  }
}

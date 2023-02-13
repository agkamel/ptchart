calculate_count_ceil <- function(count_ceil) {
  if (!is.numeric(count_ceil)) {
    stop("`count_ceil` must be numeric.")
  }

  for (i in count_ceil) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("`count_ceil` must be greater that 0.")
    }
  }

  1 / count_ceil
}

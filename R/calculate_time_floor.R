calculate_time_floor <- function(time) {
  if (!is.numeric(time)) {
    stop("`time` must be numeric.")
  }

  for (i in time) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("`time` must be greater that 0.")
    }
  }

  1 / time
}

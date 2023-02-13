calculate_count_floor <- function(count_floor) {
  if (!is.numeric(count_floor)) {
    stop("`count_floor` must be numeric.")
  }

  for (i in count_floor) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("`count_floor` must be greater that 0.")
    }
  }

  1 / count_floor
}

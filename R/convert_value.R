convert_value <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  for (i in x) {
    if (is.na(i)) {
      next
    } else if (i <= 0) {
      stop("Values in `x` must be greater than 0.")
    }
  }

  antilog(abs(log10(x)))
}

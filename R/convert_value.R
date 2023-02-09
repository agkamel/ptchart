convert_value <- function(x) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }
  if (x <= 0) {
    stop("`x` must be greater than 0")
  }

  antilog(abs(log10(x)))
}

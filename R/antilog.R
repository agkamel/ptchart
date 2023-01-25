#' Compute antilogarithm
#'
#' @title Antilogarithm
#' @description
#' `antilog()` returns the antilogarithm of a value.
#'
#' @param x Numeric. Value to which base is augmented.
#' @param base Numeric. Value to be augmented by x.
#'
#' @return Numeric.
#' @export
#'
#' @examples antilog(0.25)
antilog <- function(x, base = 10) {
  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot("base must be numeric" = is.numeric(base))

  if (is.nan(base^x)) {
    stop("`antilog` return a 'not a number' (NaN)")
  } else if (is.infinite(base^x)) {
    stop("`antilog` return an infinite number (Inf)")
  } else {
    return(base^x)
  }

}

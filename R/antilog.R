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

  if (sum(is.nan(base^x)) >= 1) {
    stop("`antilog` returns some 'not a number' values (NaN)")
  } else if (sum(is.infinite(base^x)) >= 1) {
    stop("`antilog` returns some infinite (Inf) values")
  } else {
    return(base^x)
  }

}

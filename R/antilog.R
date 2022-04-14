#' Compute antilogarithm
#'
#' `antilog()` returns the antilog of a value.
#'
#' @param x Numeric. Value to which base is augmented.
#' @param base Numeric. Value to augment by x.
#'
#' @return Numeric.
#' @export
#'
#' @examples antilog(0.25)
antilog <- function(x, base = 10) {
  base^x
}

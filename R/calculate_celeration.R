#' @title
#' Celeration
#'
#' @description
#' Calculate celeration.
#'
#' @param day Integer.
#' @param log10_freq Double.
#' @param raw Boolean.
#'
#' @return Double.
#' @export
#'
#' @examples
#' #TODO
celeration <- function(day, log10_freq, raw = TRUE) {
  b1 <- calculate_b1(day, log10_freq)
  c_raw <- antilog(b1)^7
  c_val <- convert_value(c_raw)

  if (raw == TRUE) {
    return(c_raw)
  } else {
    return(c_val)
  }
}

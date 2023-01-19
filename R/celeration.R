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

  if (c_raw == c_val) {
    direction <- paste0("\327", round(c_val, 2))
  } else {
    direction <- paste0("\367", round(c_val, 2))
  }

  if (raw == TRUE) {
    return(c_raw)
  } else {
    return(direction)
  }
}
